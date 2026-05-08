(** Source location tracking - re-export from Location module *)
module Loc = Location

type const =
  | CUnit
  | CNever
  | CInt of int
  | CString of string
  | CBool of bool

type unary_op =
  | UWait | UWatch | UTail | UDelay | UNot
  | UProj of int

type binary_op =
  | SigCons
  | BSync | BOStar | BLaterApp
  | Add | Mul | Sub | Div | Mod | Eq | Lt | Leq | Gt | Geq

type location = Location.t

type 'a list1 = Cons1 of 'a * 'a list

type typ =
  | TError
  | TUnit
  | TInt
  | TString
  | TBool
  | TName of string
  | TApp of typ * typ list
  | TParam of string
  | TVar of int
  | TFun of typ list1 * typ
  | TTuple of typ * typ
  | TSignal of typ
  | TLater of typ
  | TDelay of typ
  | TChan of typ

type parsed
type bound
type typed

type scope =
  | Scope_global
  | Scope_local of { level: int }

type _ ann =
  | Ann_parsed : Location.t -> parsed ann
  | Ann_bound : Location.t * scope -> bound ann
  | Ann_typed : Location.t * typ -> typed ann

type _ pattern =
  | PWildcard : 's ann -> 's pattern
  | PVar : string * 's ann -> 's pattern
  | PConst : const * 's ann -> 's pattern
  | PError : string * 's ann -> 's pattern
  | PTuple : 's pattern * 's pattern * 's ann -> 's pattern
  | PSigCons : 's pattern * 's name * 's ann -> 's pattern
  | PStringCons : 's pattern * 's name * 's ann -> 's pattern
  | PCtor : 's name * 's pattern list * 's ann -> 's pattern

and _ expr =
  | EConst : const * 's ann -> 's expr
  | EError : string * 's ann -> 's expr
  | EVar : 's name -> 's expr
  | ECtor : 's name * 's expr list * 's ann -> 's expr
  | ELet : 's name * 's expr * 's expr * 's ann -> 's expr
  | EFun : 's pattern list * 's expr * 's ann -> 's expr
  | EApp : 's expr * 's expr list * 's ann -> 's expr
  | EUnary : unary_op * 's expr * 's ann -> 's expr
  | EBinary : binary_op * 's expr * 's expr * 's ann -> 's expr
  | ETuple : 's expr * 's expr * 's ann -> 's expr
  | ECase : 's expr * 's case_branch list * 's ann -> 's expr
  | EIfe : 's expr * 's expr * 's expr * 's ann -> 's expr
  | EAnno : 's expr * typ * 's ann -> 's expr

and 's case_branch = 's pattern * 's expr * 's ann
and 's name = string * 's ann
and 's ctor_def = 's name * typ list * 's ann

type _ top_expr =
  | TopLet : 's name * 's expr * 's ann -> 's top_expr
  | TopTypeDef : 's name * 's name list * 's ctor_def list * 's ann -> 's top_expr

type 'stage program = 'stage top_expr list

module ExprLocTable = Hashtbl.Make(struct
  type t = parsed expr
  let equal a b = a == b
  let hash v = Hashtbl.hash (Obj.magic v : int)
end)

module TopExprLocTable = Hashtbl.Make(struct
  type t = parsed top_expr
  let equal a b = a == b
  let hash v = Hashtbl.hash (Obj.magic v : int)
end)

let get_location : type stage. stage ann -> Location.t = fun a ->
  match a with
  | Ann_parsed loc -> loc
  | Ann_bound (loc, _) -> loc
  | Ann_typed (loc, _) -> loc

let ann_get_type (Ann_typed (_, t)) = t

let expr_get_ann : type stage. stage expr -> stage ann = fun e ->
  match e with
  | EConst (_, ann) | EVar (_, ann) | ECtor (_, _, ann) | ELet (_, _, _, ann) | EFun (_, _, ann)
  | EApp (_, _, ann) | EUnary (_, _, ann) | EBinary (_, _, _, ann)
  | ETuple (_, _, ann) | ECase (_, _, ann) | EIfe (_, _, _, ann)
  | EAnno (_, _, ann) | EError (_, ann) -> ann

let rec pattern_bound_vars_with_anns : type stage. stage pattern -> (string * stage ann) list = function
  | PWildcard _ | PConst _ | PError _ -> []
  | PVar (x, ann) -> [x, ann]
  | PSigCons (p1, p2, _) | PStringCons (p1, p2, _) -> pattern_bound_vars_with_anns p1 @ [p2]
  | PTuple (p1, p2, _) -> pattern_bound_vars_with_anns p1 @ pattern_bound_vars_with_anns p2
  | PCtor (_, ps, _) -> List.concat_map pattern_bound_vars_with_anns ps

let pattern_bound_vars pattern =
  List.map fst (pattern_bound_vars_with_anns pattern)

let pattern_get_ann = function
  | PWildcard ann | PConst (_, ann)
  | PError (_, ann)
  | PVar (_, ann) | PSigCons (_,_ , ann) | PStringCons (_,_, ann) 
  | PTuple (_, _, ann)
  | PCtor (_, _, ann) -> ann

let string_of_binary_op = function
  | SigCons -> "::"
  | BSync -> "sync"
  | BOStar -> "(*)"
  | BLaterApp -> "|>"
  | Add -> "+"
  | Mul -> "*"
  | Sub -> "-"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Lt -> "<"
  | Leq -> "<="
  | Gt -> ">"
  | Geq -> ">="

let string_of_unary_op = function
  | UWait -> "wait"
  | UWatch -> "watch"
  | UTail -> "tail"
  | UDelay -> "delay"
  | UNot -> "not"
  | UProj i -> Printf.sprintf "proj_%d" i
