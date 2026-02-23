(** Source location tracking - re-export from Location module *)
module Loc = Location

type const = 
  | CUnit
  | CNever
  | CInt of int
  | CString of string
  | CBool of bool

type unary_op =
  | Fst | Snd (* tuple elimination *)
  | UWait | UWatch | UTail | UDelay
  | UProj of int (* proj_i for internal use *)
  (* | Inl | Inr *) (* constructors for sum type *)

type binary_op =
  | SigCons
  | BSync | BOStar | BLaterApp
  | Add | Mul | Sub | Div | Eq | Lt | Leq

(** Source location for an expression node *)
type location = Location.t

type typ = 
  | TVar of string
  | TFun of typ list * typ
  | TSignal of typ
  | TTuple of typ * typ
  (* could include type variables, type constructors, etc. *)

type parsed (* just parsed *)
type typed (* typechecking *)

type _ ann =
  | Ann_parsed : Location.t -> parsed ann
  | Ann_typed : Location.t * typ -> typed ann

type _ pattern =
  | PWildcard
  | PVar : string * 's ann -> 's pattern 
  | PConst : const * 's ann -> 's pattern
  | PTuple : 's pattern * 's pattern * 's ann -> 's pattern
  | PSigCons : 's pattern * 's name * 's ann -> 's pattern
  | PCtor : 's name * 's pattern list * 's ann-> 's pattern
  (* could include more complex patterns like lists, records, etc. *)

and _ expr =
  | EConst : const * 's ann -> 's expr
  | EVar : 's name -> 's expr
  | ELet : 's name * 's expr * 's expr * 's ann -> 's expr   (* variable binding *)
  | EFun : 's name list * 's expr * 's ann -> 's expr        (* Rizzo has functions with 1 parameter *)
  | EApp : 's expr * 's expr list * 's ann -> 's expr        (* Rizzo has applications with 1 arg *)
  | EUnary : unary_op * 's expr  * 's ann -> 's expr
  | EBinary : binary_op * 's expr * 's expr  * 's ann -> 's expr
  | ETuple : 's expr * 's expr  * 's ann -> 's expr
  | ECase : 's expr * 's case_branch list  * 's ann -> 's expr
  | EIfe : 's expr * 's expr * 's expr  * 's ann -> 's expr      (* if-then-else *) 
  (* | EPartialApp : 's expr * 's expr list  * 's ann -> 's expr *)
and 's case_branch = 's pattern * 's expr  * 's ann
and 's name = string * 's ann

type _ top_expr =
  | TLet : string * 's expr  * 's ann -> 's top_expr
  (* could include types, modules idk *)

type 'stage program  = 'stage top_expr list

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
  | Ann_typed (loc, _) -> loc

let expr_get_ann (e : _ expr) : _ ann =
  match e with
  | EConst (_, ann) | EVar (_, ann) | ELet (_, _, _, ann) | EFun (_, _, ann)
  | EApp (_, _, ann) | EUnary (_, _, ann) | EBinary (_, _, _, ann)
  | ETuple (_, _, ann) | ECase (_, _, ann) | EIfe (_, _, _, ann) -> ann

let rec pattern_bound_vars = function
  | PWildcard | PConst _ -> []
  | PVar (x, _) -> [x]
  | PSigCons (p1, p2, _) -> pattern_bound_vars p1 @ [fst p2]
  | PTuple (p1, p2, _) -> pattern_bound_vars p1 @ pattern_bound_vars p2
  | PCtor (_, ps, _) -> List.concat_map pattern_bound_vars ps

let rec eq_expr a b = 
  match a, b with
  | EConst (c1, _), EConst (c2,_) -> c1 = c2
  | EVar (x1, _), EVar (x2, _) -> x1 = x2
  | ELet (x1, e1_1, e1_2, _), ELet (x2, e2_1, e2_2, _) -> eq_name x1 x2 && eq_expr e1_1 e2_1 && eq_expr e1_2 e2_2
  | EFun (params1, body1, _), EFun (params2, body2, _) -> List.for_all2 eq_name params1 params2 && eq_expr body1 body2
  | EApp (f1, args1, _), EApp (f2, args2, _) -> eq_expr f1 f2 && List.for_all2 eq_expr args1 args2
  | EUnary (op1, e1, _), EUnary (op2, e2, _) -> op1 = op2 && eq_expr e1 e2
  | EBinary (op1, e11, e12, _), EBinary (op2, e21, e22, _) -> op1 = op2 && eq_expr e11 e21 && eq_expr e12 e22
  | ETuple (e11, e12, _), ETuple (e21, e22, _) -> eq_expr e11 e21 && eq_expr e12 e22
  | ECase (e1, cases1, _), ECase (e2, cases2, _) ->
    eq_expr e1 e2
    && List.length cases1 = List.length cases2
    && List.for_all2 (fun (p1, b1, _) (p2, b2, _) -> eq_pattern p1 p2 && eq_expr b1 b2) cases1 cases2
  | EIfe (e1_1, e1_2, e1_3, _), EIfe (e2_1, e2_2, e2_3, _) ->
    eq_expr e1_1 e2_1 && eq_expr e1_2 e2_2 && eq_expr e1_3 e2_3
  | _ -> false
and eq_name (a: _ name) (b: _ name) = fst a = fst b

and eq_pattern a b =
  match a, b with
  | PWildcard, PWildcard -> true
  | PVar (x1, _), PVar (x2, _) -> x1 = x2
  | PConst (c1, _), PConst (c2, _) -> c1 = c2
  | PTuple (a1, b1, _), PTuple (a2, b2, _) -> eq_pattern a1 a2 && eq_pattern b1 b2
  | PSigCons (a1, b1, _), PSigCons (a2, b2, _) -> eq_pattern a1 a2 && eq_name b1 b2
  | PCtor (name1, args1, _), PCtor (name2, args2, _) ->
    eq_name name1 name2 && List.length args1 = List.length args2 && List.for_all2 eq_pattern args1 args2
  | _ -> false

let pp_const out = function
  | CUnit -> Format.fprintf out "()"
  | CNever -> Format.fprintf out "never"
  | CInt i -> Format.fprintf out "%d" i
  | CString s -> Format.fprintf out "%S" s
  | CBool b -> Format.fprintf out "%b" b

let rec pp_pattern out = function
  | PWildcard -> Format.fprintf out "_"
  | PVar (x, _) -> Format.fprintf out "%s" x
  | PConst (c, _) -> pp_const out c
  | PTuple (p1, p2, _) -> Format.fprintf out "(%a, %a)" pp_pattern p1 pp_pattern p2
  | PSigCons (p1, p2, _) -> Format.fprintf out "(%a :: %s)" pp_pattern p1 (fst p2)
  | PCtor (name, args, _) -> 
    if List.length args = 0 then Format.fprintf out "%s (notice args are empty!!)" (fst name)
    else Format.fprintf out "%s(%a)" (fst name) (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out ", ") pp_pattern) args

let rec pp_case_branch out (p, b, _ : _ case_branch) =
  let open Format in
  fprintf out "| %a ->@ %a" pp_pattern p pp_expr b

and pp_expr out = 
  let open Format in
  function
  | EConst (c,_) -> pp_const out c
  | EVar (x, _) -> fprintf out "%s" x
  | ELet ((x, _), e1, e2, _) ->
    fprintf out "@[<hov 2>let %s =@ %a@ in@ %a@]" x pp_expr e1 pp_expr e2
  | EFun (names, body, _) ->
    let params = List.map fst names in
    fprintf out "@[<hov 2>fun (%s) ->@ %a@]" (String.concat ", " params) pp_expr body
  | EApp (f, args, _) ->
    fprintf out "@[<hov 2>%a(@[<hov>%a@])@]"
      pp_expr f
      (pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ") pp_expr) args
  | EUnary (op, e, _) -> 
    let op_str = match op with 
    | Fst -> "fst" 
    | Snd -> "snd"
    | UWait -> "wait"
    | UWatch -> "watch"
    | UTail -> "tail"
    | UDelay -> "delay"
    | UProj i -> Printf.sprintf "proj_%d" i
    in
    fprintf out "@[<hov 2>%s@ %a@]" op_str pp_expr e
  | EBinary (op, e1, e2, _) ->
    let op_str = match op with 
    | SigCons -> "::" 
    | BSync -> "sync"
    | BOStar -> "(*)"
    | BLaterApp -> "(>)"
    | Add -> "+"
    | Mul -> "*"
    | Sub -> "-"
    | Div -> "/"
    | Eq -> "=="
    | Lt -> "<"
    | Leq -> "<="
    in
    fprintf out "@[<hov 2>(%a@ %s@ %a)@]" pp_expr e1 op_str pp_expr e2
  | ETuple (e1, e2, _) -> fprintf out "@[<hov>(%a,@ %a)@]" pp_expr e1 pp_expr e2
  | EIfe (e1, e2, e3, _) ->
    fprintf out "@[<v 2>(if %a@ then@ %a@ else@ %a)@]" pp_expr e1 pp_expr e2 pp_expr e3
  | ECase (e, cases, _) ->
    fprintf out "@[<v 0>(match %a with@,%a)@]"
      pp_expr e
      (pp_print_list ~pp_sep:(fun out () -> fprintf out "@,") pp_case_branch)
      cases

let eq_top_expr a b = match a,b with
  | TLet (x1, e1, _), TLet (x2, e2, _) -> x1 = x2 && eq_expr e1 e2 

let pp_top_expr out =
  function
  | TLet (x, e, _) -> Format.fprintf out "@[<hov 2>let %s =@ %a@]" x pp_expr e

let eq_program a b = 
  List.length a = List.length b && List.for_all2 eq_top_expr a b

let pp_program out (p: _ program) =
  let open Format in
  fprintf out "@[<v>%a@]"
    (pp_print_list ~pp_sep:(fun out () -> fprintf out "@.@.") pp_top_expr)
    p
