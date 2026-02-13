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
  (* | Inl | Inr *) (* constructors for sum type *)

type binary_op =
  | SigCons
  | BSync | BOStar | BLaterApp
  | Add | Mul | Sub | Div | Eq | Lt | Leq

(** Source location for an expression node *)
type location = Location.t

type pattern =
  | PWildcard
  | PVar of string
  | PConst of const
  | PTuple of pattern * pattern
  | PSigCons of pattern * pattern

type expr =
  | EConst of const
  | EVar of string
  | ELet of string * expr * expr    (* variable binding *)
  | EFun of string list * expr      (* Rizzo has functions with 1 parameter *)
  | EApp of expr * expr list        (* Rizzo has applications with 1 arg *)
  | EUnary of unary_op * expr
  | EBinary of binary_op * expr * expr
  | ETuple of expr * expr
  | ECase of expr * case_branch list
  | EIfe of expr * expr * expr      (* if-then-else *) 

and case_branch = pattern * expr

(* Note: Helper constructors with location tracking can be added when needed:
   let mk_const ?(loc=Location.dummy) c = EConst c
   let mk_var ?(loc=Location.dummy) x = EVar x
   etc.
*)

type top_expr =
  | TLet of string * expr
  (* could include types, modules idk *)

type program = top_expr list

let rec pattern_bound_vars = function
  | PWildcard | PConst _ -> []
  | PVar x -> [x]
  | PSigCons (p1, p2) | PTuple (p1, p2) -> pattern_bound_vars p1 @ pattern_bound_vars p2

let rec eq_expr a b = 
  match a, b with
  | EConst c1, EConst c2 -> c1 = c2
  | EVar x1, EVar x2 -> x1 = x2
  | ELet (x1, e1_1, e1_2), ELet (x2, e2_1, e2_2) -> x1 = x2 && eq_expr e1_1 e2_1 && eq_expr e1_2 e2_2
  | EFun (params1, body1), EFun (params2, body2) -> params1 = params2 && eq_expr body1 body2
  | EApp (f1, args1), EApp (f2, args2) -> eq_expr f1 f2 && List.for_all2 eq_expr args1 args2
  | EUnary (op1, e1), EUnary (op2, e2) -> op1 = op2 && eq_expr e1 e2
  | EBinary (op1, e11, e12), EBinary (op2, e21, e22) -> op1 = op2 && eq_expr e11 e21 && eq_expr e12 e22
  | ETuple (e11, e12), ETuple (e21, e22) -> eq_expr e11 e21 && eq_expr e12 e22
  | ECase (e1, cases1), ECase (e2, cases2) ->
    eq_expr e1 e2
    && List.length cases1 = List.length cases2
    && List.for_all2 (fun (p1, b1) (p2, b2) -> eq_pattern p1 p2 && eq_expr b1 b2) cases1 cases2
  | EIfe (e1_1, e1_2, e1_3), EIfe (e2_1, e2_2, e2_3) ->
    eq_expr e1_1 e2_1 && eq_expr e1_2 e2_2 && eq_expr e1_3 e2_3
  | _ -> false

and eq_pattern a b =
  match a, b with
  | PWildcard, PWildcard -> true
  | PVar x1, PVar x2 -> x1 = x2
  | PConst c1, PConst c2 -> c1 = c2
  | PTuple (a1, b1), PTuple (a2, b2) -> eq_pattern a1 a2 && eq_pattern b1 b2
  | PSigCons (a1, b1), PSigCons (a2, b2) -> eq_pattern a1 a2 && eq_pattern b1 b2
  | _ -> false

let pp_const out = function
  | CUnit -> Format.fprintf out "()"
  | CNever -> Format.fprintf out "never"
  | CInt i -> Format.fprintf out "%d" i
  | CString s -> Format.fprintf out "%S" s
  | CBool b -> Format.fprintf out "%b" b

let rec pp_pattern out = function
  | PWildcard -> Format.fprintf out "_"
  | PVar x -> Format.fprintf out "%s" x
  | PConst c -> pp_const out c
  | PTuple (p1, p2) -> Format.fprintf out "(%a, %a)" pp_pattern p1 pp_pattern p2
  | PSigCons (p1, p2) -> Format.fprintf out "(%a :: %a)" pp_pattern p1 pp_pattern p2

let rec pp_case_branch out (p, b) =
  let open Format in
  fprintf out "| %a ->@ %a" pp_pattern p pp_expr b

and pp_expr out = 
  let open Format in
  function
  | EConst c -> pp_const out c
  | EVar x -> fprintf out "%s" x
  | ELet (x, e1, e2) ->
    fprintf out "@[<hov 2>let %s =@ %a@ in@ %a@]" x pp_expr e1 pp_expr e2
  | EFun (params, body) ->
    fprintf out "@[<hov 2>fun (%s) ->@ %a@]" (String.concat ", " params) pp_expr body
  | EApp (f, args) ->
    fprintf out "@[<hov 2>%a(@[<hov>%a@])@]"
      pp_expr f
      (pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ") pp_expr) args
  | EUnary (op, e) -> 
    let op_str = match op with 
    | Fst -> "fst" 
    | Snd -> "snd"
    | UWait -> "wait"
    | UWatch -> "watch"
    | UTail -> "tail"
    | UDelay -> "delay"
    in
    fprintf out "@[<hov 2>%s@ %a@]" op_str pp_expr e
  | EBinary (op, e1, e2) ->
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
  | ETuple (e1, e2) -> fprintf out "@[<hov>(%a,@ %a)@]" pp_expr e1 pp_expr e2
  | EIfe (e1, e2, e3) ->
    fprintf out "@[<v 2>(if %a@ then@ %a@ else@ %a)@]" pp_expr e1 pp_expr e2 pp_expr e3
  | ECase (e, cases) ->
    fprintf out "@[<v 0>(match %a with@,%a)@]"
      pp_expr e
      (pp_print_list ~pp_sep:(fun out () -> fprintf out "@,") pp_case_branch)
      cases

let eq_top_expr a b = match a,b with
  | TLet (x1, e1), TLet (x2, e2) -> x1 = x2 && eq_expr e1 e2 

let pp_top_expr out =
  function
  | TLet (x, e) -> Format.fprintf out "@[<hov 2>let %s =@ %a@]" x pp_expr e

let eq_program a b = 
  List.length a = List.length b && List.for_all2 eq_top_expr a b

let pp_program out (p:program) =
  let open Format in
  fprintf out "@[<v>%a@]"
    (pp_print_list ~pp_sep:(fun out () -> fprintf out "@.@.") pp_top_expr)
    p
