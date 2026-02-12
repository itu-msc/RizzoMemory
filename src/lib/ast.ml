(** Source location tracking - re-export from Location module *)
module Loc = Location

type const = 
  | CUnit
  | CNever
  | CInt of int
  | CBool of bool

type unary_op =
  | Fst | Snd (* tuple elimination *)
  | UWait | UWatch | UTail | UDelay
  (* | Inl | Inr *) (* constructors for sum type *)

type binary_op =
  | SigCons
  | BSync | BOstar | BLaterApp
  (* | Add | Mul | Sub | Div | Eq | Lt | Leq *)

(** Source location for an expression node *)
type location = Location.t

type pattern =
  | PWildcard
  | PVar of string
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
  | ECase of expr * expr list       (* case based on index of constructor. For a list, Nil is 0, Cons is 1. *)

(* Note: Helper constructors with location tracking can be added when needed:
   let mk_const ?(loc=Location.dummy) c = EConst c
   let mk_var ?(loc=Location.dummy) x = EVar x
   etc.
*)

type top_expr =
  | TLet of string * expr
  (* could include types, modules idk *)

type program = top_expr list

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
  | ECase (e1, cases1), ECase (e2, cases2) -> eq_expr e1 e2 && List.for_all2 eq_expr cases1 cases2
  | _ -> false

let pp_const out = function
  | CUnit -> Format.fprintf out "()"
  | CNever -> Format.fprintf out "never"
  | CInt i -> Format.fprintf out "%d" i
  | CBool b -> Format.fprintf out "%b" b

let rec pp_expr out = 
  let open Format in
  function
  | EConst c -> pp_const out c
  | EVar x -> fprintf out "%s" x
  | ELet (x, e1, e2) -> fprintf out "let %s = %a in %a" x pp_expr e1 pp_expr e2
  | EFun (params, body) -> fprintf out "fun (%s) -> %a" (String.concat ", " params) pp_expr body
  | EApp (f, args) -> fprintf out "%a(%a)" pp_expr f (pp_print_list ~pp_sep:(fun out () -> fprintf out ", ") pp_expr) args
  | EUnary (op, e) -> 
    let op_str = match op with 
    | Fst -> "fst" 
    | Snd -> "snd"
    | UWait -> "wait"
    | UWatch -> "watch"
    | UTail -> "tail"
    | UDelay -> "delay"
    in
    fprintf out "%s %a" op_str pp_expr e
  | EBinary (op, e1, e2) ->
    let op_str = match op with 
    | SigCons -> "::" 
    | BSync -> "sync"
    | BOstar -> "(*)"
    | BLaterApp -> "(>)"
    in
    fprintf out "(%a %s %a)" pp_expr e1 op_str pp_expr e2
  | ETuple (e1, e2) -> fprintf out "(%a, %a)" pp_expr e1 pp_expr e2
  | ECase (e, cases) ->
    fprintf out "case %a of %a" pp_expr e (pp_print_list ~pp_sep:(fun out () -> fprintf out " | ") pp_expr) cases

let eq_top_expr a b = match a,b with
  | TLet (x1, e1), TLet (x2, e2) -> x1 = x2 && eq_expr e1 e2 

let pp_top_expr out =
  function
  | TLet (x, e) -> Format.fprintf out "let %s = %a" x pp_expr e

let eq_program a b = 
  List.length a = List.length b && List.for_all2 eq_top_expr a b

let pp_program out (p:program) =
  let open Format in
  fprintf out "%a" (pp_print_list ~pp_sep:(fun out () -> fprintf out "\n") pp_top_expr) p
