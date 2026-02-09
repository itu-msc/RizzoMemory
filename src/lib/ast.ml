
type const = 
  | CUnit
  | CInt of int
  | CBool of bool

type unary_op =
  | Fst | Snd (* tuple elimination *)
  (* | Inl | Inr *) (* constructors for sum type *)

type binary_op =
  | SigCons
  (* | Add | Mul | Sub | Div | Eq | Lt | Leq *)

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

type top_expr =
  | TLet of string * expr
  (* could include types, modules idk *)

type program = top_expr list
