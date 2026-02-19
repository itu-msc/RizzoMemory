open Rizzoc
open Rizzoc.Ast

let dummy_pos : Lexing.position =
  {
    Lexing.pos_fname = "<test>";
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  }

let dummy_loc : Location.t = Location.mk dummy_pos dummy_pos

let ann : parsed ann = Ann_parsed dummy_loc

let name (s : string) : parsed name = (s, ann)

let pvar (s : string) : parsed pattern = PVar (s, ann)
let pwild : parsed pattern = PWildcard
let pconst (c : const) : parsed pattern = PConst (c, ann)
let psigcons (p1 : parsed pattern) (p2 : parsed pattern) : parsed pattern = PSigCons (p1, p2, ann)

let var (s : string) : parsed expr = EVar (name s)
let int (i : int) : parsed expr = EConst (CInt i, ann)
let bool (b : bool) : parsed expr = EConst (CBool b, ann)
let const (c : const) : parsed expr = EConst (c, ann)

let let_ (n : string) (e1 : parsed expr) (e2 : parsed expr) : parsed expr =
  ELet (name n, e1, e2, ann)

let fun_ (params : string list) (body : parsed expr) : parsed expr =
  EFun (List.map name params, body, ann)

let app (fn : parsed expr) (args : parsed expr list) : parsed expr = EApp (fn, args, ann)
let unary (op : unary_op) (e : parsed expr) : parsed expr = EUnary (op, e, ann)
let binary (op : binary_op) (e1 : parsed expr) (e2 : parsed expr) : parsed expr = EBinary (op, e1, e2, ann)
let tuple (e1 : parsed expr) (e2 : parsed expr) : parsed expr = ETuple (e1, e2, ann)
let ife (c : parsed expr) (t : parsed expr) (e : parsed expr) : parsed expr = EIfe (c, t, e, ann)

let case (scrutinee : parsed expr) (branches : (parsed pattern * parsed expr) list) : parsed expr =
  let annotated = List.map (fun (p, e) -> (p, e, ann)) branches in
  ECase (scrutinee, annotated, ann)

let tlet (n : string) (e : parsed expr) : parsed top_expr = TLet (n, e, ann)

let program_testable : parsed program Alcotest.testable =
  Alcotest.testable Ast.pp_program Ast.eq_program

let expr_testable : parsed expr Alcotest.testable =
  Alcotest.testable Ast.pp_expr Ast.eq_expr