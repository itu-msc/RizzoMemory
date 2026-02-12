(* after all other transformations *)
open Ast
let get_name = function
  | EVar x -> x
  | _ -> failwith "Assumption broken - expected all application arguments to be names"

let rec expr_to_rexpr (e: Ast.expr) : Refcount.rexpr = 
  match e with
  (* assumption: f (EVar x1) (EVar x2) (EVar x3) ... *)
  | EApp (EVar f, xs) -> RCall (f, List.map get_name xs) (* TODO: partial apps *)
  | EBinary (SigCons, EVar n1, EVar n2) -> RSignal { head = n1; tail = n2 }
  | ETuple (EVar n1, EVar n2) -> RCtor (2, [n1; n2])
  | EUnary (Fst, EVar x)  -> RProj (0, x)
  | EUnary (Snd, EVar x)  -> RProj (1, x)
  | EConst (const) -> (
      match const with
      | CUnit -> RCtor (0, [])
      | CNever -> RCtor (0, [])
      | CInt n -> RCtor (1, [string_of_int n])
      | CBool b -> RCtor (1, [string_of_bool b])
    )
  | _ -> failwith (Format.asprintf "expr_to_rexpr failed: invalid expression '%a'" Ast.pp_expr e)

and expr_to_fn_body (e: Ast.expr) : Refcount.fn_body = 
  match e with
  | EVar x -> FnRet x
  | ELet (x, rhs, e') ->
    FnLet(x, expr_to_rexpr rhs, expr_to_fn_body e')
  | ECase (EVar x, cases) -> 
    FnCase(x, List.map expr_to_fn_body cases)
  | ECase (e,cases) ->
    (* SHOULD NEVER HAPPEN but do 'case e of cases -> let x = e in case x of cases' *)
    let x = Utilities.new_var () in 
    FnLet (x, expr_to_rexpr e, FnCase (x, List.map expr_to_fn_body cases))
  | _ -> 
    (* TODO: should ANF not handle this? *)
    let x = Utilities.new_var () in 
    FnLet (x, expr_to_rexpr e, FnRet x)

let to_rc_intermediate_representation (p: Ast.program) : Refcount.program =
  let to_fun = function
  | EFun (params, body) -> Refcount.Fun (params, expr_to_fn_body body) 
  | _ -> failwith "TODO: only top level functions"
  in
  List.map (fun (TLet (name, rhs)) -> name, to_fun rhs) p
