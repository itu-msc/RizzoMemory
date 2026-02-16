(* after all other transformations *)
open Ast
let get_name = 
  function
  | EVar x -> Refcount.Var x
  | EConst c -> Refcount.Const c
  | _ -> failwith "get_name failed: expected variable or constant"

let ctor_never_i = 0
let ctor_wait_i = 1


module M = Map.Make(String)
(* consider if we can do this in a way that 
  makes the compiler complain if we dont have the entries*)
let ctorMappings = 
  M.of_list [
    (*Laters*)
    ("never", 0);
    ("wait", 1);
    ("tail", 2);
    ("sync", 4);
    ("watch", 5);
    ("later_app", 6); 

    (*delay*)
    ("delay", 0);
    ("ostar", 1);

    (*Tuples*)
    ("tuple", 0);

    (*Primitives*)
    ("unit", 0);
    ("int", 0);
    ("string", 0);
    ("false", 0);
    ("true", 1);
  ]
let idof name = M.find name ctorMappings

let op_to_application = function
  | Add -> "add"
  | Mul -> "mul"
  | Sub -> "sub"
  | Div -> "div"
  | Eq -> "eq"
  | Lt -> "lt"
  | Leq -> "leq"
  | _ -> failwith "unsupported operator"

let rec expr_to_rexpr (e: Ast.expr) : Refcount.rexpr = 
  match e with
  (* assumption: f (EVar x1) (EVar x2) (EVar x3) ... *)
  | EApp (EVar f, xs) -> RCall (f, List.map get_name xs) (* TODO: partial apps *)
  | EBinary (SigCons, n1, n2) -> RSignal { head = get_name n1; tail = get_name n2 }
  | ETuple (n1, n2) -> RCtor (idof "tuple", [get_name n1; get_name n2])
  | EBinary (BSync, n1, n2) -> RCtor (idof "sync", [get_name n1; get_name n2])
  | EBinary (BLaterApp, n1, n2) -> RCtor (idof "later_app", [get_name n1; get_name n2])
  | EBinary (BOStar, n1, n2) -> RCtor (idof "ostar", [get_name n1; get_name n2])
  | EBinary (op, n1, n2) -> RCall (op_to_application op, [get_name n1; get_name n2])
  | EUnary (UWait, n) -> RCtor (idof "wait", [get_name n])
  | EUnary (UTail, n) -> RCtor (idof "tail", [get_name n])
  | EUnary (UWatch, n) -> RCtor (idof "watch", [get_name n])
  | EUnary (UDelay, n) -> RCtor (idof "delay", [get_name n])
  | EUnary (Fst, EVar x)  -> RProj (0, x)
  | EUnary (Snd, EVar x)  -> RProj (1, x)
  | EConst (const) -> (
      match const with
      | CUnit -> RCtor (idof "unit", [])
      | CNever -> RCtor (idof "never", [])
      | CInt _ -> RCtor (idof "int", [Refcount.Const const])
      | CString _ -> RCtor (idof "string", [Refcount.Const const])
      | CBool b -> if b then RCtor (idof "true", []) else RCtor (idof "false", [])
    )
  | _ -> failwith (Format.asprintf "expr_to_rexpr failed: invalid expression '%a'" Ast.pp_expr e)

and expr_to_fn_body (e: Ast.expr) : Refcount.fn_body = 
  match e with
  | EVar x -> FnRet (Var x)
  | EConst c -> FnRet (Const c)
  | ELet (x, rhs, e') ->
    FnLet(x, expr_to_rexpr rhs, expr_to_fn_body e')
  | ECase (EVar x, cases) -> 
    FnCase(x, List.map (fun (_, branch) -> expr_to_fn_body branch) cases)
  | ECase _ ->
    (* SHOULD NEVER HAPPEN but do 'case e of cases -> let x = e in case x of cases' *)
    failwith "expr_to_fn_body failed: case scrutinee is not a variable"
    (* let x = Utilities.new_var () in 
    FnLet (x, expr_to_rexpr e, FnCase (x, List.map (fun (_, branch) -> expr_to_fn_body branch) cases)) *)
  | EIfe (EVar x, e1, e2) ->
    FnCase (x, [expr_to_fn_body e1; expr_to_fn_body e2])
  | EIfe _ -> failwith "expr_to_fn_body failed: if condition is not a variable"
  | _ -> 
    (* TODO: should ANF not handle this? *)
    let x = Utilities.new_var () in
    FnLet (x, expr_to_rexpr e, FnRet (Refcount.Var x))

let to_rc_intermediate_representation (p: Ast.program) : Refcount.program =
  let to_fun = function
  | EFun (params, body) -> Refcount.Fun (params, expr_to_fn_body body) 
  | _ -> failwith "TODO: only top level functions"
  in
  List.map (fun (TLet (name, rhs)) -> name, to_fun rhs) p
