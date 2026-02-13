(* after all other transformations *)
open Ast
let get_name = function
  | EVar x -> x
  | _ -> failwith "Assumption broken - expected all application arguments to be names"

let ctor_never_i = 0
let ctor_wait_i = 1


module M = Map.Make(String)
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
  | EBinary (SigCons, EVar n1, EVar n2) -> RSignal { head = n1; tail = n2 }
  | ETuple (EVar n1, EVar n2) -> RCtor (idof "tuple", [n1; n2])
  | EBinary (BSync, EVar n1, EVar n2) -> RCtor (idof "sync", [n1; n2])
  | EBinary (BLaterApp, EVar n1, EVar n2) -> RCtor (idof "later_app", [n1; n2])
  | EBinary (BOStar, EVar n1, EVar n2) -> RCtor (idof "ostar", [n1; n2])
  | EBinary (op, EVar n1, EVar n2) -> RCtor (idof (op_to_application op), [n1; n2])
  | EUnary (UWait, EVar n) -> RCtor (idof "wait", [n])
  | EUnary (UTail, EVar n) -> RCtor (idof "tail", [n])
  | EUnary (UWatch, EVar n) -> RCtor (idof "watch", [n])
  | EUnary (UDelay, EVar n) -> RCtor (idof "delay", [n])
  | EUnary (Fst, EVar x)  -> RProj (0, x)
  | EUnary (Snd, EVar x)  -> RProj (1, x)
  | EConst (const) -> (
      match const with
      | CUnit -> RCtor (idof "unit", [])
      | CNever -> RCtor (idof "never", [])
      | CInt n -> RCtor (idof "int", [string_of_int n])
      | CString s -> 
        RCtor (idof "string", ["\"" ^ s ^ "\""])
      | CBool b -> if b then RCtor (idof "true", []) else RCtor (idof "false", [])
    )
  | _ -> failwith (Format.asprintf "expr_to_rexpr failed: invalid expression '%a'" Ast.pp_expr e)

and expr_to_fn_body (e: Ast.expr) : Refcount.fn_body = 
  match e with
  | EVar x -> FnRet x
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
    FnLet (x, expr_to_rexpr e, FnRet x)

let to_rc_intermediate_representation (p: Ast.program) : Refcount.program =
  let to_fun = function
  | EFun (params, body) -> Refcount.Fun (params, expr_to_fn_body body) 
  | _ -> failwith "TODO: only top level functions"
  in
  List.map (fun (TLet (name, rhs)) -> name, to_fun rhs) p
