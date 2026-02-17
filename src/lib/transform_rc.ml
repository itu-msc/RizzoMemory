(* after all other transformations *)
open Ast
let get_name = 
  function
  | EVar x -> Refcount.Var x
  | EConst c -> Refcount.Const c
  | _ -> failwith "get_name failed: expected variable or constant"

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
let idof name = match M.find_opt name ctorMappings with
  | Some id -> id
  | None -> failwith (Printf.sprintf "Constructor '%s' not found in mapping" name)

let op_to_application = function
  | Add -> "add"
  | Mul -> "mul"
  | Sub -> "sub"
  | Div -> "div"
  | Eq -> "eq"
  | Lt -> "lt"
  | Leq -> "leq"
  | _ -> failwith "unsupported operator"

(* strings are names of globals, option indicates arity with None for values *)
type globals_env = int option M.t

(* TODO: find a way around locals - possibly as part of a transformations somewhere
  that leaves variables as either 'Global of string' or 'Local of string' or w/e *)
module LocalsEnv = Set.Make(String)

let rec expr_to_rexpr (globals: globals_env) locals (e: Ast.expr): Refcount.rexpr = 
  match e with
  | EApp (EVar "fst", [EVar x])
  | EApp (EVar "head", [EVar x]) -> RProj (0, x) 
  | EApp (EVar "snd", [EVar x]) -> RProj (1, x) (*TODO: parser doesn't accept these*)
  (*TODO: move these into some other places*)
  | EApp (EVar "output_int_signal", [signal]) -> RCall ("output_int_signal", [get_name signal])
  | EApp (EVar "start_event_loop", [_]) -> RCall ("start_event_loop", [Refcount.Const (CUnit)])
  | EApp (EVar f, xs) when LocalsEnv.mem f locals ->
    if List.length xs = 1 then RVarApp(f, get_name (List.hd xs))
    else 
      failwith (Format.asprintf "todo! expr_to_rexpr variable application has more than one argument - not supported in refcount IR. For expression '%a'" Ast.pp_expr e)
  | EApp (EVar f, xs) -> 
    (match M.find_opt f globals with
    | Some Some arity when arity = List.length xs -> RCall (f, List.map get_name xs)
    | Some Some _ -> RPartialApp (f, List.map get_name xs) 
    | Some None -> failwith @@ Printf.sprintf "expr_to_rexpr failed: trying to apply a non-function global '%s'" f
    | None -> failwith @@ Printf.sprintf "expr_to_rexpr failed: unable to find '%s' in globals" f)
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

and expr_to_fn_body globals locals (e: Ast.expr) : Refcount.fn_body = 
  let expr_to_fn_body = expr_to_fn_body globals in
  let expr_to_rexpr = expr_to_rexpr globals in
  match e with
  | EVar x -> FnRet (Var x)
  | EConst c -> FnRet (Const c)
  | ELet (x, rhs, e') ->
    FnLet(x, expr_to_rexpr locals rhs, expr_to_fn_body (LocalsEnv.add x locals) e')
  | ECase (EVar x, cases) -> 
    FnCase(x, List.map (fun (_, branch) -> expr_to_fn_body locals branch) cases)
  | ECase _ -> 
    failwith "expr_to_fn_body failed: case scrutinee is not a variable"
  | EIfe (EVar x, e1, e2) ->
    FnCase (x, [expr_to_fn_body locals e1; expr_to_fn_body locals e2])
  | EIfe _ -> failwith "expr_to_fn_body failed: if condition is not a variable"
  | _ -> 
    (* TODO: should ANF not handle this? *)
    let x = Utilities.new_var () in
    FnLet (x, expr_to_rexpr locals e, FnRet (Refcount.Var x))

let to_rc_intermediate_representation (p: Ast.program) : Refcount.program =
  let globals: globals_env = 
    let mapper = function 
    | TLet(name, EFun (params, _)) -> (name, Some (List.length params))
    | TLet(name, _) -> (name, None)
    in
    List.map mapper p |> M.of_list
  in
  let to_fun = function
  | EFun (params, body) -> Refcount.Fun (params, expr_to_fn_body globals (LocalsEnv.of_list params) body) 
  | _ -> failwith "TODO: only top level functions"
  in
  List.map (fun (TLet (name, rhs)) -> name, to_fun rhs) p
