(* after all other transformations *)
open Ast
let get_name = 
  function
  | EVar (x, _) -> Refcount.Var x
  | EConst (c, _) -> Refcount.Const c
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

    (*sync*)
    ("left", 0);
    ("right", 1);
    ("both", 2);

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

let rec expr_to_rexpr (globals: globals_env) locals (e: _ expr): Refcount.rexpr = 
  match e with
  | EApp (EVar ("fst", _), [EVar (x, _)], _)
  | EApp (EVar ("head", _), [EVar (x, _)], _) -> RProj (0, x) 
  | EApp (EVar ("snd", _), [EVar (x, _)], _) -> RProj (1, x) (*TODO: parser doesn't accept these*)
  (*TODO: move these into some other places*)
  | EApp (EVar ("output_int_signal", _), [signal], _) -> RCall ("output_int_signal", [get_name signal])
  | EApp (EVar ("start_event_loop", _), [_], _) -> RCall ("start_event_loop", [Refcount.Const (CUnit)])
  | EApp (EVar (f, _), xs, _) when LocalsEnv.mem f locals ->
    if List.length xs = 1 then RVarApp(f, get_name (List.hd xs))
    else 
      failwith (Format.asprintf "todo! expr_to_rexpr variable application has more than one argument - not supported in refcount IR. For expression '%a'" Ast.pp_expr e)
  | EApp (EVar (f, _), xs, _) -> 
    (match M.find_opt f globals with
    | Some Some arity when arity = List.length xs -> RCall (f, List.map get_name xs)
    | Some Some _ -> RPartialApp (f, List.map get_name xs) 
    | Some None -> failwith @@ Printf.sprintf "expr_to_rexpr failed: trying to apply a non-function global '%s'" f
    | None -> failwith @@ Printf.sprintf "expr_to_rexpr failed: unable to find '%s' in globals" f)
  | EBinary (SigCons, n1, n2, _)-> RSignal { head = get_name n1; tail = get_name n2 }
  | ETuple (n1, n2, _) -> RCtor { tag = idof "tuple"; fields = [get_name n1; get_name n2] }
  | EBinary (BSync, n1, n2, _) -> RCtor { tag = idof "sync"; fields = [get_name n1; get_name n2] }
  | EBinary (BLaterApp, n1, n2, _) -> RCtor { tag = idof "later_app"; fields = [get_name n1; get_name n2] }
  | EBinary (BOStar, n1, n2, _) -> RCtor { tag = idof "ostar"; fields = [get_name n1; get_name n2] }
  | EBinary (op, n1, n2, _) -> RCall (op_to_application op, [get_name n1; get_name n2])
  | EUnary (UWait, n, _) -> RCtor { tag = idof "wait"; fields = [get_name n] }
  | EUnary (UTail, n, _) -> RCtor { tag = idof "tail"; fields = [get_name n] }
  | EUnary (UWatch, n, _) -> RCtor { tag = idof "watch"; fields = [get_name n] }
  | EUnary (UDelay, n, _) -> RCtor { tag = idof "delay"; fields = [get_name n] }
  | EUnary (Fst, EVar (x, _), _)  -> RProj (0, x)
  | EUnary (Snd, EVar (x, _), _)  -> RProj (1, x)
  | EConst (const, _) -> (
      match const with
      | CUnit -> RCtor { tag = idof "unit"; fields = [] } (* -> RCtor(int, [])*)
      | CNever -> RCtor { tag = idof "never"; fields = [] }
      | CInt _ -> RCtor { tag = idof "int"; fields = [Refcount.Const const] }
      | CString _ -> RCtor { tag = idof "string"; fields = [Refcount.Const const]}
      | CBool b -> if b then RCtor { tag = idof "true"; fields = []} else RCtor { tag = idof "false"; fields = []}
    )
  | _ -> failwith (Format.asprintf "expr_to_rexpr failed: invalid expression '%a'" Ast.pp_expr e)

and expr_to_fn_body globals locals (e: _ expr) : Refcount.fn_body = 
  let expr_to_fn_body = expr_to_fn_body globals in
  let expr_to_rexpr = expr_to_rexpr globals in
  match e with
  | EVar (x, _) -> FnRet (Var x)
  | EConst (c, _) -> FnRet (Const c)
  | ELet ((x, _), rhs, e', _) ->
    FnLet(x, expr_to_rexpr locals rhs, expr_to_fn_body (LocalsEnv.add x locals) e')
  | ECase (EVar (x,_), cases, _) -> 
    let get_fields_of_pattern = function (* Causion: are we counting the nested part correctly? *)
    | PVar _ | PWildcard | PConst _ -> 0
    | PTuple _ | PBoth _ -> 2 
    | PSigCons _ -> 5 (* should it be 2, 3, 4, 5? *)
    | PRight _ | PLeft _ -> 1
    in
    (*match mysync with
      | Left a ->
        Some (a+1)
      | Right b -> Some (b+10)
      | Both (a,b) -> Some (a + b)
        
      case mysync <2> of [
      1(let a = proj0 mysync in ctor1(a+1))
      2(let b = proj0 mysync in ctor2(b+10))
      3(let a = proj0 mysync in let b = proj1 mysync in ctor3(a+b))
      ]

      match mysig with
      | 5 :: tl_5 -> ...
      | 7 :: tl_7 -> ...
      | hd :: tl -> ...
      
      let hd = proj0 mysig in 
      case hd == 5 of [
      True<1>(let tl_5 = proj1 mysig in ...)
      False<2>(
        case hd == 7 of [
        True<1>(let tl_7 = proj1 mysig in ...)
        False<2>(let tl = proj1 mysig in ...)
        ]
      ]

      (hd == 5) -> True<1>

      header: <tag, refcount, fields...>
    *)
    FnCase(x, List.map (fun (c, branch, _) -> (get_fields_of_pattern c, expr_to_fn_body locals branch)) cases)
  | ECase _ -> 
    failwith "expr_to_fn_body failed: case scrutinee is not a variable"
  | EIfe (EVar (x, _), e1, e2, _) ->
    (* We decided booleans are objects with 0 fields but with tag 0 or 1*)
    FnCase (x, [(0,expr_to_fn_body locals e1); (0,expr_to_fn_body locals e2)]) 
  | EIfe _ -> failwith "expr_to_fn_body failed: if condition is not a variable"
  | _ -> 
    (* TODO: should ANF not handle this? *)
    let x = Utilities.new_var () in
    FnLet (x, expr_to_rexpr locals e, FnRet (Refcount.Var x))

let to_rc_intermediate_representation (builtins: Refcount.ownership list M.t) (p: _ program) : Refcount.program =
  (* maps  top-level-names to its parameter number *)
  let globals: globals_env = 
    let mapper = function 
    | TLet(name, EFun (params, _, _), _) -> (name, Some (List.length params))
    | TLet(name, _, _) -> (name, None)
    in
    let builtins_arity = M.map (fun b -> Some (List.length b)) builtins in
    List.map mapper p |> M.of_list 
    |> M.union (fun key _ _ -> failwith (Printf.sprintf "Duplicate global name %s" key)) builtins_arity
  in
  let to_fun = function
  | EFun (names, body, _) -> 
    let params = List.map fst names in
    Refcount.Fun (params, expr_to_fn_body globals (LocalsEnv.of_list params) body) 
  | _ -> failwith "TODO: only top level functions"
  in
  List.map (fun (TLet (name, rhs, _)) -> name, to_fun rhs) p
