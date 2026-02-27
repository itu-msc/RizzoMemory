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
    ("sigcons", -1);
    ("nothing", 0);
    ("just", 1);
  ]
let tagof name = match M.find_opt name ctorMappings with
  | Some id -> id
  | None -> failwith (Printf.sprintf "Constructor '%s' not found in mapping" name)

(* TODO: change when user defined types are added *)
let ctor_tag_of_name (name : string) : int =
  tagof (String.lowercase_ascii name)

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

let rec expr_to_rexpr globals locals (e: _ expr): Refcount.rexpr = 
  match e with
  | ECtor ((ctor_name, _), args, _) ->
    RCtor (Ctor { tag = ctor_tag_of_name ctor_name; fields = List.map get_name args })
  | EApp (EVar (f, _), [EVar (x, _)], _) when Rizzo_builtins.is_builtin_projection f ->
    let proj_idx = (Rizzo_builtins.get f).projection_index |> Option.get in
    RProj (proj_idx, x)
  | EApp (EVar (f, _), args, _) when Rizzo_builtins.is_builtin f -> 
    (match Rizzo_builtins.get f with
    | { param_ownership = Some ownerships; _ } when List.length ownerships = List.length args -> 
      RCall (f, List.map get_name args)
    | _ -> RPartialApp (f, List.map get_name args)
    )
  | EApp (EVar (f, _), [x], _) when LocalsEnv.mem f locals -> Refcount.RVarApp (f, get_name x)
  | EApp (EVar (f, _), _,_) when LocalsEnv.mem f locals -> 
    failwith "expr_to_rexpr failed: variable application has more than one argument - not supported in refcount IR"
    (* f x1 x2 x3 -> (((f x1) x2) x3)
      =>
      let fx1 = f x1 in
      let fx2 = fx1 x2 in
      let fx3 = fx2 x3 in
      fx3
    *)
    (* let fx1 = Refcount.RVarApp (f, get_name x) in
    List.fold_left (fun acc arg ->
      let intermediate_var = Utilities.new_var () in
      let binding  = Refcount.FnLet (intermediate_var, acc, rest_rexpr) in
      Refcount.RVarApp (intermediate_var, get_name arg)
    ) fx1 xs *)
  | EApp (EVar (f, _), xs, _) -> 
    (match M.find_opt f globals with
    | Some Some arity when arity = List.length xs -> RCall (f, List.map get_name xs)
    | Some Some _ -> RPartialApp (f, List.map get_name xs)
    | Some None -> failwith @@ Printf.sprintf "expr_to_rexpr failed: trying to apply a non-function global '%s'" f
    | None -> failwith @@ Printf.sprintf "expr_to_rexpr failed: unable to find '%s' in globals" f)
  | EBinary (SigCons, n1, n2, _)-> RCtor (Signal { head = get_name n1; tail = get_name n2 })
  | ETuple (n1, n2, _) -> RCtor (Ctor { tag = tagof "tuple"; fields = [get_name n1; get_name n2] })
  | EBinary (BSync, n1, n2, _) -> RCtor (Ctor { tag = tagof "sync"; fields = [get_name n1; get_name n2] })
  | EBinary (BLaterApp, n1, n2, _) -> RCtor (Ctor { tag = tagof "later_app"; fields = [get_name n1; get_name n2] })
  | EBinary (BOStar, n1, n2, _) -> RCtor (Ctor { tag = tagof "ostar"; fields = [get_name n1; get_name n2] })
  | EBinary (op, n1, n2, _) -> RCall (op_to_application op, [get_name n1; get_name n2])
  | EUnary (UWait, n, _) -> RCtor (Ctor { tag = tagof "wait"; fields = [get_name n] })
  | EUnary (UTail, n, _) -> RCtor (Ctor { tag = tagof "tail"; fields = [get_name n] })
  | EUnary (UWatch, n, _) -> RCtor (Ctor { tag = tagof "watch"; fields = [get_name n] })
  | EUnary (UDelay, n, _) -> RCtor (Ctor { tag = tagof "delay"; fields = [get_name n] })
  | EUnary (Fst, EVar (x, _), _)  -> RProj (0, x)
  | EUnary (Snd, EVar (x, _), _)  -> RProj (1, x)
  | EUnary (UProj idx, EVar (x, _), _) -> RProj (idx, x)
  | EConst (const, _) -> RConst const
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
    | PTuple _ -> 2 
    | PSigCons _ -> 5 (* should it be 2, 3, 4, 5? *)
    | PCtor (_, args, _) -> List.length args 
    in
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
    | TopLet(name, EFun (params, _, _), _) -> (name, Some (List.length params))
    | TopLet(name, _, _) -> (name, None)
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
  List.map (fun (TopLet (name, rhs, _)) -> name, to_fun rhs) p
