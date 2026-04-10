(* after all other transformations *)
open Ast
let get_name = 
  function
  | EVar (x, _) -> Refcount.Var x
  | EConst (c, _) -> Refcount.Const c
  | EAnno (EVar (x, _), _, _) -> Refcount.Var x
  | EAnno (EConst (c, _), _, _) -> Refcount.Const c
  | _ -> failwith "get_name failed: expected variable or constant"

module M = Map.Make(String)

let tagof = Rizzo_builtins.ctor_tag_of

(* TODO: change when user defined types are added *)
let ctor_tag_of_name (name : string) : int = 
  Rizzo_builtins.ctor_tag_of name

let case_arm ?tag ?num_fields body : Refcount.case_arm =
  { tag; num_fields; body }

let op_to_application = function
  | Add -> "add"
  | Mul -> "mul"
  | Sub -> "sub"
  | Div -> "div"
  | Mod -> "mod"
  | Eq -> "eq"
  | Lt -> "lt"
  | Leq -> "leq"
  | Gt -> "gt"
  | Geq -> "geq"
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
    failwith (Printf.sprintf "expr_to_rexpr failed: variable application has more than one argument - if this case was hit, we did something wrong in %s" __FILE__)
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
  | EUnary (UNot, n, _) -> RCall ("not", [get_name n])
  | EUnary (UProj idx, EVar (x, _), _) -> RProj (idx, x)
  | EConst (const, _) -> RConst const
  | EAnno (e, _, _) -> expr_to_rexpr globals locals e
  | _ -> failwith (Format.asprintf "expr_to_rexpr failed: invalid expression '%a'" Ast.pp_expr e)

and convert_var_apps f args x body : Refcount.fn_body =  
  let rec aux prev_name = function
      | [] -> body
      | [arg] -> Refcount.FnLet (x, Refcount.RVarApp (prev_name, get_name arg), body)
      | arg :: args ->
        let intermediate_var = Utilities.new_name f in
        Refcount.FnLet (intermediate_var, Refcount.RVarApp (prev_name, get_name arg), aux intermediate_var args)
  in
  aux f args 
and constant_needs_split (globals: int option M.t) f args =
  Option.bind (M.find_opt f globals) (Option.map (fun i -> i < List.length args))
  |> Option.value ~default: false

and app_to_fn_body globals locals = function
  | EApp (EVar (f, _), args, _) when LocalsEnv.mem f locals -> 
    let final_name = Utilities.new_var () in 
    convert_var_apps f args final_name (FnRet (Var final_name)) 
  | EApp (EVar (f, _), args, _) when constant_needs_split globals f args ->
    (* assuming constant_needs_split already checked that f exists! *)
    let arity = M.find_opt f globals |> Option.get |> Option.get in
    let args_constant = List.take arity args |> List.map get_name in
    let constant_app_var = Utilities.new_var () in
    let var_apps_variable = Utilities.new_var () in
    let var_apps_body = convert_var_apps constant_app_var (List.drop arity args) var_apps_variable (FnRet (Var var_apps_variable)) in
    FnLet (constant_app_var, Refcount.RCall (f, args_constant), var_apps_body)
  | EApp (EVar (f, _), EVar (x, _) :: (_ :: _ as rest), _) when Rizzo_builtins.is_builtin_projection f ->
    let proj_idx = (Rizzo_builtins.get f).projection_index |> Option.get in
    let proj_var = Utilities.new_var () in
    let apps_variable = Utilities.new_var () in
    let var_apps_body = convert_var_apps proj_var rest apps_variable (FnRet (Var apps_variable)) in
    FnLet (proj_var, Refcount.RProj (proj_idx, x), var_apps_body)
  | ELet ((x,_), EApp(EVar (f, _), args, _), body, _) when LocalsEnv.mem f locals ->
    let body = expr_to_fn_body globals (LocalsEnv.add x locals) body in
    convert_var_apps f args x body
  | ELet ((x, _), EApp (EVar (f, _), args, _), body, _) when constant_needs_split globals f args -> 
    let arity = M.find_opt f globals |> Option.get |> Option.get in
    let args_constant = List.take arity args |> List.map get_name in
    let constant_app_var = Utilities.new_var () in
    let body = expr_to_fn_body globals (LocalsEnv.add x locals) body in
    let var_apps_body = convert_var_apps constant_app_var (List.drop arity args) x body in
    FnLet (constant_app_var, Refcount.RCall (f, args_constant), var_apps_body) 
  | ELet ((vx,_), EApp (EVar (f, _), EVar (x, _) :: (_ :: _ as rest), _), body, _) when Rizzo_builtins.is_builtin_projection f ->
    let proj_idx = (Rizzo_builtins.get f).projection_index |> Option.get in
    let proj = Refcount.RProj (proj_idx, x) in
    let proj_var = Utilities.new_var () in
    let body = expr_to_fn_body globals (LocalsEnv.add vx locals) body in
    let var_apps_body = convert_var_apps proj_var rest vx body in
    FnLet (proj_var, proj, var_apps_body)
  | ELet ((x, _), rhs, body, _) -> 
    FnLet(x, expr_to_rexpr globals locals rhs, expr_to_fn_body globals (LocalsEnv.add x locals) body)
  | e -> 
    let x = Utilities.new_var () in
    FnLet (x, expr_to_rexpr globals locals e, FnRet (Refcount.Var x))
    
and expr_to_fn_body globals locals (e: _ expr) : Refcount.fn_body = 
  let expr_to_fn_body = expr_to_fn_body globals in
  let expr_to_rexpr = expr_to_rexpr globals in
  let lower_case_arm (pattern, branch, _) =
    let body = expr_to_fn_body locals branch in
    match pattern with
    | PVar _ | PWildcard _ -> case_arm body
    | PConst (CBool false, _) -> case_arm ~tag:(tagof "false") ~num_fields:0 body
    | PConst (CBool true, _) -> case_arm ~tag:(tagof "true") ~num_fields:0 body
    | PConst (CNever, _) -> case_arm ~tag:(tagof "never") ~num_fields:0 body
    | PConst (CUnit, _) -> case_arm ~tag:(tagof "unit") ~num_fields:0 body
    | PConst (CString _, _) -> failwith "String patterns should be eliminated before RC lowering"
    | PConst (CInt _, _) -> failwith "Integer patterns are not supported in RC lowering"
    | PTuple _ -> case_arm ~tag:(tagof "tuple") ~num_fields:2 body
    | PSigCons _ -> case_arm ~tag:0 ~num_fields:5 body
    | PStringCons _ -> failwith "String patterns should be eliminated before RC lowering"
    | PCtor ((ctor_name, _), args, _) ->
        case_arm ~tag:(ctor_tag_of_name ctor_name) ~num_fields:(List.length args) body
  in
  let lower_case_arms cases =
    let rec aux seen_tags acc = function
      | [] -> List.rev acc
      | case :: rest ->
          let arm = lower_case_arm case in
          match arm.tag with
          | None -> List.rev (arm :: acc)
          | Some tag when List.mem tag seen_tags -> aux seen_tags acc rest
          | Some tag -> aux (tag :: seen_tags) (arm :: acc) rest
    in
    aux [] [] cases
  in
  match e with
  | EVar (x, _) -> FnRet (Var x)
  | EConst (c, _) -> FnRet (Const c)
  (* TODO: couldn't we just move this elsewhere? shouldn't we just add this sort of logic to anf? *)
  | EApp _ | ELet _ -> app_to_fn_body globals locals e
  | ECase (EVar (x,_), cases, _) -> 
    FnCase (x, lower_case_arms cases)
  | ECase _ -> 
    failwith "expr_to_fn_body failed: case scrutinee is not a variable"
  | EIfe (EVar (x, _), e1, e2, _) ->
    (* We decided booleans are objects with 0 fields but with tag 0 or 1*)
    FnCase (x, [
      case_arm ~tag:(tagof "true") ~num_fields:0 (expr_to_fn_body locals e1);
      case_arm ~tag:(tagof "false") ~num_fields:0 (expr_to_fn_body locals e2)
    ]) 
  | EIfe _ -> failwith "expr_to_fn_body failed: if condition is not a variable"
  | EAnno (e, _, _) -> expr_to_fn_body locals e
  | _ -> 
    (* TODO: should ANF not handle this? *)
    let x = Utilities.new_var () in
    FnLet (x, expr_to_rexpr locals e, FnRet (Refcount.Var x))

let to_rc_intermediate_representation (builtins: Refcount.ownership list M.t) (p: _ program) : Refcount.program =
  (* maps  top-level-names to its parameter number *)
  let globals: globals_env = 
    let mapper = function 
    | TopLet(name, EFun (params, _, _), _) -> (fst name, Some (List.length params))
    | TopLet(name, EAnno(EFun (params, _, _), _, _), _) -> (fst name, Some (List.length params))
    | TopLet(name, _, _) -> (fst name, None)
    in
    let toplevel_arity = M.of_list (List.map mapper p) in
    let builtins_arity = M.map (fun b -> Some (List.length b)) builtins in
    M.union (fun key _ _ -> failwith (Printf.sprintf "Duplicate global name %s" key)) builtins_arity toplevel_arity
  in
  let to_fun names body =
    let params = List.map fst names in
    Refcount.Fun (params, expr_to_fn_body globals (LocalsEnv.of_list params) body)
  in
  let (functions, globals) = List.fold_right (fun (TopLet (name, rhs, _)) (func, glob) -> 
    match rhs with
    | EFun (names, body, _) | EAnno(EFun (names, body,_), _ ,_ ) ->
      ((fst name, to_fun names body) :: func, glob)
    | _ -> 
      (func, (fst name, expr_to_fn_body globals (LocalsEnv.empty) rhs) :: glob)
  ) p ([], [])
  in
  Refcount.RefProg { functions; globals }
