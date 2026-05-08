open Ast

module Rc = Refcount_private.Refcount_core
module M = Map.Make(String)

open Collections

let get_name = function
  | EVar (x, _) -> Rc.Var x
  | EConst (c, _) -> Rc.Const c
  | EAnno (EVar (x, _), _, _) -> Rc.Var x
  | EAnno (EConst (c, _), _, _) -> Rc.Const c
  | _ -> failwith "get_name failed: expected variable or constant"

(** Finds the tag of a constructor by first looking in the [user_defined_ctor_mappings] and then in [Rizzo_builtins.ctor_mappings].
  Throws a runtime failure if [name] is not found in any of the mappings. *)
let tagof user_defined_ctor_mappings name = 
  match StringMap.find_opt name user_defined_ctor_mappings with
  | Some tag -> tag
  | None -> Rizzo_builtins.ctor_tag_of name

let case_arm ?tag ?num_fields body : Rc.case_arm =
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

type globals_env = int option M.t

module LocalsEnv = Set.Make(String)

let rec expr_to_rexpr ctor_tag_map globals locals (e : _ expr) : Rc.rexpr =
  let tagof = tagof ctor_tag_map in
  match e with
  | ECtor ((ctor_name, _), args, _) ->
      RCtor (Ctor { tag = tagof ctor_name; fields = List.map get_name args })
  | EApp (EVar (f, _), [EVar (x, _)], _) when Rizzo_builtins.is_builtin_projection f ->
      let proj_idx = (Rizzo_builtins.get f).projection_index |> Option.get in
      RProj (proj_idx, x)
  | EApp (EVar (f, _), args, _) when Rizzo_builtins.is_builtin f ->
      (match Rizzo_builtins.get f with
       | { param_ownership = Some ownerships; _ } when List.length ownerships = List.length args ->
           RCall (f, List.map get_name args)
       | _ -> RPartialApp (f, List.map get_name args))
  | EApp (EVar (f, _), [x], _) when LocalsEnv.mem f locals -> Rc.RVarApp (f, get_name x)
  | EApp (EVar (f, _), _, _) when LocalsEnv.mem f locals ->
      failwith (Printf.sprintf "expr_to_rexpr failed: variable application has more than one argument - if this case was hit, we did something wrong in %s" __FILE__)
  | EApp (EVar (f, _), xs, _) ->
      (match M.find_opt f globals with
       | Some Some arity when arity = List.length xs -> RCall (f, List.map get_name xs)
       | Some Some _ -> RPartialApp (f, List.map get_name xs)
       | Some None -> failwith @@ Printf.sprintf "expr_to_rexpr failed: trying to apply a non-function global '%s'" f
       | None -> failwith @@ Printf.sprintf "expr_to_rexpr failed: unable to find '%s' in globals" f)
  | EBinary (SigCons, n1, n2, _) -> RCtor (Signal { head = get_name n1; tail = get_name n2 })
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
  | EUnary (UProj idx, (EVar (x, _) | EAnno (EVar (x, _), _, _)), _) -> RProj (idx, x)
  | EConst (const, _) -> RConst const
  | EAnno (inner, _, _) -> expr_to_rexpr ctor_tag_map globals locals inner
  | _ -> failwith (Format.asprintf "expr_to_rexpr failed: invalid expression '%a'" Ast.pp_expr e)

and convert_var_apps f args x body : Rc.fn_body =
  let rec aux prev_name = function
    | [] -> body
    | [arg] -> Rc.FnLet (x, Rc.RVarApp (prev_name, get_name arg), body)
    | arg :: rest ->
        let intermediate_var = Utilities.new_name f in
        Rc.FnLet (intermediate_var, Rc.RVarApp (prev_name, get_name arg), aux intermediate_var rest)
  in
  aux f args

and constant_needs_split (globals : int option M.t) f args =
  Option.bind (M.find_opt f globals) (Option.map (fun i -> i < List.length args))
  |> Option.value ~default:false

and app_to_fn_body ctor_tag_map globals locals = 
  let expr_to_rexpr_partial = expr_to_rexpr ctor_tag_map globals in
  let expr_to_fn_body_partial = expr_to_fn_body ctor_tag_map globals in 
  function
  | EApp (EVar (f, _), args, _) when LocalsEnv.mem f locals ->
      let final_name = Utilities.new_var () in
      convert_var_apps f args final_name (FnRet (Var final_name))
  | EApp (EVar (f, _), args, _) when constant_needs_split globals f args ->
      let arity = M.find_opt f globals |> Option.get |> Option.get in
      let args_constant = List.take arity args |> List.map get_name in
      let constant_app_var = Utilities.new_var () in
      let var_apps_variable = Utilities.new_var () in
      let var_apps_body = convert_var_apps constant_app_var (List.drop arity args) var_apps_variable (FnRet (Var var_apps_variable)) in
      FnLet (constant_app_var, Rc.RCall (f, args_constant), var_apps_body)
  | EApp (EVar (f, _), EVar (x, _) :: (_ :: _ as rest), _) when Rizzo_builtins.is_builtin_projection f ->
      let proj_idx = (Rizzo_builtins.get f).projection_index |> Option.get in
      let proj_var = Utilities.new_var () in
      let apps_variable = Utilities.new_var () in
      let var_apps_body = convert_var_apps proj_var rest apps_variable (FnRet (Var apps_variable)) in
      FnLet (proj_var, Rc.RProj (proj_idx, x), var_apps_body)
  | ELet ((x, _), EApp (EVar (f, _), args, _), body, _) when LocalsEnv.mem f locals ->
      let body = expr_to_fn_body_partial (LocalsEnv.add x locals) body in
      convert_var_apps f args x body
  | ELet ((x, _), EApp (EVar (f, _), args, _), body, _) when constant_needs_split globals f args ->
      let arity = M.find_opt f globals |> Option.get |> Option.get in
      let args_constant = List.take arity args |> List.map get_name in
      let constant_app_var = Utilities.new_var () in
      let body = expr_to_fn_body_partial (LocalsEnv.add x locals) body in
      let var_apps_body = convert_var_apps constant_app_var (List.drop arity args) x body in
      FnLet (constant_app_var, Rc.RCall (f, args_constant), var_apps_body)
  | ELet ((vx, _), EApp (EVar (f, _), EVar (x, _) :: (_ :: _ as rest), _), body, _) when Rizzo_builtins.is_builtin_projection f ->
      let proj_idx = (Rizzo_builtins.get f).projection_index |> Option.get in
      let proj = Rc.RProj (proj_idx, x) in
      let proj_var = Utilities.new_var () in
      let body = expr_to_fn_body_partial (LocalsEnv.add vx locals) body in
      let var_apps_body = convert_var_apps proj_var rest vx body in
      FnLet (proj_var, proj, var_apps_body)
  | ELet ((x, _), rhs, body, _) ->
      FnLet (x, expr_to_rexpr_partial locals rhs, expr_to_fn_body_partial (LocalsEnv.add x locals) body)
  | expr ->
      let x = Utilities.new_var () in
      FnLet (x, expr_to_rexpr_partial locals expr, FnRet (Rc.Var x))

and expr_to_fn_body ctor_tag_map globals locals (e : _ expr) : Rc.fn_body =
  let expr_to_fn_body = expr_to_fn_body ctor_tag_map globals in
  let expr_to_rexpr = expr_to_rexpr ctor_tag_map globals in
  let lower_case_arm (pattern, branch, _) =
    let tagof = tagof ctor_tag_map in
    let body = expr_to_fn_body locals branch in
    match pattern with
    | PError _ -> failwith "Parse error pattern reached RC lowering"
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
    | PCtor ((ctor_name, _), args, _) -> case_arm ~tag:(tagof ctor_name) ~num_fields:(List.length args) body
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
  | EError _ -> failwith "Parse error expression reached RC lowering"
  | EVar (x, _) -> FnRet (Var x)
  | EConst (c, _) -> FnRet (Const c)
  | EApp _ | ELet _ -> app_to_fn_body ctor_tag_map globals locals e
  | ECase ((EVar (x, _) | EAnno (EVar (x, _), _, _)), cases, _) -> FnCase (x, lower_case_arms cases)
  | ECase _ -> failwith "expr_to_fn_body failed: case scrutinee is not a variable"
  | EIfe (EVar (x, _), e1, e2, _) ->
      FnCase (x, [
        case_arm ~tag:(tagof ctor_tag_map "true") ~num_fields:0 (expr_to_fn_body locals e1);
        case_arm ~tag:(tagof ctor_tag_map "false") ~num_fields:0 (expr_to_fn_body locals e2)
      ])
  | EIfe _ -> failwith "expr_to_fn_body failed: if condition is not a variable"
  | EAnno (inner, _, _) -> expr_to_fn_body locals inner
  | expr ->
      let x = Utilities.new_var () in
      FnLet (x, expr_to_rexpr locals expr, FnRet (Rc.Var x))

let to_rc_intermediate_representation (ctor_tag_map : int StringMap.t) (builtins : Rc.parameter_ownership) (p : _ program) : Refcount.program =
  let globals : globals_env =
    let mapper = function
      | TopTypeDef _ -> None
      | TopLet (name, EFun (params, _, _), _) -> Some (fst name, Some (List.length params))
      | TopLet (name, EAnno (EFun (params, _, _), _, _), _) -> Some (fst name, Some (List.length params))
      | TopLet (name, _, _) -> Some (fst name, None)
    in
    let toplevel_arity = M.of_list (List.filter_map mapper p) in
    let builtins_arity =
      builtins
      |> Collections.StringMap.to_list
      |> List.map (fun (name, ownerships) -> (name, Some (List.length ownerships)))
      |> M.of_list
    in
    M.union (fun key _ _ -> failwith (Printf.sprintf "Duplicate global name %s" key)) builtins_arity toplevel_arity
  in
  let to_fun names body =
    if List.exists (function PVar _ -> false | _ -> true) names 
    then failwith (Printf.sprintf "%s: whoops, we forgot to eliminate patterns from function parameters" __FILE__)
    else  
      let params = List.map (function PVar (n,_ ) -> n | _ -> failwith "bad pattern") names in
      Rc.Fun (params, expr_to_fn_body ctor_tag_map globals (LocalsEnv.of_list params) body)
  in
  let functions, globals =
    List.fold_right
      (fun te (funcs, globs) ->
        match te with
        | TopTypeDef _ -> (funcs, globs)
        | TopLet ((name,_ ), rhs, _) ->
          match rhs with
          | EFun (params, body, _) | EAnno (EFun (params, body, _), _, _) ->
              ((name, to_fun params body) :: funcs, globs)
          | _ ->
              (funcs, (name, expr_to_fn_body ctor_tag_map globals LocalsEnv.empty rhs) :: globs))
      p
      ([], [])
  in
  Refcount.RefProg { functions; globals }
