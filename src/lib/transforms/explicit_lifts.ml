open Ast

module StringSet = Set.Make(String)

let builtin_function_names =
  Rizzo_builtins.builtins
  |> List.filter_map (fun ({ name; typ; _ } : Rizzo_builtins.builtin_info) ->
         match typ with
         | TFun _ -> Some name
         | _ -> None)
  |> StringSet.of_list

let top_level_function_name = function
  | TopLet ((name, _), EFun _, _) -> Some name
  | TopLet ((name, _), EAnno (EFun _, _, _), _) -> Some name
  | _ -> None

let rec rewrite_expr function_names bound_names ~as_callee = function
  | EVar (((name, _) as var_name)) when (not as_callee) && StringSet.mem name function_names && not (StringSet.mem name bound_names) ->
      EApp (EVar var_name, [], snd var_name)
  | EConst _ | EVar _ as expr -> expr
  | ECtor (name, args, ann) ->
      ECtor (name, List.map (rewrite_expr function_names bound_names ~as_callee:false) args, ann)
  | ELet (((name, _) as bound_name), rhs, body, ann) ->
      let rhs' = rewrite_expr function_names bound_names ~as_callee:false rhs in
      let body' = rewrite_expr function_names (StringSet.add name bound_names) ~as_callee:false body in
      ELet (bound_name, rhs', body', ann)
  | EFun (params, body, ann) ->
      let bound_names =
        List.fold_left (fun acc (name, _) -> StringSet.add name acc) bound_names params
      in
      EFun (params, rewrite_expr function_names bound_names ~as_callee:false body, ann)
  | EApp (fn, args, ann) ->
      EApp
        ( rewrite_expr function_names bound_names ~as_callee:true fn,
          List.map (rewrite_expr function_names bound_names ~as_callee:false) args,
          ann )
  | EUnary (op, expr, ann) ->
      EUnary (op, rewrite_expr function_names bound_names ~as_callee:false expr, ann)
  | EBinary (op, left, right, ann) ->
      EBinary
        ( op,
          rewrite_expr function_names bound_names ~as_callee:false left,
          rewrite_expr function_names bound_names ~as_callee:false right,
          ann )
  | ETuple (left, right, ann) ->
      ETuple
        ( rewrite_expr function_names bound_names ~as_callee:false left,
          rewrite_expr function_names bound_names ~as_callee:false right,
          ann )
  | ECase (scrutinee, branches, ann) ->
      let scrutinee' = rewrite_expr function_names bound_names ~as_callee:false scrutinee in
      let branches' =
        List.map
          (fun (pattern, branch, branch_ann) ->
            let branch_bound_names =
              List.fold_left (fun acc name -> StringSet.add name acc) bound_names (pattern_bound_vars pattern)
            in
            (pattern, rewrite_expr function_names branch_bound_names ~as_callee:false branch, branch_ann))
          branches
      in
      ECase (scrutinee', branches', ann)
  | EIfe (cond, then_branch, else_branch, ann) ->
      EIfe
        ( rewrite_expr function_names bound_names ~as_callee:false cond,
          rewrite_expr function_names bound_names ~as_callee:false then_branch,
          rewrite_expr function_names bound_names ~as_callee:false else_branch,
          ann )
  | EAnno (expr, typ, ann) ->
      EAnno (rewrite_expr function_names bound_names ~as_callee:false expr, typ, ann)

let make_explicit (program : _ program) : _ program =
  let function_names =
    List.filter_map top_level_function_name program
    |> StringSet.of_list
    |> StringSet.union builtin_function_names
  in
  List.map
    (fun (TopLet (name, expr, ann)) ->
      TopLet (name, rewrite_expr function_names StringSet.empty ~as_callee:false expr, ann))
    program
