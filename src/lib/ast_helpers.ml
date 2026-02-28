open! Ast

module StringSet = Set.Make(String)

(** Finds the free variables of a function. 
    [top_decl_names] are the names of all top-level let-bindings. *)
let rec free_vars_fun top_decl_names params body =
  (* free variables are those not bound in EFun subtracted by the paramers of the EFun AND builtins AND top-level functions *)
  let builtins = List.map (fun ({name; _} : Rizzo_builtins.builtin_info) -> name) Rizzo_builtins.builtins in
  let params = List.map fst params in
  let to_diff = StringSet.union top_decl_names @@ StringSet.of_list  (params @ builtins) in
  StringSet.diff (free_vars_expr top_decl_names body) to_diff

(** Find free variables of an expression in a context with no globals (and no builtins)*)
and free_vars_expr_no_globals e = free_vars_expr StringSet.empty e

(** Finds the free variables of an expression *)
and free_vars_expr top_decl_names e : StringSet.t = 
  let free_vars_expr = free_vars_expr top_decl_names in  
  match e with
  | EConst _ -> StringSet.empty
  | EVar (x, _) -> StringSet.singleton x
  | ECtor (_, args, _) ->
    List.fold_left StringSet.union StringSet.empty (List.map free_vars_expr args)
  | EApp (f, args, _) -> 
    StringSet.union (free_vars_expr f) (List.fold_left StringSet.union StringSet.empty (List.map free_vars_expr args))
  | EBinary (_, e1, e2, _) | ETuple (e1, e2, _) -> 
    StringSet.union (free_vars_expr e1) (free_vars_expr e2)
  | EUnary (_, e, _) -> free_vars_expr e
  | ELet ((x,_), e1, e2, _) ->
    StringSet.union (free_vars_expr e1) (StringSet.remove x (free_vars_expr e2))
  | EIfe (cond, e1, e2, _) ->
    StringSet.union (free_vars_expr cond) (StringSet.union (free_vars_expr e1) (free_vars_expr e2))
  | ECase (scrutinee, branches, _) -> 
    let scrutinee_fv = free_vars_expr scrutinee in
    List.fold_left
      (fun acc (pattern, branch, _) ->
        let branch_fv = StringSet.diff (free_vars_expr branch) (StringSet.of_list @@ Ast.pattern_bound_vars pattern) in
        StringSet.union acc branch_fv)
      scrutinee_fv
      branches
  | EFun (params, body, _) -> free_vars_fun top_decl_names params body
  | EAnno (e, _, _) -> free_vars_expr e

let list1_length (Cons1 (_, rest)) = 1 + List.length rest
