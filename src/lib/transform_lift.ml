open Ast

module StringSet = Set.Make(String)

let rec free_vars_expr e : StringSet.t = 
  match e with
  | EConst _ -> StringSet.empty
  | EVar (x, _) when Rizzo_builtins.is_builtin x -> StringSet.empty
  | EVar (x, _) -> StringSet.singleton x
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
  | EFun (params, body, _) ->
    (* fun x -> let z = 2 in fun y -> x + z + y  
      => 
      lifted1: fun x -> let z = 2 in lifted x z
      lifted2: fun x z y -> x + z + y
    *)
    (*
      let x = 1 in
      let f = fun y -> x + y in
      =>
      lifted: fun x y -> x + y
      let x = 1 in
      let f = lifted x in
    *)
    (* TODO optimise with partial application when possible *)
    StringSet.diff (free_vars_expr body) (StringSet.of_list (List.map fst params))

let rec lift (p: _ program) : _ program =
  let globals = ref [] in
  let lifted_program = List.fold_right (fun te acc -> lift_top_expr globals te :: acc) p [] in
  List.rev_append !globals lifted_program

and lift_top_expr (globals: _ top_expr list ref) (te: _ top_expr) : _ top_expr =
  match te with
  | TLet (x, EFun (params, body, fun_loc), let_loc) -> TLet (x, EFun (params, lift_expr globals body, fun_loc), let_loc)
  | TLet (x, e, loc) -> TLet (x, lift_expr globals e, loc)

and lift_expr (globals: _ top_expr list ref) (e: _ expr) = match e with
  | EConst _ | EVar _ -> e
  | EApp (f, args, loc) -> EApp (lift_expr globals f, List.map (lift_expr globals) args, loc)
  | EBinary (op, e1, e2, loc) -> EBinary (op, lift_expr globals e1, lift_expr globals e2, loc)
  | EUnary (op, e, loc) -> EUnary (op, lift_expr globals e, loc)
  | ELet (x, e1, e2, loc) ->
      let lifted_e1 = lift_expr globals e1 in
      let lifted_e2 = lift_expr globals e2 in
      ELet (x, lifted_e1, lifted_e2, loc)
  | ETuple (e1, e2, loc) -> ETuple (lift_expr globals e1, lift_expr globals e2, loc)
  | EIfe (cond, e1, e2, loc) -> 
    EIfe (lift_expr globals cond, lift_expr globals e1, lift_expr globals e2, loc)
  | ECase (scrutinee, branches, loc) -> 
    ECase (lift_expr globals scrutinee, List.map (fun (p, b, loc) -> (p, lift_expr globals b, loc)) branches, loc)
  | EFun (params, body, loc) ->
    let fv = StringSet.to_list (StringSet.diff (free_vars_expr body) (StringSet.of_list (List.map fst params))) in
    let fv_with_loc = List.map (fun v -> (v, loc)) fv in
    let lifted_loc = loc in
    let name = Utilities.new_name "lifted_fun" in
    let lifted_body = lift_expr globals body in
    globals := TLet (name, EFun(fv_with_loc @ params, lifted_body, lifted_loc), lifted_loc) :: !globals;
    if List.length fv = 0 then EVar (name, loc)
    else EApp (EVar (name, loc), List.map (fun v -> EVar (v, loc)) fv, loc)
