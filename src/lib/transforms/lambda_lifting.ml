open Ast
open Collections

(** Lambda lifting: transform all functions into top-level functions, and replace lambda expressions with applications of the lifted functions. *)

let rec lift (p: _ program) : _ program =
  let lifted_lambdas = ref [] in
  let top_decl_names = 
    StringSet.of_list (
      p
      |> List.filter_map (function 
      | TopTypeDef _ -> None
      | TopLet((name, _),_,_) -> Some name))in
  let lifted_program = List.fold_right (fun te acc -> lift_top_expr top_decl_names lifted_lambdas te :: acc) p [] in
  List.rev_append !lifted_lambdas lifted_program

and lift_top_expr top_names (lifted_lambdas: _ top_expr list ref) (te: _ top_expr) : _ top_expr =
  let lift_expr = lift_expr top_names lifted_lambdas in
  match te with
  | TopTypeDef _ as e -> e
  | TopLet (x, EFun (params, body, fun_loc), let_loc) -> TopLet (x, EFun (params, lift_expr body, fun_loc), let_loc)
  | TopLet (x, EAnno (EFun (params, body, fun_loc), t, annotation_loc), let_loc) ->
    TopLet (x, EAnno (EFun (params, lift_expr body, fun_loc), t, annotation_loc), let_loc)
  | TopLet (x, e, loc) -> TopLet (x, lift_expr e, loc)

and lift_expr top_names (lifted_lambdas: _ top_expr list ref) (e: _ expr) = 
  let lift_expr = lift_expr top_names lifted_lambdas in
  match e with
  | EConst _ | EVar _ -> e
  | ECtor (name, args, loc) -> ECtor (name, List.map lift_expr args, loc)
  | EApp (f, args, loc) -> EApp (lift_expr f, List.map lift_expr args, loc)
  | EBinary (op, e1, e2, loc) -> EBinary (op, lift_expr e1, lift_expr e2, loc)
  | ELet (x, e1, e2, loc) ->
      let lifted_e1 = lift_expr e1 in
      let lifted_e2 = lift_expr e2 in
      ELet (x, lifted_e1, lifted_e2, loc)
  | ETuple (e1, e2, es, loc) -> ETuple (lift_expr e1, lift_expr e2, List.map lift_expr es, loc)
  | EIfe (cond, e1, e2, loc) -> 
    EIfe (lift_expr cond, lift_expr e1, lift_expr e2, loc)
  | ECase (scrutinee, branches, loc) -> 
    ECase (lift_expr scrutinee, List.map (fun (p, b, loc) -> (p, lift_expr b, loc)) branches, loc)
  | EAnno (e, t, loc) -> EAnno (lift_expr e, t, loc)
  | EUnary (UDelay, e, loc) -> 
    let fv = StringSet.to_list (Ast_helpers.free_vars_fun top_names [] e) in
    let fv_with_loc = List.map (fun v -> (v, loc)) fv in
    let name = Utilities.new_name "thunk_" in
    let unit_name = Utilities.new_name "unit_arg" in
    let lifted_body = lift_expr e in
    let thunk = TopLet((name, loc), EFun (fv_with_loc @ [unit_name, loc], lifted_body, loc), loc) in
    (* it's fine to leave it as an EApp even when there were no free variables, 
       since a later transformation [explicit_lifts] produces exactly this anyway *)
    let access_thunk = EApp (EVar (name, loc), List.map (fun v -> EVar (v, loc)) fv, loc) in
    lifted_lambdas := thunk :: !lifted_lambdas;
    EUnary (UDelay, access_thunk, loc)
  | EUnary (op, e, loc) -> EUnary (op, lift_expr e, loc)
  | EFun (params, body, loc) ->
    let fv = StringSet.to_list (Ast_helpers.free_vars_fun top_names params body) in
    let fv_with_loc = List.map (fun v -> (v, loc)) fv in
    let lifted_loc = loc in
    let name = Utilities.new_name "lifted_fun" in
    let lifted_body = lift_expr body in
    lifted_lambdas := TopLet ((name, lifted_loc), EFun(fv_with_loc @ params, lifted_body, lifted_loc), lifted_loc) :: !lifted_lambdas;
    if List.length fv = 0 then EVar (name, loc)
    else EApp (EVar (name, loc), List.map (fun v -> EVar (v, loc)) fv, loc) (* TODO optimise with partial application when possible *)
