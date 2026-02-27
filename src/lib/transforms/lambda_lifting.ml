open Ast

module StringSet = Set.Make(String)


(** Finds the free variables of a function. 
    [top_decl_names] are the names of all top-level let-bindings. *)
let rec free_vars_fun top_decl_names params body =
  (* free variables are those not bound in EFun subtracted by the paramers of the EFun AND builtins AND top-level functions *)
  let builtins = List.map (fun ({name; _} : Rizzo_builtins.builtin_info) -> name) Rizzo_builtins.builtins in
  let params = List.map fst params in
  let to_diff = StringSet.union top_decl_names @@ StringSet.of_list  (params @ builtins) in
  StringSet.diff (free_vars_expr top_decl_names body) to_diff

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
    (* StringSet.diff (free_vars_expr body) (StringSet.of_list (List.map fst params)) *)
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

let rec lift (p: _ program) : _ program =
  let lifted_lambdas = ref [] in
  let top_decl_names = StringSet.of_list @@ List.map (fun (TLet(name,_,_)) -> name) p in
  let lifted_program = List.fold_right (fun te acc -> lift_top_expr top_decl_names lifted_lambdas te :: acc) p [] in
  List.rev_append !lifted_lambdas lifted_program

and lift_top_expr top_names (lifted_lambdas: _ top_expr list ref) (te: _ top_expr) : _ top_expr =
  let lift_expr = lift_expr top_names lifted_lambdas in
  match te with
  | TLet (x, EFun (params, body, fun_loc), let_loc) -> TLet (x, EFun (params, lift_expr body, fun_loc), let_loc)
  | TLet (x, e, loc) -> TLet (x, lift_expr e, loc)

and lift_expr top_names (lifted_lambdas: _ top_expr list ref) (e: _ expr) = 
  let lift_expr = lift_expr top_names lifted_lambdas in
  match e with
  | EConst _ | EVar _ -> e
  | ECtor (name, args, loc) -> ECtor (name, List.map lift_expr args, loc)
  | EApp (f, args, loc) -> EApp (lift_expr  f, List.map lift_expr args, loc)
  | EBinary (op, e1, e2, loc) -> EBinary (op, lift_expr  e1, lift_expr e2, loc)
  | EUnary (op, e, loc) -> EUnary (op, lift_expr  e, loc)
  | ELet (x, e1, e2, loc) ->
      let lifted_e1 = lift_expr e1 in
      let lifted_e2 = lift_expr e2 in
      ELet (x, lifted_e1, lifted_e2, loc)
  | ETuple (e1, e2, loc) -> ETuple (lift_expr e1, lift_expr e2, loc)
  | EIfe (cond, e1, e2, loc) -> 
    EIfe (lift_expr cond, lift_expr e1, lift_expr e2, loc)
  | ECase (scrutinee, branches, loc) -> 
    ECase (lift_expr scrutinee, List.map (fun (p, b, loc) -> (p, lift_expr b, loc)) branches, loc)
  | EFun (params, body, loc) ->
    let fv = StringSet.to_list (free_vars_fun top_names params body) in
    let fv_with_loc = List.map (fun v -> (v, loc)) fv in
    let lifted_loc = loc in
    let name = Utilities.new_name "lifted_fun" in
    let lifted_body = lift_expr body in
    lifted_lambdas := TLet (name, EFun(fv_with_loc @ params, lifted_body, lifted_loc), lifted_loc) :: !lifted_lambdas;
    if List.length fv = 0 then EVar (name, loc)
    else EApp (EVar (name, loc), List.map (fun v -> EVar (v, loc)) fv, loc) (* TODO optimise with partial application when possible *)
