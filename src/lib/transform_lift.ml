open Ast

module StringSet = Set.Make(String)

let rec free_vars_expr e : StringSet.t = 
  match e with
  | EConst _ -> StringSet.empty
  | EVar x -> StringSet.singleton x
  | EApp (f, args) -> 
    StringSet.union (free_vars_expr f) (List.fold_left StringSet.union StringSet.empty (List.map free_vars_expr args))
  | EBinary (_, e1, e2) | ETuple (e1, e2) -> 
    StringSet.union (free_vars_expr e1) (free_vars_expr e2)
  | EUnary (_, e) -> free_vars_expr e
  | ELet (x, e1, e2) ->
    StringSet.union (free_vars_expr e1) (StringSet.remove x (free_vars_expr e2))
  | ECase (scrutinee, branches) -> 
    let scrutinee_fv = free_vars_expr scrutinee in
    List.fold_left StringSet.union scrutinee_fv (List.map free_vars_expr branches)
  | EFun (params, body) ->
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
    StringSet.diff (free_vars_expr body) (StringSet.of_list params)

let rec lift (p:program) : program =
  let globals = ref [] in
  let lifted_program = List.fold_right (fun te acc -> lift_top_expr globals te :: acc) p [] in
  List.rev_append !globals lifted_program

and lift_top_expr (globals: top_expr list ref) (te: top_expr) : top_expr =
  match te with
  | TLet (x, EFun (params, body)) -> TLet (x, EFun (params, lift_expr globals body))
  | TLet (x, e) -> TLet (x, lift_expr globals e)

and lift_expr (globals: top_expr list ref) (e: expr) = match e with
  | EConst _ | EVar _ -> e
  | EApp (f, args) -> EApp (lift_expr globals f, List.map (lift_expr globals) args)
  | EBinary (op, e1, e2) -> EBinary (op, lift_expr globals e1, lift_expr globals e2)
  | EUnary (op, e) -> EUnary (op, lift_expr globals e)
  | ELet (x, e1, e2) ->
      let lifted_e1 = lift_expr globals e1 in
      let lifted_e2 = lift_expr globals e2 in
      ELet (x, lifted_e1, lifted_e2)
  | ETuple (e1, e2) -> ETuple (lift_expr globals e1, lift_expr globals e2)
  | ECase (scrutinee, branches) -> 
    ECase (lift_expr globals scrutinee, List.map (lift_expr globals) branches)
  | EFun (params, body) ->
    let fv = StringSet.to_list (StringSet.diff (free_vars_expr body) (StringSet.of_list params)) in
    let name = Utilities.new_name "lifted_fun" in
    let lifted_body = lift_expr globals body in
    globals := TLet (name, EFun(fv @ params, lifted_body)) :: !globals;
    if List.length fv = 0 then EVar name
    else EApp (EVar name, List.map (fun v -> EVar v) fv)
