open Ast

(*
Before Copy Propagation 
y = x
z = 3 + y

After Copy Propagation
z = 3 + x
*)
(*
  let x = y in
  if x then 1 else 0
  => 
  if y then 1 else 0
*)

let eliminate_copy_propagation (e: _ expr) : _ expr =
  let remove_keys keys env =
    List.filter (fun (k, _) -> not (List.mem k keys)) env
  in
  let rec aux (env: (string * _ expr) list) (e: _ expr) : _ expr =
    match e with
    | EConst _ -> e
    | EVar (x, _) -> Option.value (List.assoc_opt x env) ~default:e
    | EApp (f, args, ann) -> EApp (aux env f, List.map (aux env) args, ann)
    | EBinary (op, e1, e2, ann) -> EBinary (op, aux env e1, aux env e2, ann)
    | EUnary (op, e, ann) -> EUnary (op, aux env e, ann)
    | ELet ((s, _) as x, e1, e2, ann) ->
        let e1' = aux env e1 in
        (match e1' with
          | EVar y -> (* let x = y *)
            let env' = (s, EVar y) :: env in 
            aux env' e2
          | _ -> ELet (x, e1', aux env e2, ann)
        )
    | ETuple (e1, e2, ann) -> ETuple (aux env e1, aux env e2, ann)
    | EIfe (cond, e1, e2, ann) -> 
        EIfe (aux env cond, aux env e1, aux env e2, ann)
    | ECase (scrutinee, branches, ann) ->
        ECase (
          aux env scrutinee,
          List.map
            (fun (pattern, branch, ann) ->
              let env' = remove_keys (Ast.pattern_bound_vars pattern) env in
              (pattern, aux env' branch, ann))
            branches, ann)
    | EFun (names, body, ann) ->
        let params = List.map fst names in
        let env' = remove_keys params env in
        EFun (names, aux env' body, ann)
  in
  aux [] e

let copy_propagate (p: _ program) : _ program =
  List.map (fun (TLet (x, e, ann)) -> TLet(x, eliminate_copy_propagation e, ann)) p