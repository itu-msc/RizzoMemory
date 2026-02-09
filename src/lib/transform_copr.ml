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

let eliminate_copy_propagation (e: expr) : expr =
  let remove_keys keys env =
    List.filter (fun (k, _) -> not (List.mem k keys)) env
  in
  let rec aux (env: (string * expr) list) (e: expr) : expr =
    match e with
    | EConst _ -> e
    | EVar x -> Option.value (List.assoc_opt x env) ~default:e
    | EApp (f, args) -> EApp (aux env f, List.map (aux env) args)
    | EBinary (op, e1, e2) -> EBinary (op, aux env e1, aux env e2)
    | EUnary (op, e) -> EUnary (op, aux env e)
    | ELet (x, e1, e2) ->
        let e1' = aux env e1 in
        (match e1' with
          | EVar y -> (* let x = y *)
            let env' = (x, EVar y) :: env in 
            aux env' e2
          | _ -> ELet (x, e1', aux env e2)
        )
    | ETuple (e1, e2) -> ETuple (aux env e1, aux env e2)
    | ECase (scrutinee, branches) ->
        ECase (aux env scrutinee, List.map (aux env) branches)
    | EFun (params, body) ->
        let env' = remove_keys params env in
        EFun (params, aux env' body)
  in
  aux [] e

let copy_propagate (p:Ast.program) : Ast.program =
  List.map (fun (TLet (x, e)) -> TLet(x, eliminate_copy_propagation e)) p