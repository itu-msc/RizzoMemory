open Ast

let has_overlap xs ys =
  List.exists (fun x -> List.mem x ys) xs

let rec collect_consecutive_params (params : string list) (body : expr) : string list * expr =
  match body with
  | EFun (next_params, next_body) when not (has_overlap params next_params) ->
      collect_consecutive_params (params @ next_params) next_body
  | _ ->
      (params, body)

let rec eliminate_consecutive_lambdas_expr (e : expr) : expr =
  match e with
  | EConst _ | EVar _ -> e
  | ELet (x, e1, e2) ->
      ELet (x, eliminate_consecutive_lambdas_expr e1, eliminate_consecutive_lambdas_expr e2)
  | EFun (params, body) ->
      let body' = eliminate_consecutive_lambdas_expr body in
      let params', body'' = collect_consecutive_params params body' in
      EFun (params', body'')
  | EApp (f, args) ->
      EApp (eliminate_consecutive_lambdas_expr f, List.map eliminate_consecutive_lambdas_expr args)
  | EUnary (op, e1) ->
      EUnary (op, eliminate_consecutive_lambdas_expr e1)
  | EBinary (op, e1, e2) ->
      EBinary (op, eliminate_consecutive_lambdas_expr e1, eliminate_consecutive_lambdas_expr e2)
  | ETuple (e1, e2) ->
      ETuple (eliminate_consecutive_lambdas_expr e1, eliminate_consecutive_lambdas_expr e2)
  | ECase (scrutinee, branches) ->
      ECase (
        eliminate_consecutive_lambdas_expr scrutinee,
        List.map (fun (p, b) -> (p, eliminate_consecutive_lambdas_expr b)) branches
      )
  | EIfe (cond, e1, e2) ->
      EIfe (
        eliminate_consecutive_lambdas_expr cond,
        eliminate_consecutive_lambdas_expr e1,
        eliminate_consecutive_lambdas_expr e2
      )

let eliminate_consecutive_lambdas_program (p : program) : program =
  List.map (fun (TLet (x, e)) -> TLet (x, eliminate_consecutive_lambdas_expr e)) p
