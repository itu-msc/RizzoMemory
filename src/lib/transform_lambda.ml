open Ast

let has_overlap xs ys =
  List.exists (fun x -> List.mem x ys) xs

let rec collect_consecutive_params (params : _ name list) (body : _ expr) : _ name list * _ expr =
  match body with
  | EFun (next_params, next_body, _) when not (has_overlap params next_params) ->
      collect_consecutive_params (params @ next_params) next_body
  | _ ->
      (params, body)

let rec eliminate_consecutive_lambdas_expr (e : _ expr) : _ expr =
  match e with
  | EConst _ | EVar _ -> e
  | ECtor (name, args, loc) ->
      ECtor (name, List.map eliminate_consecutive_lambdas_expr args, loc)
  | ELet (x, e1, e2, loc) ->
      ELet (x, eliminate_consecutive_lambdas_expr e1, eliminate_consecutive_lambdas_expr e2, loc)
  | EFun (params, body, loc) ->
      let body' = eliminate_consecutive_lambdas_expr body in
      let params', body'' = collect_consecutive_params params body' in
      EFun (params', body'', loc)
  | EApp (f, args, loc) ->
      EApp (eliminate_consecutive_lambdas_expr f, List.map eliminate_consecutive_lambdas_expr args, loc)
  | EUnary (op, e1, loc) ->
      EUnary (op, eliminate_consecutive_lambdas_expr e1, loc)
  | EBinary (op, e1, e2, loc) ->
      EBinary (op, eliminate_consecutive_lambdas_expr e1, eliminate_consecutive_lambdas_expr e2, loc)
  | ETuple (e1, e2, loc) ->
      ETuple (eliminate_consecutive_lambdas_expr e1, eliminate_consecutive_lambdas_expr e2, loc)
  | ECase (scrutinee, branches, loc) ->
      ECase (
        eliminate_consecutive_lambdas_expr scrutinee,
        List.map (fun (p, b, l) -> (p, eliminate_consecutive_lambdas_expr b, l)) branches,
        loc
      )
  | EIfe (cond, e1, e2, loc) ->
      EIfe (
        eliminate_consecutive_lambdas_expr cond,
        eliminate_consecutive_lambdas_expr e1,
        eliminate_consecutive_lambdas_expr e2,
        loc
      )

let eliminate_consecutive_lambdas_program (p : _ program) : _ program =
  List.map (fun (TLet (x, e, loc)) -> TLet (x, eliminate_consecutive_lambdas_expr e, loc)) p
