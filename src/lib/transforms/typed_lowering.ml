open! Ast

let parsed_ann : type s. s ann -> parsed ann = fun ann ->
  Ann_parsed (get_location ann)

let parsed_name ((name, ann) : _ name) : parsed name =
  (name, parsed_ann ann)

let parsed_app name args ann =
  let name_ann = parsed_ann ann in
  EApp (EVar (name, name_ann), args, parsed_ann ann)

let rec lower_program (p : typed program) : parsed program =
  List.map lower_top_expr p

and lower_top_expr = function
  | TopLet (name, expr, ann) -> TopLet (parsed_name name, lower_expr expr, parsed_ann ann)

and lower_expr (e : typed expr) : parsed expr =
  match e with
  | EConst (c, ann) -> EConst (c, parsed_ann ann)
  | EVar name -> EVar (parsed_name name)
  | ECtor (name, args, ann) -> ECtor (parsed_name name, List.map lower_expr args, parsed_ann ann)
  | ELet (name, rhs, body, ann) ->
      ELet (parsed_name name, lower_expr rhs, lower_expr body, parsed_ann ann)
  | EFun (params, body, ann) ->
      EFun (List.map parsed_name params, lower_expr body, parsed_ann ann)
  | EApp (fn, args, ann) ->
      EApp (lower_expr fn, List.map lower_expr args, parsed_ann ann)
  | EUnary (op, e1, ann) ->
      EUnary (op, lower_expr e1, parsed_ann ann)
  | EBinary (Add, e1, e2, ann) ->
      let lowered_args = [lower_expr e1; lower_expr e2] in
      (match ann_get_type ann with
      | TInt -> parsed_app "add" lowered_args ann
      | TString -> parsed_app "string_concat" lowered_args ann
      | t ->
          failwith
            (Format.asprintf "typed lowering expected '+' to have Int or String type, got '%a'" pp_typ t))
  | EBinary (op, e1, e2, ann) ->
      EBinary (op, lower_expr e1, lower_expr e2, parsed_ann ann)
  | ETuple (e1, e2, ann) ->
      ETuple (lower_expr e1, lower_expr e2, parsed_ann ann)
  | ECase (scrutinee, branches, ann) ->
      ECase (lower_expr scrutinee, List.map lower_case_branch branches, parsed_ann ann)
  | EIfe (cond, if_true, if_false, ann) ->
      EIfe (lower_expr cond, lower_expr if_true, lower_expr if_false, parsed_ann ann)
  | EAnno (e1, typ, ann) ->
      EAnno (lower_expr e1, typ, parsed_ann ann)

and lower_case_branch (pattern, body, ann) =
  (lower_pattern pattern, lower_expr body, parsed_ann ann)

and lower_pattern : type s. s pattern -> parsed pattern = function
  | PWildcard -> PWildcard
  | PVar (name, ann) -> PVar (name, parsed_ann ann)
  | PConst (c, ann) -> PConst (c, parsed_ann ann)
  | PTuple (p1, p2, ann) -> PTuple (lower_pattern p1, lower_pattern p2, parsed_ann ann)
  | PSigCons (p1, p2, ann) -> PSigCons (lower_pattern p1, parsed_name p2, parsed_ann ann)
  | PStringCons (p1, p2, ann) -> PStringCons (lower_pattern p1, parsed_name p2, parsed_ann ann)
  | PCtor (name, args, ann) -> PCtor (parsed_name name, List.map lower_pattern args, parsed_ann ann)
