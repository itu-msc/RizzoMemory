open! Ast

let parsed_ann : type s. s ann -> parsed ann = fun ann ->
  Ann_parsed (get_location ann)

let parsed_name ((name, ann) : _ name) : parsed name =
  (name, parsed_ann ann)

let parsed_const c ann = EConst (c, parsed_ann ann)
let parsed_var name ann = EVar (name, parsed_ann ann)

let parsed_app name args ann =
  let name_ann = parsed_ann ann in
  EApp (EVar (name, name_ann), args, parsed_ann ann)

let parsed_let name rhs body ann =
  ELet (parsed_name name, rhs, body, parsed_ann ann)

let parsed_if cond if_true if_false ann =
  EIfe (cond, if_true, if_false, parsed_ann ann)

let is_simple = function
  | EVar _ | EConst _ -> true
  | _ -> false

let expr_typ (e : typed expr) = ann_get_type (expr_get_ann e)

let rec pattern_is_string : typed pattern -> bool = function
  | PConst (CString _, _) -> true
  | PVar (_, Ann_typed (_, TString)) -> true
  | PSigCons (_, _, Ann_typed (_, TString)) -> true
  | PSigCons _ -> false
  | PTuple (p1, p2, _) -> pattern_is_string p1 || pattern_is_string p2
  | PCtor (_, ps, _) -> List.exists pattern_is_string ps
  | PWildcard | PVar _ | PConst _ -> false

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
  | ECase (scrutinee, branches, ann) when List.exists (fun (pattern, _, _) -> pattern_is_string pattern) branches ->
      lower_string_case scrutinee branches ann
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
  | PCtor (name, args, ann) -> PCtor (parsed_name name, List.map lower_pattern args, parsed_ann ann)

and lower_string_case scrutinee branches ann =
  let lowered_scrutinee = lower_expr scrutinee in
  let loc = parsed_ann ann in
  let compile scrutinee_name =
    compile_string_branches (parsed_var scrutinee_name ann) branches ann
  in
  if is_simple lowered_scrutinee then
    compile_string_branches lowered_scrutinee branches ann
  else
    let tmp = (Utilities.new_name "string_match", parsed_ann ann) in
    ELet (tmp, lowered_scrutinee, compile (fst tmp), loc)

and compile_string_branches scrutinee branches ann =
  match branches with
  | [] -> parsed_app "match_fail" [parsed_const (CString "Non-exhaustive string match") ann] ann
  | (pattern, body, _) :: rest ->
      compile_string_pattern scrutinee pattern (lower_expr body) (fun () -> compile_string_branches scrutinee rest ann) ann

and compile_string_pattern scrutinee pattern success next ann =
  match pattern with
  | PWildcard -> success
  | PVar (name, name_ann) ->
      parsed_let (name, name_ann) scrutinee success ann
  | PConst (CString s, patt_ann) ->
      parsed_if
        (parsed_app "string_eq" [scrutinee; parsed_const (CString s) patt_ann] ann)
        success
        (next ())
        ann
  | PSigCons (head_pat, tail_name, _) ->
      lower_string_cons scrutinee head_pat tail_name success next ann
  | _ ->
      failwith "typed lowering encountered an unsupported string pattern"

and lower_string_cons scrutinee head_pat tail_name success next ann =
  let head_tmp = (Utilities.new_name "string_head", parsed_ann ann) in
  let tail_expr () = parsed_app "string_tail" [scrutinee] ann in
  let bind_tail body = parsed_let tail_name (tail_expr ()) body ann in
  let head_body =
    match head_pat with
    | PWildcard -> bind_tail success
    | PVar (name, name_ann) ->
        parsed_let (name, name_ann) (parsed_var (fst head_tmp) ann) (bind_tail success) ann
    | PConst (CString s, patt_ann) ->
        parsed_if
          (parsed_app "string_eq" [parsed_var (fst head_tmp) ann; parsed_const (CString s) patt_ann] ann)
          (bind_tail success)
          (next ())
          ann
    | _ ->
        failwith "typed lowering encountered an unsupported string head pattern"
  in
  parsed_if
    (parsed_app "string_is_empty" [scrutinee] ann)
    (next ())
    (parsed_let head_tmp (parsed_app "string_head" [scrutinee] ann) head_body ann)
    ann
