open! Ast

let head_elim = (Rizzo_builtins.get "head").name
let string_eq = (Rizzo_builtins.get "string_eq").name
let string_is_empty = (Rizzo_builtins.get "string_is_empty").name
let string_head = (Rizzo_builtins.get "string_head").name
let string_tail = (Rizzo_builtins.get "string_tail").name
let match_fail = (Rizzo_builtins.get "match_fail").name

let is_var_or_wildcard = function 
  | PVar _ | PWildcard _ -> true 
  | _ -> false

let parsed_var ((name, ann) : _ name) = EVar (name, ann)

let parsed_app ((name, ann) : _ name) args =
  EApp (parsed_var (name, ann), args, ann)

let is_simple_expr = function
  | EVar _ | EConst _ -> true
  | _ -> false

let is_string_case_head = function
  | PWildcard _ | PVar _ | PConst (CString _, _) -> true
  | _ -> false

let is_string_case_pattern = function
  | PWildcard _ | PVar _ | PConst (CString _, _) -> true
  | PStringCons (head, _, _) -> is_string_case_head head
  | _ -> false

let is_string_case cases =
  let has_string_discriminator =
    List.exists (fun (pattern, _, _) ->
      match pattern with
      | PConst (CString _, _) | PStringCons _ -> true
      | _ -> false) cases
  in
  has_string_discriminator && List.for_all (fun (pattern, _, _) -> is_string_case_pattern pattern) cases

let rec sink_until_first_use name proj ann e = 
  let (var_name, _) = name in
  let used_in e = Collections.StringSet.mem var_name (Ast_helpers.free_vars_expr_no_globals e) in
  match e with
  | EVar (n1,_) when var_name = n1 -> ELet(name, proj, e, ann)
  | EVar _ | EConst _ -> e
  | ELet (name', rhs, e2, ann') -> 
    if used_in rhs then ELet(name, proj, e, ann)
    else ELet (name', rhs, sink_until_first_use name proj ann e2, ann')
  | EApp (f, args, _) when List.exists used_in args || used_in f -> ELet(name, proj, e, ann)
  | EBinary (_, e1, e2, _) when used_in e1 || used_in e2 -> ELet(name, proj, e, ann)
  | EUnary (op, e, ann') -> 
    EUnary (op, sink_until_first_use name proj ann e, ann')
  | ECtor (_, args, _) when List.exists used_in args -> ELet(name, proj, e, ann)
  | EAnno (e, t, ann') -> EAnno (sink_until_first_use name proj ann e, t, ann')
  | ETuple (e1, e2, es, _) when used_in e1 || used_in e2 || List.exists used_in es -> ELet(name, proj, e, ann)
  | EFun (_, body, _) when used_in body -> ELet(name, proj, e, ann)
  | EIfe (cond, e1, e2, ann') -> 
    if used_in cond then ELet(name, proj, e, ann)
    else
      let e1' = if used_in e1 then sink_until_first_use name proj ann e1 else e1 in
      let e2' = if used_in e2 then sink_until_first_use name proj ann e2 else e2 in
      EIfe (cond, e1', e2', ann')
  | ECase (scrutinee, branches, ann') -> 
    if used_in scrutinee then ELet(name, proj, e, ann)
    else
      let cases = List.map (fun (pat, branch, ann') -> (pat, sink_until_first_use name proj ann branch, ann')) branches in
      ECase (sink_until_first_use name proj ann scrutinee, cases, ann')
  | _ -> e

let rec transform_patterns (p: 's Ast.program) = 
  p |> List.map (function
    | TopTypeDef _ as e -> e
    | (TopLet (name, expr, ann)) -> TopLet (name, compile_match expr, ann))
    
and compile_simple_pattern scrutinee case_body = function
  | PWildcard _ ->
    Some (case_body ())
  | PVar (s, ann) -> 
    let x = ELet ((s, ann), scrutinee, case_body (), ann) in
    Some (x)
  | PCtor (_, ps, ann) when List.for_all is_var_or_wildcard ps -> 
    let projs = 
      (List.mapi (fun i p -> i,p) ps) 
      |> List.filter_map (fun (i,p) -> 
      match p with 
      | PVar (name, name_ann ) -> Some ((name, name_ann), EUnary (UProj i, scrutinee, ann))
      | PWildcard _ -> None
      | _ -> failwith "compile_simple_pattern NEVER HAPPENS!")
    in
    Some( List.fold_right (fun (name, proj) acc -> ELet (name, proj, acc, ann)) projs (case_body ()))
  | PTuple (p1, p2, ps_rest, ann) ->
    let all_patterns = p1 :: p2 :: ps_rest in
    if List.exists (Fun.compose not is_var_or_wildcard) all_patterns then None
    else
      let projs = 
        all_patterns
        |> List.mapi (fun i p -> i,p) 
        |> List.filter_map (fun (i,p) -> 
        match p with 
        | PVar (name, name_ann ) -> Some ((name, name_ann), EUnary (UProj i, scrutinee, ann))
        | PWildcard _ -> None
        | _ -> failwith "compile_simple_pattern NEVER HAPPENS!")
      in
      Some( List.fold_right (fun (name, proj) acc -> ELet (name, proj, acc, ann)) projs (case_body ()))
  | PSigCons (PVar (head, head_ann), tail, ann) ->
    let hd_proj = EApp (EVar (head_elim, ann), [scrutinee], ann) in
    let tl_proj = EUnary (UTail, scrutinee, ann) in
    let body = sink_until_first_use tail tl_proj ann (case_body ()) in
    Some (ELet ((head, head_ann), hd_proj, body, ann))
  | PSigCons (PWildcard _, tail, ann) ->
    let tl_proj = EUnary (UTail, scrutinee, ann) in
    let body = sink_until_first_use tail tl_proj ann (case_body ()) in
    Some (body)
  | _ -> 
    let ann = expr_get_ann scrutinee in
    let location = Ast.get_location ann in
    Location.report_error location "compile_simple_pattern: unexpected non-simple pattern in match"

and compile_string_case scrutinee cases ann =
  let scrutinee = compile_match scrutinee in
  let compile_with scrutinee_expr = compile_string_branches scrutinee_expr cases ann in
  if is_simple_expr scrutinee then
    compile_with scrutinee
  else
    let tmp = (Utilities.new_name "string_match", ann) in
    ELet (tmp, scrutinee, compile_with (parsed_var tmp), ann)

and compile_string_branches scrutinee cases ann =
  match cases with
  | [] -> parsed_app (match_fail, ann) [EConst (CString "Non-exhaustive string match", ann)]
  | (pattern, body, _) :: rest ->
    compile_string_pattern scrutinee pattern (compile_match body) (fun () -> compile_string_branches scrutinee rest ann) ann

and compile_string_pattern scrutinee pattern success next ann =
  match pattern with
  | PWildcard _ -> success
  | PVar (name, name_ann) -> ELet ((name, name_ann), scrutinee, success, ann)
  | PConst (CString s, patt_ann) ->
    EIfe (
      parsed_app (string_eq, ann) [scrutinee; EConst (CString s, patt_ann)],
      success,
      next (),
      ann)
  | PStringCons (head_pat, tail_name, _) ->
    compile_string_cons scrutinee head_pat tail_name success next ann
  | _ -> failwith "Unexpected non-string pattern in compile_string_pattern"

and compile_string_cons scrutinee head_pat tail_name success next ann =
  let head_tmp = (Utilities.new_name "string_head", ann) in
  let bind_tail body = ELet (tail_name, parsed_app (string_tail, ann) [scrutinee], body, ann) in
  let head_body =
    match head_pat with
    | PWildcard _ -> bind_tail success
    | PVar (name, name_ann) ->
      ELet ((name, name_ann), parsed_var head_tmp, bind_tail success, ann)
    | PConst (CString s, patt_ann) ->
      EIfe (
        parsed_app (string_eq, ann) [parsed_var head_tmp; EConst (CString s, patt_ann)],
        bind_tail success,
        next (),
        ann)
    | _ -> failwith "Unexpected non-string head pattern in compile_string_cons"
  in
  EIfe (
    parsed_app (string_is_empty, ann) [scrutinee],
    next (),
    ELet (head_tmp, parsed_app (string_head, ann) [scrutinee], head_body, ann),
    ann)

and compile_match e = 
  match e with
  | EVar _ | EConst _ -> e 
  | ECtor (name, args, ann) -> ECtor (name, List.map compile_match args, ann)
  | ECase (scrutinee, cases, ann) when is_string_case cases ->
    compile_string_case scrutinee cases ann
  | ECase ((EVar _ | EAnno (EVar _,_,_)) as scrutinee, cases, ann) ->
    (* Alternatively, we could just do this AFTER ANF? *)
    let cases_compiled = 
      List.map 
      (fun (pat, case_body, case_ann) -> 
        let compiled = compile_simple_pattern scrutinee (fun () -> compile_match case_body) pat in
        (pat, Option.value compiled ~default:case_body, case_ann)) 
      cases 
    in 
    ECase (scrutinee, cases_compiled, ann)
  | ECase (scrutinee, cases, ann) -> 
    (* this happens before ANF, so expr may be complex *)
    let scrutinee_name = Utilities.new_name "scrut", expr_get_ann scrutinee  in
    let scrutinee_var = EVar scrutinee_name in
    let cases_compiled = 
      List.map 
      (fun (pat, case_body, case_ann) -> 
        let compiled = compile_simple_pattern scrutinee_var (fun () -> compile_match case_body) pat in
        (pat, Option.value compiled ~default:case_body, case_ann)) 
      cases 
    in
    ELet (scrutinee_name, compile_match scrutinee, ECase (scrutinee_var, cases_compiled, ann), ann)
  | ELet (name, e1, e2, ann) -> ELet (name, compile_match e1, compile_match e2, ann) 
  | EApp (e, args, ann) -> EApp (e, List.map compile_match args, ann)
  | EUnary (u, e, ann) -> EUnary (u, compile_match e, ann)
  | ETuple (e1, e2, es, ann) -> ETuple (compile_match e1, compile_match e2, List.map compile_match es, ann)
  | EFun (args, body, ann) -> EFun (args, compile_match body, ann)
  | EBinary (op, e1, e2, ann) -> EBinary (op, compile_match e1, compile_match e2, ann)
  | EIfe (cond, e1, e2, ann) -> EIfe (compile_match cond, compile_match e1, compile_match e2, ann)
  | EAnno (e, t, ann) -> EAnno (compile_match e, t, ann)
