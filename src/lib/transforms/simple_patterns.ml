open! Ast

let head_elim = (Rizzo_builtins.get "head").name

let is_var_or_wildcard = function 
  | PVar _ | PWildcard -> true 
  | _ -> false

let rec transform_patterns (p: 's Ast.program) = 
  List.map (fun (TopLet (name, expr, ann)) -> TopLet (name, compile_match expr, ann)) p
  
and compile_simple_pattern scrutinee case_body = function
  | PVar (s, ann) -> 
    let x = ELet ((s, ann), scrutinee, case_body (), ann) in
    Some (x)
  | PCtor (_, ps, ann) when List.for_all is_var_or_wildcard ps -> 
    let projs = (List.mapi (fun i p -> i,p) ps) |> List.filter_map (fun (i,p) -> 
      match p with 
      | PVar (name, name_ann ) -> Some ((name, name_ann), EUnary (UProj i, scrutinee, ann))
      | PWildcard -> None
      | _ -> failwith "compile_simple_pattern NEVER HAPPENS!")
    in
    Some( List.fold_right (fun (name, proj) acc -> ELet (name, proj, acc, ann)) projs (case_body ()))
  | PTuple (PVar (x, x_ann), PWildcard, ann) -> 
    Some (ELet ((x, x_ann), EUnary (Fst, scrutinee, ann), case_body (), ann))
  | PTuple (PWildcard, PVar (x, x_ann), ann) -> 
    Some (ELet ((x, x_ann), EUnary (Snd, scrutinee, ann), case_body (), ann))
  | PTuple (PVar (left, left_ann), PVar (right, right_ann), ann) -> 
    let t0 = left, left_ann in
    let t1 = right, right_ann in
    Some (
      ELet (t0, EUnary (Fst, scrutinee, ann), 
      ELet (t1, EUnary (Snd, scrutinee, ann), case_body (), ann), ann))
  | PSigCons (PVar (head, head_ann), tail, ann) ->
    let hd_proj = EApp (EVar (head_elim, ann), [scrutinee], ann) in
    let tl_proj = EUnary (UTail, scrutinee, ann) in
    Some (ELet ((head, head_ann), hd_proj, ELet (tail, tl_proj, case_body (), ann), ann))
  | PSigCons (PWildcard, tail, ann) ->
    let tl_proj = EUnary (UTail, scrutinee, ann) in
    Some (ELet (tail, tl_proj, case_body (), ann))
  | _ -> failwith "Only simple patterns - variables !"
and compile_match e = 
  match e with
  | EVar _ | EConst _ -> e 
  | ECtor (name, args, ann) -> ECtor (name, List.map compile_match args, ann)
  | ECase (scrutinee, cases, ann) -> 
    (* this happens before ANF, so expr may be complex *)
    let cases_compiled = 
      List.map 
      (fun (pat, case_body, case_ann) -> 
        let compiled = compile_simple_pattern scrutinee (fun () -> case_body) pat in
        (pat, Option.value compiled ~default:case_body, case_ann)) 
      cases 
    in
    ECase (compile_match scrutinee, cases_compiled, ann)
    (* let s = Utilities.new_name "scrut", expr_get_ann scrutinee in
    ELet(s, compile_match scrutinee, compile_match_cases (EVar s) cases, ann) *)
  | ELet (name, e1, e2, ann) -> ELet (name, compile_match e1, compile_match e2, ann) 
  | EApp (e, args, ann) -> EApp (e, List.map compile_match args, ann)
  | EUnary (u, e, ann) -> EUnary (u, compile_match e, ann)
  | ETuple (e1, e2, ann) -> ETuple (compile_match e1, compile_match e2, ann)
  | EFun (args, body, ann) -> EFun (args, compile_match body, ann)
  | EBinary (op, e1, e2, ann) -> EBinary (op, compile_match e1, compile_match e2, ann)
  | EIfe (cond, e1, e2, ann) -> EIfe (compile_match cond, compile_match e1, compile_match e2, ann)
