open! Ast
open Collections

let rec subst_program (p: _ program) = 
  p 
  |> List.map (function 
    | TopTypeDef _ as e -> e
    | TopLet(x, rhs, ann) -> TopLet(x, subst StringMap.empty rhs, ann))

and subst (replacement_of : string StringMap.t) (e : 's expr) : 's expr =
  match e with
  | EConst _ -> e
  | EVar (y, ann) when StringMap.mem y replacement_of -> EVar (StringMap.find y replacement_of, ann)
  | EVar _ -> e
  | ELet ((y, y_ann), e1, e2,  ann) when StringMap.mem y replacement_of ->
    let new_y = Utilities.new_name y in
    let e1' = subst replacement_of e1 in
    let e2' = subst (StringMap.add y new_y replacement_of) e2 in
    ELet ((new_y, y_ann), e1', e2', ann)
  | ELet ((y, y_ann), e1, e2, ann) ->
    let replacement_of = StringMap.add y y replacement_of in
    let e1' = subst replacement_of e1 in
    let e2' = subst replacement_of e2 in
    ELet ((y, y_ann), e1', e2', ann)
  | EApp (e1, es, ann) ->
    let e1' = subst replacement_of e1 in
    let es' = List.map (subst replacement_of) es in
    EApp (e1', es', ann)
  | EBinary (op, e1, e2, ann) ->
    let e1' = subst replacement_of e1 in
    let e2' = subst replacement_of e2 in
    EBinary (op, e1', e2', ann)
  | EIfe (e1, e2, e3, ann) ->
    let e1' = subst replacement_of e1 in
    let e2' = subst replacement_of e2 in
    let e3' = subst replacement_of e3 in
    EIfe (e1', e2', e3', ann)
  | EFun (params, e, ann) -> 
    let (params', replacement_of) = 
      List.fold_left
        (fun (sub_params, acc) param -> 
          let create_subst acc n = 
            let sub = if StringMap.mem n acc then Utilities.new_name n else n in
            StringMap.add n sub acc
          in
          let acc = List.fold_left create_subst acc (Core.pattern_bound_vars param) in 
          (subst_pattern acc param :: sub_params, acc)) 
        ([], replacement_of)
        params
      in
    let e' = subst replacement_of e in
    EFun (List.rev params', e', ann)
  | ECtor (name, es, ann) -> 
    let es' = List.map (subst replacement_of) es in
    ECtor (name, es', ann)
  | ECase (e, branches, ann) ->
    (* variables bound by patterns are giving fresh names too - just like let bindings *)
    let e' = subst replacement_of e in
    let branches' = List.map (fun (pat, branch_e, ann) ->
      let pat_vars = Ast.pattern_bound_vars pat in
      let replacement_of = List.fold_left (fun acc var ->
        if StringMap.mem var replacement_of 
        then StringMap.add var (Utilities.new_name var) acc
        else StringMap.add var var acc) replacement_of pat_vars 
      in
      (subst_pattern replacement_of pat, subst replacement_of branch_e, ann)
    ) branches in
    ECase (e', branches', ann)
  | EUnary (op, e, ann) -> EUnary (op, subst replacement_of e, ann)
  | ETuple (e1, e2, es, ann) -> ETuple (subst replacement_of e1, subst replacement_of e2, List.map (subst replacement_of) es, ann)
  | EAnno (e, t, ann) -> EAnno (subst replacement_of e, t, ann)
and subst_pattern (replacement_of : string StringMap.t) (p : 's pattern) : 's pattern =
  match p with
  | PWildcard _ -> p
  | PConst _ -> p
  | PVar (y, ann) when StringMap.mem y replacement_of -> PVar (StringMap.find y replacement_of, ann)
  | PVar _ -> p
  | PSigCons (p1, (y, y_ann), ann) ->
    (* assume whoever called this checked bound vars before - 
       so replacement_of already contains new name if necessary*)
    PSigCons (subst_pattern replacement_of p1, (y, y_ann), ann)
  | PStringCons (p1, (y, y_ann), ann) ->
    PStringCons (subst_pattern replacement_of p1, (y, y_ann), ann)
  | PTuple (p1, p2, ps, ann) ->
    let p1' = subst_pattern replacement_of p1 in
    let p2' = subst_pattern replacement_of p2 in
    let ps' = List.map (subst_pattern replacement_of) ps in
    PTuple (p1', p2', ps', ann)
  | PCtor (name, ps, ann) ->
    let ps' = List.map (subst_pattern replacement_of) ps in
    PCtor (name, ps', ann)