open! Ast

type typing_error = Typing_error of string
let ( let* ) = Result.bind
let return = Result.ok
let error msg = Result.error (Typing_error msg)

let result_combine (rs : ('a,'b) result list) : ('a list, 'b) result =
  let mapper r acc =
    let* acc' = acc in
    let* r' = r in
    return (r' :: acc')
  in
  List.fold_right mapper rs (return [])

  
(* type typ = typ  <- exists in Ast.ml *)
(* type a b . Tuple[a,b] *)
type schema = Forall of string list * typ
type typedefinition = typ list

let mono t = Forall ([], t)

let get_typ (e : typed expr) = 
  match (expr_get_ann e : typed ann) with
  | Ann_typed (_, t) -> t
  | _ -> failwith "This should never happen, expected a typed ann"

module StringMap = Map.Make(String)
type schema_env = schema StringMap.t (* x : TInt, f : TInt -> TInt *)
type typedefinition_env = typedefinition StringMap.t
type typing_env = { global : schema_env; local : schema_env; typedefinitions: typedefinition_env }
let empty_env = { global = StringMap.empty; local = StringMap.empty; typedefinitions = StringMap.empty }

let rec typecheck : type stage. stage program -> (typed program, typing_error) Result.t =
  fun p -> 
  let results = List.map (fun (TopLet (name, e, ann)) -> 
    let* te = infer empty_env e in 
    return (TopLet (name, te, Ann_typed (get_location ann, get_typ te)))
  ) p in
  result_combine results

and infer : type stage. typing_env -> stage expr -> (typed expr, typing_error) Result.t = fun env e ->
  match e with
  | EConst (c, ann) -> 
    let* const_type = infer_const_type c in
    return (EConst (c, Ann_typed (get_location ann, const_type)))
  | EVar (name, ann) -> 
    let* var_type = 
      match StringMap.find_opt name env.global with
      | Some (Forall ([], t)) -> return t
      | Some (Forall (_, _)) -> error "Polymorphic types not supported yet"
      | None -> (
        match StringMap.find_opt name env.local with
        | Some (Forall ([], t)) -> return t
        | Some (Forall (_, _)) -> error "Polymorphic types not supported yet"
        | None -> error ("Unbound variable: " ^ name)
      )
    in
    return (EVar (name, Ann_typed (get_location ann, var_type)))
  | ETuple (e1, e2, ann) -> 
    let* te1 = infer env e1 in
    let* te2 = infer env e2 in
    let t1 = get_typ te1 in
    let t2 = get_typ te2 in
    return (ETuple (te1, te2, Ann_typed (get_location ann, TTuple (t1, t2))))
  | EBinary (SigCons, e1, e2, ann) ->
    let* te1 = infer env e1 in
    let t1 = get_typ te1 in
    let* te2 = check env e2 (TLater (TSignal t1)) in
    return (EBinary (SigCons, te1, te2, Ann_typed (get_location ann, TSignal t1)))
  | EBinary (Eq, e1, e2, ann) ->
    let* te1 = infer env e1 in
    let t1 = get_typ te1 in
    let* te2 = check env e2 t1 in (* check that typeof(e1) == typeof(e2) *)
    return (EBinary (Eq, te1, te2, Ann_typed (get_location ann, TBool)))
  | EBinary (Lt | Leq as op, e1, e2, ann) ->
    let* te1 = check env e1 TInt in
    let* te2 = check env e2 TInt in
    return (EBinary (op, te1, te2, Ann_typed (get_location ann, TBool)))
  | EBinary (Add | Mul | Sub | Div as op, e1, e2, ann) ->
    let* te1 = check env e1 TInt in
    let* te2 = check env e2 TInt in
    return (EBinary (op, te1, te2, Ann_typed (get_location ann, TInt)))
    (* | BSync | BOStar | BLaterApp *)
  | EBinary (BSync, e1, e2, ann) ->
    let* te1 = infer env e1 in
    let t1 = get_typ te1 in
    let* te2 = infer env e2 in 
    let t2 = get_typ te2 in
    return (EBinary (BSync, te1, te2, Ann_typed (get_location ann, TSync (t1, t2)))) 
  | EBinary (BOStar, e1, e2, ann) ->
    let* te1 = infer env e1 in 
    let* (a,b) = match get_typ te1 with
    (* typeof(te1) : TFun ([t1; t2; t3; t4], t5) 
      which after applying te2 to it should give a function of type TFun ([t2; t3; t4], t5), makes sense I guess *)
    | TDelay (TFun (Cons1(a, []), b)) -> return (a, b)
    | TDelay (TFun (Cons1(a, a' :: rest), b)) -> return (a, TFun(Cons1(a',rest), b))
    | _ -> error "not too sure here"
    in
    let* te2 = check env e2 (TDelay a) in
    return (EBinary (BOStar, te1, te2, Ann_typed (get_location ann, TDelay b)))
  | EBinary (BLaterApp, e1, e2, ann) ->
    let* te1 = infer env e1 in 
    let* (a,b) = match get_typ te1 with
    | TDelay (TFun (Cons1(a,[]), b)) -> return (a, b)
    | TDelay (TFun (Cons1(a, a' :: rest), b)) -> return (a, TFun(Cons1(a',rest), b))
    | _ -> error "not too sure here"
    in
    let* te2 = check env e2 (TLater a) in
    return (EBinary (BLaterApp, te1, te2, Ann_typed (get_location ann, TLater b)))
  | ECase (scrutinee, branches, ann) ->
    let* tscrutinee = infer env scrutinee in
    let t_scrutinee = get_typ tscrutinee in
    let branch_mapper : type s. (s pattern * s expr * s ann) -> (typed pattern * typed expr, typing_error) Result.t = 
      fun (pattern, branch, _) -> check_pattern_and_infer_branch env pattern branch t_scrutinee in
    let* tbranches = result_combine (List.map branch_mapper branches) in
    let branch_types = List.map (fun (_, branch) -> get_typ branch) tbranches in
    (* check that all branches have the same type *)
    (match branch_types with
    | [] -> error "Case expression must have at least one branch"
    | t :: ts -> 
      if List.for_all (fun t' -> t' = t) ts
      then 
        let typed_branches = 
          List.map2 (fun (_,_, ann) (typed_pattern, typed_body) -> (typed_pattern, typed_body, Ann_typed(get_location ann, get_typ typed_body))) branches tbranches 
        in
        return (ECase (tscrutinee, typed_branches, Ann_typed (get_location ann, t)))
      else error "All case branches must have the same type")
  | EFun _ ->
    failwith "typechecking for functions not implemented yet"
  | _ -> error "todo!"
  

and infer_const_type : const -> (typ, typing_error) Result.t = function
  | CUnit -> return TUnit
  | CInt _ -> return TInt
  | CBool _ -> return TBool
  | CString _ -> return TString
  | CNever -> error "Never cannot be inferred?"

(* and check_pattern_and_infer_branch env pattern rhs scrutinee_type : ((typed pattern * typed expr), typing_error) Result.t = *)
and check_pattern_and_infer_branch : type stage. typing_env -> stage pattern -> stage expr -> typ -> ((typed pattern * typed expr), typing_error) Result.t =
  fun env pattern rhs scrutinee_type -> 
  match pattern with
  | PWildcard -> 
    let* rhs' = infer env rhs in
    return (PWildcard, rhs')
  | PVar (name, ann) ->
    let pattern_type = scrutinee_type in
    let env' = { env with local = StringMap.add name (mono pattern_type) env.local } in
    let pattern = PVar (name, Ann_typed (get_location ann, pattern_type)) in
    let* rhs' = infer env' rhs in
    return (pattern, rhs')
  | PConst (c, ann) ->
    let* const_type = infer_const_type c in
    if const_type = scrutinee_type 
    then let* rhs' = infer env rhs in return (PConst (c, Ann_typed (get_location ann, const_type)), rhs')
    else error "Pattern constant type does not match scrutinee type"
  | PTuple (PVar (n1,_) , PVar (n2,_), ann) ->
    let* (t1, t2) = match scrutinee_type with
      | TTuple (t1, t2) -> return (t1, t2)
      | _ -> error "Pattern tuple type does not match scrutinee type"
    in
    let env' = { env with local = StringMap.add n1 (mono t1) env.local } in
    let env' = { env' with local = StringMap.add n2 (mono t2) env'.local } in 
    let p1 = PVar (n1, Ann_typed (get_location ann, t1)) in
    let p2 = PVar (n2, Ann_typed (get_location ann, t2)) in
    let pattern_type = TTuple (t1, t2) in
    let pattern : typed pattern = PTuple (p1, p2, Ann_typed (get_location ann, pattern_type)) in
    let* rhs' = infer env' rhs in
    return (pattern, rhs')
  | PSigCons (PVar (hd_name, hd_ann), (tail_name, _), ann) ->
    let* t = match scrutinee_type with
      | TSignal t -> return t
      | _ -> error "Pattern SigCons type does not match scrutinee type"
    in
    let env' = { env with local = StringMap.add hd_name (mono t) env.local } in 
    let env' = { env' with local = StringMap.add tail_name (mono (TLater (TSignal t))) env'.local } in
    let* rhs' = infer env' rhs in
    let pattern_type = TSignal t in
    let hd_pattern = PVar (hd_name, Ann_typed (get_location hd_ann, t)) in
    let tail_name = (tail_name, Ann_typed (get_location ann, TSignal t)) in
    let pattern = PSigCons (hd_pattern, tail_name, Ann_typed (get_location ann, pattern_type)) in
    return (pattern, rhs')
  | PCtor _ -> failwith "saving ctor patterns for later"
  | _ -> error "Pattern type checking not implemented yet"

and check : type stage. typing_env -> stage expr -> typ -> (typed expr, typing_error) Result.t =
  fun _ _ _ -> failwith ""
