open! Ast

type typing_error = Typing_error of Location.t * string
let ( let* ) = Result.bind
let return = Result.ok
let error ann msg = Result.error (Typing_error (get_location ann, msg))
let errorl_from_to (loc_start: Location.t) (loc_end: Location.t) msg = 
  Result.error (Typing_error (Location.mk loc_start.start_pos loc_end.end_pos, msg))

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

let get_typ (e : typed expr) = expr_get_ann e |> ann_get_type

module StringMap = Map.Make(String)
type schema_env = schema StringMap.t (* x : TInt, f : TInt -> TInt *)
type typedefinition_env = typedefinition StringMap.t
type typing_env = { global : schema_env; local : schema_env; typedefinitions: typedefinition_env }
let empty_env = { global = StringMap.empty; local = StringMap.empty; typedefinitions = StringMap.empty }
let typing_env_add_local env name schema = { env with local = StringMap.add name schema env.local }
let typing_env_add_locals env names_and_schemas = 
  { env with local = StringMap.add_seq (List.to_seq names_and_schemas) env.local }

let rec typecheck : type stage. stage program -> (typed program, typing_error) Result.t = fun p -> 
  let initial_acc = (return (empty_env, [])) in
  let* (_, checked_program) = List.fold_left (fun acc (TopLet (name, e, ann)) -> 
    let* env_acc, res_acc = acc in
    let* te = infer env_acc e in 
    let t = get_typ te in
    (* TOOD: Polymorphism? *)
    let env_acc = typing_env_add_local env_acc name (mono t) in
    let toplet_expr = TopLet (name, te, Ann_typed (get_location ann, t)) in
    return (env_acc, toplet_expr :: res_acc)
  ) initial_acc p in
  return checked_program

(** Infers the type of an expr*)
and infer : type stage. typing_env -> stage expr -> (typed expr, typing_error) Result.t = fun env e ->
  match e with
  | EConst (c, ann) -> 
    let* const_type = infer_const_type ann c in
    return (EConst (c, Ann_typed (get_location ann, const_type)))
  | EAnno (e, t, ann) -> 
    let* te = check env e t in
    return (EAnno (te, t, Ann_typed (get_location ann, t)))
  | EVar (name, ann) -> 
    (* TODO: when we do this look up, record that it either came from scope_global or scope_local *)
    let* var_type = 
      match StringMap.find_opt name env.global with
      | Some (Forall ([], t)) -> return t
      | Some (Forall (_, _)) -> error ann "Polymorphic types not supported yet"
      | None -> (
        match StringMap.find_opt name env.local with
        | Some (Forall ([], t)) -> return t
        | Some (Forall (_, _)) -> error ann "Polymorphic types not supported yet"
        | None -> error ann ("Unbound variable: " ^ name)
      )
    in
    return (EVar (name, Ann_typed (get_location ann, var_type)))
  | ETuple (e1, e2, ann) -> 
    let* te1 = infer env e1 in
    let* te2 = infer env e2 in
    let t1 = get_typ te1 in
    let t2 = get_typ te2 in
    return (ETuple (te1, te2, Ann_typed (get_location ann, TTuple (t1, t2))))
  | EBinary (op, e1, e2, ann) -> infer_binary env op e1 e2 ann
  | EApp (f, args, ann) -> 
    let* tf = infer env f in 
    let tf_type = get_typ tf in
    (match tf_type with
    | TFun (param_types, ret_type) -> infer_application env tf param_types ret_type args ann
    | _ -> error (expr_get_ann f) (Format.asprintf "Cannot apply a non-function type '%a'" Ast.pp_typ tf_type))
  | ECase (scrutinee, branches, ann) ->
    let* tscrutinee = infer env scrutinee in
    let t_scrutinee = get_typ tscrutinee in
    let branch_mapper : type s. (s pattern * s expr * s ann) -> (typed pattern * typed expr, typing_error) Result.t = 
      fun (pattern, branch, _) -> check_pattern_and_infer_branch env pattern branch t_scrutinee in
    let* tbranches = result_combine (List.map branch_mapper branches) in
    let branch_types = List.map (fun (_, branch) -> get_typ branch) tbranches in
    (* check that all branches have the same type *)
    (match branch_types with
    | [] -> error ann "Case expression must have at least one branch"
    | t :: ts -> 
      if List.for_all (fun t' -> Ast.eq_typ t' t) ts
      then 
        let typed_branches = 
          List.map2 (fun (_,_, ann) (typed_pattern, typed_body) -> (typed_pattern, typed_body, Ann_typed(get_location ann, get_typ typed_body))) branches tbranches 
        in
        return (ECase (tscrutinee, typed_branches, Ann_typed (get_location ann, t)))
      else error ann "All case branches must have the same type")
  | EFun (_,_, ann) -> error ann "Inference for functions not implemented yet - unification todo"
  | _ -> 
    let msg = (Format.asprintf "Unable to infer type for this expression.\n  Try (%a : T) to check against an expected type T" Ast.pp_expr e) in
    error (expr_get_ann e) msg

(** Checks a type against an expected type *)
and check : type stage. typing_env -> stage expr -> typ -> (typed expr, typing_error) Result.t =
  fun env e expected -> match e with
  | EConst (CNever, ann) -> 
    (match expected with 
    | TLater (_) -> return (EConst (CNever, Ann_typed (get_location ann, expected)))
    | _ -> error ann "Never can only be of type 'Later t' for some t")
  | EFun (params, body, ann) -> 
    let* Cons1(p1_type, param_types_rest), ret_type = match expected with
      | TFun (param_types, ret_type) when Ast_helpers.list1_length param_types = List.length params -> 
        return (param_types, ret_type)
      | TFun (_, _) -> error ann (Format.asprintf "Function type does not match number of parameters which was expected of %a" Ast.pp_typ expected)
      | _ -> error ann (Format.asprintf "Type check expected non-function '%a'" Ast.pp_expr e)
    in
    let params_annotated = List.map2 (fun (pn, pann) pt -> (pn, Ann_typed(get_location pann, pt))) params (p1_type :: param_types_rest) in
    let env' = typing_env_add_locals env (List.map (fun (p, ann) -> (p, mono (ann_get_type ann))) params_annotated) in
    let* typed_body = check env' body ret_type in
    let body_type = get_typ typed_body in
    let new_expected = TFun (Cons1(p1_type, param_types_rest), body_type) in
    return (EFun (params_annotated, typed_body, Ann_typed(get_location ann, new_expected)))
  | _ -> 
    let* te = infer env e in
    let t = get_typ te in
    if Ast.eq_typ expected t then return te
    else error (expr_get_ann e) (Format.asprintf "Type mismatch: expected '%a' but got '%a'" Ast.pp_typ expected Ast.pp_typ t)

and infer_const_type : type stage. stage ann -> const -> (typ, typing_error) Result.t = 
  fun ann -> function
  | CUnit -> return TUnit
  | CInt _ -> return TInt
  | CBool _ -> return TBool
  | CString _ -> return TString
  | CNever -> error ann "Never cannot be inferred (yet) - consider annotating (never : Later T)"

(* and check_pattern_and_infer_branch env pattern rhs scrutinee_type : ((typed pattern * typed expr), typing_error) Result.t = *)
and check_pattern_and_infer_branch : type stage. typing_env -> stage pattern -> stage expr -> typ -> ((typed pattern * typed expr), typing_error) Result.t =
  fun env pattern rhs scrutinee_type -> 
  match pattern with
  | PWildcard -> 
    let* rhs' = infer env rhs in
    return (PWildcard, rhs')
  | PVar (name, ann) ->
    let pattern_type = scrutinee_type in
    let env' = typing_env_add_local env name (mono pattern_type) in  
    let pattern = PVar (name, Ann_typed (get_location ann, pattern_type)) in
    let* rhs' = infer env' rhs in
    return (pattern, rhs')
  | PConst (c, ann) ->
    let* const_type = infer_const_type ann c in
    if const_type = scrutinee_type 
    then let* rhs' = infer env rhs in return (PConst (c, Ann_typed (get_location ann, const_type)), rhs')
    else error ann "Pattern constant type does not match scrutinee type"
  | PTuple (PVar (n1,_) , PVar (n2,_), ann) ->
    let* (t1, t2) = match scrutinee_type with
      | TTuple (t1, t2) -> return (t1, t2)
      | _ -> error ann "Pattern tuple type does not match scrutinee type"
    in
    let env' = typing_env_add_local env n1 (mono t1) in
    let env' = typing_env_add_local env' n2 (mono t2) in 
    let p1 = PVar (n1, Ann_typed (get_location ann, t1)) in
    let p2 = PVar (n2, Ann_typed (get_location ann, t2)) in
    let pattern_type = TTuple (t1, t2) in
    let pattern : typed pattern = PTuple (p1, p2, Ann_typed (get_location ann, pattern_type)) in
    let* rhs' = infer env' rhs in
    return (pattern, rhs')
  | PSigCons (PVar (hd_name, hd_ann), (tail_name, _), ann) ->
    let* t = match scrutinee_type with
      | TSignal t -> return t
      | _ -> error ann "Pattern SigCons type does not match scrutinee type"
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
  | PTuple (_,_,ann) | PSigCons(_,_,ann) -> error ann "Pattern type checking not implemented yet"

and infer_binary : type stage. typing_env -> binary_op -> stage expr -> stage expr -> stage ann -> (typed expr, typing_error) Result.t =
  fun env op e1 e2 ann -> match op with
  | SigCons ->
    let* te1 = infer env e1 in
    let t1 = get_typ te1 in
    let* te2 = check env e2 (TLater (TSignal t1)) in
    return (EBinary (SigCons, te1, te2, Ann_typed (get_location ann, TSignal t1)))
  | Eq ->
    let* te1 = infer env e1 in
    let t1 = get_typ te1 in
    let* te2 = check env e2 t1 in (* check that typeof(e1) == typeof(e2) *)
    return (EBinary (Eq, te1, te2, Ann_typed (get_location ann, TBool)))
  | Lt | Leq ->
    let* te1 = check env e1 TInt in
    let* te2 = check env e2 TInt in
    return (EBinary (op, te1, te2, Ann_typed (get_location ann, TBool)))
  | Add | Mul | Sub | Div ->
    let* te1 = check env e1 TInt in
    let* te2 = check env e2 TInt in
    return (EBinary (op, te1, te2, Ann_typed (get_location ann, TInt)))
  | BSync ->
    let* te1 = infer env e1 in
    let t1 = get_typ te1 in
    let* te2 = infer env e2 in 
    let t2 = get_typ te2 in
    return (EBinary (BSync, te1, te2, Ann_typed (get_location ann, TSync (t1, t2)))) 
  | BOStar ->
    let* te1 = infer env e1 in 
    let* (a,b) = match get_typ te1 with
    (* typeof(te1) : TFun ([t1; t2; t3; t4], t5) 
      which after applying te2 to it should give a function of type TFun ([t2; t3; t4], t5), makes sense I guess *)
    | TDelay (TFun (Cons1(a, []), b)) -> return (a, b)
    | TDelay (TFun (Cons1(a, a' :: rest), b)) -> return (a, TFun(Cons1(a',rest), b))
    | _ -> error (expr_get_ann e1) (Format.asprintf "Expected a delayed function - but got '%a'" Ast.pp_typ (get_typ te1))
    in
    let* te2 = check env e2 (TDelay a) in
    return (EBinary (BOStar, te1, te2, Ann_typed (get_location ann, TDelay b)))
  | BLaterApp ->
    let* te1 = infer env e1 in 
    let* (a,b) = match get_typ te1 with
    | TDelay (TFun (Cons1(a,[]), b)) -> return (a, b)
    | TDelay (TFun (Cons1(a, a' :: rest), b)) -> return (a, TFun(Cons1(a',rest), b))
    | _ -> error (expr_get_ann e1) (Format.asprintf "Expected a delayed function - but got '%a'" Ast.pp_typ (get_typ te1))
    in
    let* te2 = check env e2 (TLater a) in
    return (EBinary (BLaterApp, te1, te2, Ann_typed (get_location ann, TLater b)))

and infer_application : type s. typing_env -> typed expr -> typ list1 -> typ -> s expr list -> s ann -> (typed expr, typing_error) Result.t =
  fun env typed_function_expr param_types ret_type args ann ->
  let Cons1(param_type1, param_types_rest) = param_types in
  if Ast_helpers.list1_length param_types == List.length args 
  then begin
    let* checked = 
      let args_and_expected = (List.combine args (param_type1 :: param_types_rest)) in
      let checked_results = List.map (fun (arg, expected_type) -> check env arg expected_type) args_and_expected in
      result_combine checked_results
    in
    return (EApp (typed_function_expr, checked, Ann_typed (get_location ann, ret_type)))
  end
  else if Ast_helpers.list1_length param_types > List.length args
  then begin 
    (* only a partial application *)
    let check_hd = check env (List.hd args) param_type1 in (* parser can't produce EApp(f, [], ...) *)
    let rec go p_types arg_types acc = match p_types, arg_types with
    | [], _ -> failwith (Printf.sprintf "(%s, %d) oops - mismatching lengths" __FILE__ __LINE__)
    | first :: rest, [] -> return (Cons1(first, rest), List.rev acc)
    | t :: ts, arg :: args -> 
      let check_arg_result = check env arg t in
      go ts args (check_arg_result :: acc)
    in
    let* remaining_params, go_result = go param_types_rest args [check_hd] in
    let* typed_args = result_combine go_result in
    let new_fun_type = TFun (remaining_params, ret_type) in
    return (EApp (typed_function_expr, typed_args, Ann_typed (get_location ann, new_fun_type)))
  end 
  else (* it is now the case that length(param_types) < length(args) ... *)  
      failwith (Printf.sprintf "(%s, %d) TODO! function was overapplied, needs a split?" __FILE__ __LINE__)
