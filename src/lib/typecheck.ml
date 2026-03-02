open! Ast

let ( let* ) = Type_env.Operators.( let* )
let return = Type_env.return
let error ann msg = Type_env.report_error ann msg

(** TODO: what to do - since we cannot explode on first failure *)
let dummy ann = EConst (CUnit, Ann_typed (get_location ann, TError))

let get_typ e = Type_env.get_type e

type scheme = Type_env.scheme
module StringSet = Set.Make(String)
module StringMap = Type_env.StringMap
let forall vars t = Type_env.Forall (vars, t)

(** creates a monomorphic - fully concrete - type of some typ *)
let mono t = forall [] t

(** Generalises a typ -> used for polymorphic let-bindings *)
let rec generalize : typ -> scheme Type_env.t = fun t -> 
  let* env_free = free_type_vars_env () in
  let* t_free = free_type_vars_typ t in
  let generalized_vars = StringSet.diff t_free env_free in
  return (forall (StringSet.elements generalized_vars) t)
and free_type_vars_typ : typ -> StringSet.t Type_env.t = function
  | TError -> return StringSet.empty
  | TVar _ -> return StringSet.empty
  | TTuple (t1, t2) | TSync (t1, t2) -> 
    let* t1_free = free_type_vars_typ t1 in
    let* t2_free = free_type_vars_typ t2 in
    return (StringSet.union t1_free t2_free)
  | TSignal t | TDelay t | TLater t | TOption t | TChan t -> free_type_vars_typ t
  | TUnit | TInt | TBool | TString | TName _ -> return StringSet.empty
  | TParam v -> return (StringSet.singleton v)
  | TFun (Cons1(front, rest), ret_type) -> 
    let* param_free = List.fold_left (fun acc t -> 
        let* acc = acc in 
        let* free_in_t = free_type_vars_typ t in
        return (StringSet.union acc free_in_t)) 
      (return StringSet.empty) 
      (front :: rest) in
    let* ret_free = free_type_vars_typ ret_type in
    return (StringSet.union param_free ret_free)
and free_type_vars_scheme : scheme -> StringSet.t Type_env.t = function
  | Forall (vars, t) -> 
    let* free_in_t = free_type_vars_typ t in
    return (StringSet.diff free_in_t (StringSet.of_list vars))
and free_type_vars_env_scheme: Type_env.scheme_env -> StringSet.t Type_env.t = 
  fun scheme_env -> StringMap.fold (
    fun _ scheme acc -> 
      let* acc = acc in
      let* free_in_scheme = free_type_vars_scheme scheme in
      return (StringSet.union acc free_in_scheme)
    ) scheme_env (return StringSet.empty)
and free_type_vars_env : unit -> StringSet.t Type_env.t = fun () ->
  let* env = Type_env.get_state in
  let* global_free = free_type_vars_env_scheme env.global in
  let* local_free = free_type_vars_env_scheme env.local in
  return (StringSet.union global_free local_free)

let rec typecheck : type stage. stage program -> typed program option = fun p -> 
  let builtins,_ = Type_env.run @@ Type_env.collect (List.map 
    (fun ({name; typ; _}:Rizzo_builtins.builtin_info) -> 
      let* typ_free = free_type_vars_typ typ in
      return (name, forall (StringSet.to_list typ_free) typ)) 
    Rizzo_builtins.builtins)
  in
  let checked_program, env  = Type_env.run (typecheck_program p) ~builtins:(Some builtins) in
  let errors = env.errors in
  List.iter (fun err -> match err with
    | Type_env.Typing_error (loc, msg) -> Location.show_error_context loc msg
  ) errors;
  match errors with
  | [] -> Some checked_program
  | _ -> None

and typecheck_program : type stage. stage program -> typed program Type_env.t = fun p -> 
  let* checked_program = List.fold_left (fun acc (TopLet (name, e, ann)) -> 
    let* acc = acc in
    let* te = match e with
    | EFun (params, _, _) -> 
      let* param_types = Type_env.collect (List.map (fun _ -> Type_env.fresh_type_var ()) params) in
      let* ret_type = Type_env.fresh_type_var () in
      let  t = TFun (Cons1(List.hd param_types, List.tl param_types), ret_type) in
      let* typed_e = Type_env.with_locals [name, mono t] (infer e) in
      let* inferred_t = get_typ typed_e in
      let* _ = Type_env.expected_equal ann inferred_t t in
      return typed_e
    | _ -> Type_env.with_local_scope (infer e)
    in
    let* t = get_typ te in
    let* t = Type_env.generalize_type_vars t in
    let* generalized_type = generalize t in
    let* () = Type_env.add_global name generalized_type in
    let toplet_expr = TopLet (name, te, Ann_typed (get_location ann, t)) in
    return (toplet_expr :: acc)
  ) (return []) p in
  return checked_program

(** Infers the type of an expr*)
and infer : type stage. stage expr -> typed expr Type_env.t = fun e ->
  match e with
  | EConst (c, ann) -> 
    let* const_type = infer_const_type ann c in
    return (EConst (c, Ann_typed (get_location ann, const_type)))
  | EAnno (e, t, ann) -> 
    let* te = check e t in
    return (EAnno (te, t, Ann_typed (get_location ann, t)))
  | ELet ((name, name_ann), rhs, body, ann) ->
    let* trhs = infer rhs in
    let* t_rhs = get_typ trhs in
    let* t_rhs = Type_env.generalize_type_vars t_rhs in
    let* t_rhs_generalized = generalize t_rhs in
    let* tbody = Type_env.with_locals [name, t_rhs_generalized] (infer body) in
    let* tbody_type = get_typ tbody in
    let name' = (name, Ann_typed (get_location name_ann, t_rhs)) in
    return (ELet (name', trhs, tbody, Ann_typed (get_location ann, tbody_type)))
  | EIfe (cond, e1, e2, ann) ->
    let* tcond = check cond TBool in
    let* te1 = infer e1 in
    let* te2 = infer e2 in
    let* t1 = get_typ te1 in
    let* t2 = get_typ te2 in
    let* _ = Type_env.expected_equal ann t1 t2 in
    let* t = Type_env.apply_subst t1 in
    return (EIfe (tcond, te1, te2, Ann_typed (get_location ann, t)))
  | EVar (name, ann) -> 
    (* TODO: when we do this look up, record that it either came from scope_global or scope_local *)
    let* env = Type_env.get_state in
    let* var_type = 
      match StringMap.find_opt name env.global with
      | Some scheme ->
        Type_env.instantiate_scheme scheme
      | None -> (
        match StringMap.find_opt name env.local with
        | Some scheme -> Type_env.instantiate_scheme scheme
        | None -> 
          let* _ = error ann ("Unbound variable: " ^ name) in
          return TError)
    in
    return (EVar (name, Ann_typed (get_location ann, var_type)))
  | ETuple (e1, e2, ann) -> 
    let* te1 = infer e1 in
    let* te2 = infer e2 in
    let*t1 = get_typ te1 in
    let* t2 = get_typ te2 in
    return (ETuple (te1, te2, Ann_typed (get_location ann, TTuple (t1, t2))))
  | EBinary (op, e1, e2, ann) -> infer_binary op e1 e2 ann
  | EUnary (op, e, ann) -> infer_unary op e ann
  | EApp (f, args, ann) -> 
    let* inferred_args = Type_env.collect (List.map infer args) in
    let* arg_types = Type_env.collect (List.map get_typ inferred_args) in
    let* ret_type = Type_env.fresh_type_var () in
    let expected_fun_type = TFun (Cons1(List.hd arg_types, List.tl arg_types), ret_type) in
    let* tf = check f expected_fun_type in
    let* ret_type = Type_env.apply_subst ret_type in
    return (EApp (tf, inferred_args, Ann_typed (get_location ann, ret_type)))
  | ECase (scrutinee, branches, ann) ->
    let* tscrutinee = infer scrutinee in
    let* t_scrutinee = get_typ tscrutinee in
    let branch_mapper : type s. (s pattern * s expr * s ann) -> (typed pattern * typed expr) Type_env.t = 
      fun (pattern, branch, _) -> check_pattern_and_infer_branch pattern branch t_scrutinee in
    let* tbranches = Type_env.collect (List.map branch_mapper branches) in
    let* branch_types = Type_env.collect (List.map (fun (_, branch) -> get_typ branch) tbranches) in
    (* check that all branches have the same type *)
    (match branch_types with
    | [] -> 
      let* _ = error ann "Case expression must have at least one branch" in
      return (ECase (tscrutinee, [], Ann_typed (get_location ann, TError)))
    | t :: ts -> 
      if List.for_all (fun t' -> Ast.eq_typ t' t) ts
      then 
        let* typed_branches = 
          Type_env.collect @@
          List.map2 (fun (_,_, ann) (typed_pattern, typed_body) -> 
            let* body_type = get_typ typed_body in
            return (typed_pattern, typed_body, Ann_typed(get_location ann, body_type))) branches tbranches 
        in
        return (ECase (tscrutinee, typed_branches, Ann_typed (get_location ann, t)))
      else 
        let* _ = error ann "All case branches must have the same type" in 
        return (ECase (tscrutinee, [], Ann_typed (get_location ann, TError))))
  | EFun (param_names, body, ann) -> 
    let* param_types = Type_env.collect @@ List.map (fun (p, _) -> 
      let* pt = Type_env.fresh_type_var () in
      return (p, mono pt)
    ) param_names 
    in
    let* typed_body = Type_env.with_locals param_types (infer body) in
    let* body_type = get_typ typed_body in
    let* param_types = Type_env.collect (List.map (fun (_, Type_env.Forall (_, t)) -> Type_env.apply_subst t) param_types) in
    let fun_type = TFun (Cons1(List.hd param_types, List.tl param_types), body_type) in
    let param_names = List.map2 (fun (p, pann) pt -> (p, Ann_typed(get_location pann, pt))) param_names param_types in
    return (EFun (param_names, typed_body, Ann_typed(get_location ann, fun_type)))
  | _ -> 
    let msg = (Format.asprintf "Unable to infer type for this expression.\n  Try (%a : T) to check against an expected type T" Ast.pp_expr e) in
    let* _ = error (expr_get_ann e) msg in
    (* TODO: What to do when everything fails? *)
    return (dummy (expr_get_ann e))

(** Checks a type against an expected type *)
and check : type stage. stage expr -> typ -> typed expr Type_env.t =
  fun e expected -> match e with
  | EConst (CNever, ann) -> 
    let* inner = Type_env.fresh_type_var () in 
    let* _ = Type_env.expected_equal ann expected (TLater inner) in
    let* t = Type_env.find expected in
    return (EConst (CNever, Ann_typed (get_location ann, t)))
  | ELet ((name, name_ann), rhs, body, ann) ->
    let* trhs = infer rhs in
    let* t_rhs = get_typ trhs in
    let* t_rhs = Type_env.generalize_type_vars t_rhs in
    let* rhs_gen = generalize t_rhs in
    let* tbody = Type_env.with_local name rhs_gen (check body expected) in
    let* tbody_type = get_typ tbody in
    let name' = (name, Ann_typed (get_location name_ann, t_rhs)) in
    return (ELet (name', trhs, tbody, Ann_typed (get_location ann, tbody_type)))
  | EFun (params, body, ann) -> 
    let* Cons1(p1_type, param_types_rest), ret_type = 
      match expected with
      | TFun (param_types, ret_type) when Ast_helpers.list1_length param_types = List.length params -> 
        return (param_types, ret_type)
      | TFun (ps, _) -> 
        let ps_len = Ast_helpers.list1_length ps in
        let* _ = error ann (Format.asprintf "Function has %d parameters but expected type '%a'" ps_len Ast.pp_typ expected) in
        return (Cons1(TError, List.init (List.length params - 1) (fun _ -> TError)), TError)
      | _ -> 
        let* _ = error ann (Format.asprintf "Type check expected non-function '%a'" Ast.pp_expr e) in
        return (Cons1(TError, List.init (List.length params - 1) (fun _ -> TError)), TError)
    in
    let params_annotated = List.map2 (fun (pn, pann) pt -> (pn, Ann_typed(get_location pann, pt))) params (p1_type :: param_types_rest) in
    let param_bindings = (List.map (fun (p, ann) -> (p, mono (ann_get_type ann))) params_annotated) in
    let* typed_body = Type_env.with_locals param_bindings (check body ret_type) in
    let* body_type = get_typ typed_body in
    let new_expected = TFun (Cons1(p1_type, param_types_rest), body_type) in
    return (EFun (params_annotated, typed_body, Ann_typed(get_location ann, new_expected)))
  | _ -> 
    let* te = infer e in
    let* te_type = get_typ te in
    let* _ = Type_env.expected_equal (expr_get_ann e) expected te_type in
    return te

and infer_const_type : type stage. stage ann -> const -> typ Type_env.t = 
  fun ann -> function
  | CUnit -> return TUnit
  | CInt _ -> return TInt
  | CBool _ -> return TBool
  | CString _ -> return TString
  | CNever -> 
    let* _ = error ann "Never cannot be inferred (yet) - consider annotating (never : Later T)" in
    return TError

and check_pattern_and_infer_branch : type stage. stage pattern -> stage expr -> typ -> (typed pattern * typed expr) Type_env.t =
  fun pattern rhs scrutinee_type -> 
  match pattern with
  | PWildcard -> 
    let* rhs' = infer rhs in
    return (PWildcard, rhs')
  | PVar (name, ann) ->
    let pattern_type = scrutinee_type in
    let* rhs' = Type_env.with_local name (mono pattern_type) (infer rhs) in
    let pattern = PVar (name, Ann_typed (get_location ann, pattern_type)) in
    return (pattern, rhs')
  | PConst (c, ann) ->
    let* const_type = infer_const_type ann c in
    if const_type = scrutinee_type 
    then let* rhs' = infer rhs in return (PConst (c, Ann_typed (get_location ann, const_type)), rhs')
    else 
      let* _ = error ann "Pattern constant type does not match scrutinee type" in
      return (PConst (c, Ann_typed (get_location ann, TError)), dummy ann)
  | PTuple (PVar (n1,_) , PVar (n2,_), ann) ->
    let* (t1, t2) = match scrutinee_type with
      | TTuple (t1, t2) -> return (t1, t2)
      | _ -> 
        let* _ = error ann "Pattern tuple type does not match scrutinee type" in
        return (TError, TError)
    in
    let* rhs' = Type_env.with_locals [(n1, mono t1); (n2, mono t2)] (infer rhs) in
    let p1 = PVar (n1, Ann_typed (get_location ann, t1)) in
    let p2 = PVar (n2, Ann_typed (get_location ann, t2)) in
    let pattern_type = TTuple (t1, t2) in
    let pattern = PTuple (p1, p2, Ann_typed (get_location ann, pattern_type)) in
    return (pattern, rhs')
  | PSigCons (PVar (hd_name, hd_ann), (tail_name, _), ann) ->
    let* a = Type_env.fresh_type_var () in
    let* _ = Type_env.expected_equal ann scrutinee_type (TSignal a) in
    let* t = Type_env.apply_subst a in

    let* rhs' = Type_env.with_locals [(hd_name, mono t); (tail_name, mono (TLater (TSignal t)))] (infer rhs) in
    let pattern_type = TSignal t in
    let hd_pattern = PVar (hd_name, Ann_typed (get_location hd_ann, t)) in
    let tail_name = (tail_name, Ann_typed (get_location ann, TLater (TSignal t))) in
    let pattern = PSigCons (hd_pattern, tail_name, Ann_typed (get_location ann, pattern_type)) in
    return (pattern, rhs')
  | PCtor (_,_, ann) -> 
    let* _ = error ann "saving ctor patterns for later" in
    return (PWildcard, dummy ann)
  | PTuple (_,_,ann) | PSigCons(_,_,ann) -> 
    let* _ = error ann "Pattern type checking not implemented yet" in
    (* TODO: what to do when it all goes wrong? *)
    return (PWildcard, dummy ann)

and infer_binary : type stage. binary_op -> stage expr -> stage expr -> stage ann -> typed expr Type_env.t =
  fun op e1 e2 ann -> match op with
  | SigCons ->
    let* te1 = infer e1 in
    let* t1 = get_typ te1 in
    let* te2 = check e2 (TLater (TSignal t1)) in
    return (EBinary (SigCons, te1, te2, Ann_typed (get_location ann, TSignal t1)))
  | Eq ->
    let* te1 = infer e1 in
    let* t1 = get_typ te1 in
    let* te2 = check e2 t1 in (* check that typeof(e1) == typeof(e2) *)
    return (EBinary (Eq, te1, te2, Ann_typed (get_location ann, TBool)))
  | Lt | Leq ->
    let* te1 = check e1 TInt in
    let* te2 = check e2 TInt in
    return (EBinary (op, te1, te2, Ann_typed (get_location ann, TBool)))
  | Add | Mul | Sub | Div ->
    let* te1 = check e1 TInt in
    let* te2 = check e2 TInt in
    return (EBinary (op, te1, te2, Ann_typed (get_location ann, TInt)))
  | BSync ->
    let* te1 = infer e1 in
    let* t1 = get_typ te1 in
    let* te2 = infer e2 in 
    let* t2 = get_typ te2 in
    return (EBinary (BSync, te1, te2, Ann_typed (get_location ann, TLater (TSync (t1, t2))))) 
  | BOStar ->
    let* a = Type_env.fresh_type_var () in
    let* b = Type_env.fresh_type_var () in
    let expected_shape = TDelay (TFun (Cons1(a, []), b)) in
    let* te1 = check e1 expected_shape in
    let* te2 = check e2 (TDelay a) in
    return (EBinary (BOStar, te1, te2, Ann_typed (get_location ann, TDelay b)))
  | BLaterApp -> 
    let* a = Type_env.fresh_type_var () in
    let* b = Type_env.fresh_type_var () in
    let expected_shape = TDelay (TFun (Cons1(a, []), b)) in
    let* te1 = check e1 expected_shape in
    let* te2 = check e2 (TLater a) in
    let* b = Type_env.apply_subst b in
    return (EBinary (BLaterApp, te1, te2, Ann_typed (get_location ann, TLater b)))

and infer_unary : type s. Ast.unary_op -> s expr -> s ann -> typed expr Type_env.t = fun op e ann ->
  match op with
  | UDelay ->
    let* te = infer e in
    let* t = get_typ te in
    return (EUnary (UDelay, te, Ann_typed (get_location ann, TDelay t)))
  | UWatch -> 
    let* fresh_t = Type_env.fresh_type_var () in
    let* te = check e (TSignal (TOption (fresh_t))) in (* Signal (Option A) *)
    let* t = Type_env.apply_subst fresh_t in
    return (EUnary (UWatch, te, Ann_typed (get_location ann, TLater t)))
  | UTail ->
    let* fresh_t = Type_env.fresh_type_var () in
    let* te = check e (TSignal fresh_t) in (* Signal A *)
    let* t = get_typ te in
    return (EUnary (UTail, te, Ann_typed (get_location ann, TLater t)))
  | UWait -> 
    let* a = Type_env.fresh_type_var () in
    let* te = check e (TChan a) in
    let* t = Type_env.apply_subst a in
    return (EUnary (UWait, te, Ann_typed (get_location ann, TLater t)))
  | UProj i -> 
    let* te = infer e in
    let* t = get_typ te in
    match t with 
    | TError -> return (EUnary (UProj i, te, Ann_typed (get_location ann, TError))) (* stop cascading errors *)
    | TString | TUnit | TInt | TBool | TName _ | TParam _ | TVar _| TFun _ | TChan _ -> 
      let* _ = error ann (Format.asprintf "Cannot project from non-constructor '%a'" Ast.pp_typ t) in
      return (EUnary (UProj i, te, Ann_typed (get_location ann, TError)))
    | TLater _ -> 
      let* _ = error ann (Format.asprintf "Cannot project a LATER! '%a'" Ast.pp_typ t) in
      return (EUnary (UProj i, te, Ann_typed (get_location ann, TError)))
    | TDelay _ ->
      let* _ = error ann (Format.asprintf "Cannot project a DELAY! '%a'" Ast.pp_typ t) in
      return (EUnary (UProj i, te, Ann_typed (get_location ann, TError)))
    | TSignal t' -> 
      (match i with
      | 0 -> return (EUnary (UProj i, te, Ann_typed (get_location ann, t')))
      | 1 -> return (EUnary (UProj i, te, Ann_typed (get_location ann, TLater t)))
      | _ -> 
        let* _ = error ann "Signal only has 2 projections: 0 for head and 1 for tail" in
        return (EUnary (UProj i, te, Ann_typed (get_location ann, TError))))
    | TOption t -> 
      (match i with
      | 0 -> return (EUnary (UProj i, te, Ann_typed (get_location ann, t))) 
      | _ -> 
        let* _ = error ann "Option has at most one projection" in
        return (EUnary (UProj i, te, Ann_typed (get_location ann, TError))))
    | TTuple (t1, t2) -> 
      (match i with
      | 0 -> return (EUnary (UProj i, te, Ann_typed (get_location ann, t1))) 
      | 1 -> return (EUnary (UProj i, te, Ann_typed (get_location ann, t2))) 
      | _ -> 
        let* _ = error ann "Tuple only has 2 projections: 0 for first and 1 for second" in
        return (EUnary (UProj i, te, Ann_typed (get_location ann, TError))))
    | TSync _ -> 
      let* _ = error ann "Cannot project a sync?" in
      return (EUnary (UProj i, te, Ann_typed (get_location ann, TError)))
