open! Ast

let ( let* ) = Type_env.Operators.( let* )
let return = Type_env.return
let error ann msg = Type_env.report_error ann msg

(** TODO: what to do - since we cannot explode on first failure *)
let dummy ann = EConst (CUnit, Ann_typed (get_location ann, TError))

let get_typ e = Type_env.get_type e

type scheme = Type_env.scheme
module StringSet = Set.Make(String)
module IntSet = Set.Make(Int)
module StringMap = Type_env.StringMap
let forall vars t = Type_env.Forall (vars, t)

(** creates a monomorphic - fully concrete - type of some typ *)
let mono t = forall [] t

(** Generalises a typ -> used for polymorphic let-bindings *)
let rec generalize : typ -> scheme Type_env.t = fun t ->
  let* t = Type_env.apply_subst t in
  let* env_tvars = free_tvar_ids_env () in
  let* env_params = free_type_vars_env () in
  let* t_tvars = free_tvar_ids_typ t in
  let* t_params = free_type_vars_typ t in
  let generalized_tvars = IntSet.diff t_tvars env_tvars in
  let generalized_params = StringSet.diff t_params env_params in
  let id_to_name = ref Type_env.IntMap.empty in
  let used_param_names = ref t_params in
  let name_index = ref 0 in
  let fresh_param_name id =
    match Type_env.IntMap.find_opt id !id_to_name with
    | Some name -> name
    | None ->
      let rec fresh_name () =
        incr name_index;
        let name = "'inferred" ^ string_of_int !name_index in
        if StringSet.mem name !used_param_names
        then fresh_name ()
        else name
      in
      let name = fresh_name () in
      used_param_names := StringSet.add name !used_param_names;
      id_to_name := Type_env.IntMap.add id name !id_to_name;
      name
  in
  let rec replace_generalized_tvars : typ -> typ Type_env.t = function
    | TError -> return TError
    | TUnit -> return TUnit
    | TInt -> return TInt
    | TString -> return TString
    | TBool -> return TBool
    | TName n -> return (TName n)
    | TParam p -> return (TParam p)
    | TVar id ->
      if IntSet.mem id generalized_tvars
      then return (TParam (fresh_param_name id))
      else return (TVar id)
    | TSignal t -> let* t = replace_generalized_tvars t in return (TSignal t)
    | TLater t -> let* t = replace_generalized_tvars t in return (TLater t)
    | TDelay t -> let* t = replace_generalized_tvars t in return (TDelay t)
    | TOption t -> let* t = replace_generalized_tvars t in return (TOption t)
    | TChan t -> let* t = replace_generalized_tvars t in return (TChan t)
    | TTuple (t1, t2) ->
      let* t1 = replace_generalized_tvars t1 in
      let* t2 = replace_generalized_tvars t2 in
      return (TTuple (t1, t2))
    | TSync (t1, t2) ->
      let* t1 = replace_generalized_tvars t1 in
      let* t2 = replace_generalized_tvars t2 in
      return (TSync (t1, t2))
    | TFun (Cons1 (front, rest), ret) ->
      let* params = Type_env.collect (List.map replace_generalized_tvars (front :: rest)) in
      let* ret = replace_generalized_tvars ret in
      return (TFun (Cons1 (List.hd params, List.tl params), ret))
  in
  let* t = replace_generalized_tvars t in
  let generalized_vars =
    Type_env.IntMap.bindings !id_to_name
    |> List.map snd
    |> StringSet.of_list
    |> StringSet.union generalized_params
    |> StringSet.elements
  in
  return (forall generalized_vars t)
and free_tvar_ids_typ : typ -> IntSet.t Type_env.t = fun t ->
  let* t = Type_env.apply_subst t in
  match t with
  | TError -> return IntSet.empty
  | TVar id -> return (IntSet.singleton id)
  | TTuple (t1, t2) | TSync (t1, t2) ->
    let* t1_free = free_tvar_ids_typ t1 in
    let* t2_free = free_tvar_ids_typ t2 in
    return (IntSet.union t1_free t2_free)
  | TSignal t | TDelay t | TLater t | TOption t | TChan t -> free_tvar_ids_typ t
  | TUnit | TInt | TBool | TString | TName _ | TParam _ -> return IntSet.empty
  | TFun (Cons1 (front, rest), ret_type) ->
    let* param_free =
      List.fold_left
        (fun acc t ->
          let* acc = acc in
          let* free_in_t = free_tvar_ids_typ t in
          return (IntSet.union acc free_in_t))
        (return IntSet.empty)
        (front :: rest)
    in
    let* ret_free = free_tvar_ids_typ ret_type in
    return (IntSet.union param_free ret_free)
and free_tvar_ids_scheme : scheme -> IntSet.t Type_env.t = function
  | Forall (_, t) -> free_tvar_ids_typ t
and free_tvar_ids_env_scheme : Type_env.scheme_env -> IntSet.t Type_env.t =
  fun scheme_env ->
    StringMap.fold
      (fun _ scheme acc ->
        let* acc = acc in
        let* free_in_scheme = free_tvar_ids_scheme scheme in
        return (IntSet.union acc free_in_scheme))
      scheme_env
      (return IntSet.empty)
and free_tvar_ids_env : unit -> IntSet.t Type_env.t = fun () ->
  let* env = Type_env.get_state in
  let* global_free = free_tvar_ids_env_scheme env.global in
  let* local_free = free_tvar_ids_env_scheme env.local in
  return (IntSet.union global_free local_free)
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

(*
* Typechecks a program, returns a typed program and a boolean indicating whether there were any type errors (TODO: this is a bit of a hack - we should probably return the errors instead of printing them and returning a bool)
*)
let rec typecheck : type stage. stage program -> typed program * ((Location.t * string) list) = fun p -> 
  let builtins,_ = Type_env.run @@ Type_env.collect (List.map 
    (fun ({name; typ; _}:Rizzo_builtins.builtin_info) -> 
      let* typ_free = free_type_vars_typ typ in
      return (name, forall (StringSet.to_list typ_free) typ)) 
    Rizzo_builtins.builtins) in
  let checked_program, env  = Type_env.run (typecheck_program p) ~builtins:(Some builtins) in
  let errors = List.map (function | Type_env.Typing_error (loc, err) -> (loc, err)) env.errors in
  (checked_program, errors)

and run_type_env_from_state (state : Type_env.typing_state) (m : 'a Type_env.t) =
  let Type_env.Env run = m in
  run state

and probe_string_case_on_unresolved_scrutinee
    : type stage.
      stage ann ->
      typ ->
      stage case_branch list ->
      bool Type_env.t =
  fun ann unresolved_scrutinee branches ->
  let* state = Type_env.get_state in
  let base_error_count = List.length state.errors in
  let probe : unit Type_env.t =
    let* _ = Type_env.expected_equal ann unresolved_scrutinee TString in
    let* probe_scrutinee = Type_env.apply_subst unresolved_scrutinee in
    let branch_mapper : type s. (s pattern * s expr * s ann) -> (typed pattern * typed expr) Type_env.t =
      fun (pattern, branch, _) -> check_pattern_and_infer_branch pattern branch probe_scrutinee
    in
    let* typed_branches = Type_env.collect (List.map branch_mapper branches) in
    let* branch_types = Type_env.collect (List.map (fun (_, branch) -> get_typ branch) typed_branches) in
    match branch_types with
    | [] -> return ()
    | branch_type :: _ ->
      let* _ = Type_env.collect (List.map (fun other_type -> Type_env.expected_equal ann branch_type other_type) branch_types) in
      return ()
  in
  let _, probe_state = run_type_env_from_state state probe in
  return (List.length probe_state.errors = base_error_count)

and typecheck_program : type stage. stage program -> typed program Type_env.t = fun p -> 
  let* checked_program = List.fold_left (fun acc (TopLet (name, e, ann)) -> 
    let* acc = acc in
    let name_text = fst name in
    let* te = match e with
    | EFun (params, _, _) | EAnno (EFun (params, _, _), _, _)-> 
      let* param_types = Type_env.collect (List.map (fun _ -> Type_env.fresh_type_var ()) params) in
      let* ret_type = Type_env.fresh_type_var () in
      let  t = TFun (Cons1(List.hd param_types, List.tl param_types), ret_type) in
      let* typed_e = Type_env.with_locals [name_text, mono t] (infer e) in
      let* inferred_t = get_typ typed_e in
      let* _ = Type_env.expected_equal ann inferred_t t in
      return typed_e
    | _ -> Type_env.with_local_scope (infer e)
    in
    let* t = get_typ te in
    let* generalized_type = generalize t in
    let* t = Type_env.generalize_type_vars t in
    let* () = Type_env.add_global name_text generalized_type in
    let typed_name = (name_text, Ann_typed (get_location (snd name), t)) in
    let toplet_expr = TopLet (typed_name, te, Ann_typed (get_location ann, t)) in
    return (toplet_expr :: acc)
  ) (return []) p in
  let* () = Type_env.flatten_unification_env in
  normalize_typed_program (List.rev checked_program)

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
    let* t_rhs_generalized = generalize t_rhs in
    let* tbody = Type_env.with_locals [name, t_rhs_generalized] (infer body) in
    let* tbody_type = get_typ tbody in
    let* t_rhs = get_typ trhs in
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
      match StringMap.find_opt name env.local with
      | Some scheme ->
        Type_env.instantiate_scheme scheme
      | None -> (
        match StringMap.find_opt name env.global with
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
    let has_sigcons_pattern =
      List.exists (fun (pattern, _, _) ->
        match pattern with
        | PSigCons _ -> true
        | _ -> false) branches
    in
    let* t_scrutinee =
      if has_sigcons_pattern then (
        let* t_scrutinee = Type_env.apply_subst t_scrutinee in
        match t_scrutinee with
        | TVar _ ->
          let* prefers_string = probe_string_case_on_unresolved_scrutinee ann t_scrutinee branches in
          if prefers_string then (
            let* _ = Type_env.expected_equal ann t_scrutinee TString in
            Type_env.apply_subst t_scrutinee
          ) else (
            let* elem_t = Type_env.fresh_type_var () in
            let* _ = Type_env.expected_equal ann t_scrutinee (TSignal elem_t) in
            Type_env.apply_subst t_scrutinee
          )
        | _ -> return t_scrutinee
      ) else
        return t_scrutinee
    in
    let branch_mapper : type s. (s pattern * s expr * s ann) -> (typed pattern * typed expr) Type_env.t = 
      fun (pattern, branch, _) -> check_pattern_and_infer_branch pattern branch t_scrutinee in
    let* tbranches = Type_env.collect (List.map branch_mapper branches) in
    let* branch_types = Type_env.collect (List.map (fun (_, branch) -> get_typ branch) tbranches) in
    (* check that all branches have the same type *)
    (match branch_types with
    | [] -> 
      let* _ = error ann "Case expression must have at least one branch" in
      return (ECase (tscrutinee, [], Ann_typed (get_location ann, TError)))
    | t :: _ -> 
      let* _ = Type_env.collect (List.map (fun t' -> Type_env.expected_equal ann t t')  branch_types) in
      let* t = Type_env.apply_subst t in
      let* typed_branches = 
        List.map2 (
          fun (_,_, ann) (typed_pattern, typed_body) -> 
            let* body_type = get_typ typed_body in
            return (typed_pattern, typed_body, Ann_typed(get_location ann, body_type))) 
          branches tbranches
        |> Type_env.collect
      in
      return (ECase (tscrutinee, typed_branches, Ann_typed (get_location ann, t))))
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
  | ECtor ((typ_name, typ_name_ann), args, ann) ->
    (* TODO: proper handling here - [get_constructor_signature] returns an error message mentioning patterns! *)
    let* arg_types, ctor_type = get_constructor_signature typ_name typ_name_ann in
    let* inferred_args = Type_env.collect (List.map infer args) in
    let* inferred_arg_types = Type_env.collect (List.map get_typ inferred_args) in
    let* _ = 
      let n_args = List.length arg_types in
      let n_inferred = List.length inferred_arg_types in
      if n_args = n_inferred
      then Type_env.collect (List.map2 (Type_env.expected_equal ann) arg_types inferred_arg_types)
      else
        let* _ = error ann (Format.asprintf "Constructor '%s' expects %d argument(s), but got %d" typ_name n_args n_inferred) in
        return []
    in
    let* ctor_type = Type_env.apply_subst ctor_type in
    let name = (typ_name, Ann_typed (get_location typ_name_ann, ctor_type)) in
    return (ECtor (name, inferred_args, Ann_typed (get_location ann, ctor_type)))

(** Checks a type against an expected type *)
and check : type stage. stage expr -> typ -> typed expr Type_env.t = fun e expected -> 
  let* expected = Type_env.apply_subst expected in (* I think this is fine - some extra work, hell yea! *)
  match e with
  | EConst (CNever, ann) -> 
    let* inner = Type_env.fresh_type_var () in 
    let* _ = Type_env.expected_equal ann expected (TLater inner) in
    let* t = Type_env.apply_subst inner in
    return (EConst (CNever, Ann_typed (get_location ann, TLater t)))
  | ELet ((name, name_ann), rhs, body, ann) ->
    let* trhs = infer rhs in
    let* t_rhs = get_typ trhs in
    let* rhs_gen = generalize t_rhs in
    let* tbody = Type_env.with_local name rhs_gen (check body expected) in
    let* tbody_type = get_typ tbody in
    let* t_rhs = get_typ trhs in
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

and get_constructor_signature : type stage. string -> stage ann -> (typ list * typ) Type_env.t =
  fun ctor_name ann ->
  match ctor_name with
  | "Just" ->
    let* a = Type_env.fresh_type_var () in
    return ([a], TOption a)
  | "Nothing" ->
    let* a = Type_env.fresh_type_var () in
    return ([], TOption a)
  | "Left" ->
    let* a = Type_env.fresh_type_var () in
    let* b = Type_env.fresh_type_var () in
    return ([a], TSync (a, b))
  | "Right" ->
    let* a = Type_env.fresh_type_var () in
    let* b = Type_env.fresh_type_var () in
    return ([b], TSync (a, b))
  | "Both" ->
    let* a = Type_env.fresh_type_var () in
    let* b = Type_env.fresh_type_var () in
    return ([a; b], TSync (a, b))
  | _ ->
    let* _ = error ann (Format.asprintf "Unknown constructor pattern '%s'" ctor_name) in
    return ([], TError)

and check_pattern : type stage. stage pattern -> typ -> (typed pattern * (string * scheme) list) Type_env.t =
  fun pattern expected_type ->
  match pattern with
  | PWildcard ann ->
    let* pattern_type = Type_env.apply_subst expected_type in
    return (PWildcard (Ann_typed (get_location ann, pattern_type)), [])
  | PVar (name, ann) ->
    let* pattern_type = Type_env.apply_subst expected_type in
    return (PVar (name, Ann_typed (get_location ann, pattern_type)), [name, mono pattern_type])
  | PConst (c, ann) ->
    let* const_type = infer_const_type ann c in
    let* _ = Type_env.expected_equal ann const_type expected_type in
    let* pattern_type = Type_env.apply_subst const_type in
    return (PConst (c, Ann_typed (get_location ann, pattern_type)), [])
  | PTuple (p1, p2, ann) ->
    let* t1 = Type_env.fresh_type_var () in
    let* t2 = Type_env.fresh_type_var () in
    let* _ = Type_env.expected_equal ann expected_type (TTuple (t1, t2)) in
    let* typed_p1, bindings_1 = check_pattern p1 t1 in
    let* typed_p2, bindings_2 = check_pattern p2 t2 in
    let* t1 = Type_env.apply_subst t1 in
    let* t2 = Type_env.apply_subst t2 in
    let pattern_type = TTuple (t1, t2) in
    return (PTuple (typed_p1, typed_p2, Ann_typed (get_location ann, pattern_type)), bindings_1 @ bindings_2)
  | PSigCons (hd_pattern, (tail_name, tail_ann), ann) ->
    let* expected_type = Type_env.apply_subst expected_type in
    (match expected_type with
    | TSignal a ->
      let* a = Type_env.apply_subst a in
      let* typed_hd, hd_bindings = check_pattern hd_pattern a in
      let tail_type = TLater (TSignal a) in
      let tail_binding = (tail_name, mono tail_type) in
      let typed_tail = (tail_name, Ann_typed (get_location tail_ann, tail_type)) in
      let pattern_type = TSignal a in
      return (PSigCons (typed_hd, typed_tail, Ann_typed (get_location ann, pattern_type)), hd_bindings @ [tail_binding])
    | TString ->
      let* typed_hd, hd_bindings = check_pattern hd_pattern TString in
      let tail_binding = (tail_name, mono TString) in
      let typed_tail = (tail_name, Ann_typed (get_location tail_ann, TString)) in
      return (PStringCons (typed_hd, typed_tail, Ann_typed (get_location ann, TString)), hd_bindings @ [tail_binding])
    | _ ->
      let* _ = error ann (Format.asprintf "Expected a signal or string for '::' pattern, got '%a'" Ast.pp_typ expected_type) in
      let typed_tail = (tail_name, Ann_typed (get_location tail_ann, TError)) in
      return (PStringCons (PWildcard (Ann_typed (get_location ann, TError)), typed_tail, Ann_typed (get_location ann, TError)), []))
  | PStringCons (_, _, ann) ->
    let* _ = error ann "Internal error: string-cons pattern should not appear before typed lowering" in
    return (PWildcard (Ann_typed (get_location ann, TError)), [])
  | PCtor ((ctor_name, ctor_ann), args, ann) ->
    let* param_types, ctor_result = get_constructor_signature ctor_name ctor_ann in
    let* _ = Type_env.expected_equal ann expected_type ctor_result in
    let expected_arity = List.length param_types in
    let actual_arity = List.length args in
    let param_types =
      if expected_arity = actual_arity
      then param_types
      else if expected_arity < actual_arity
      then param_types @ List.init (actual_arity - expected_arity) (fun _ -> TError)
      else List.take actual_arity param_types
    in
    let* _ =
      if expected_arity = actual_arity
      then return ()
      else error ann (Format.asprintf "Constructor '%s' expects %d argument(s), but pattern has %d" ctor_name expected_arity actual_arity)
    in
    let* checked_args =
      Type_env.collect (List.map2 (fun p pt -> check_pattern p pt) args param_types)
    in
    let typed_args = List.map fst checked_args in
    let bindings = List.concat_map snd checked_args in
    let* pattern_type = Type_env.apply_subst ctor_result in
    let typed_ctor_name = (ctor_name, Ann_typed (get_location ctor_ann, pattern_type)) in
    return (PCtor (typed_ctor_name, typed_args, Ann_typed (get_location ann, pattern_type)), bindings)

and check_pattern_and_infer_branch : type stage. stage pattern -> stage expr -> typ -> (typed pattern * typed expr) Type_env.t =
  fun pattern rhs scrutinee_type -> 
  let* typed_pattern, bindings = check_pattern pattern scrutinee_type in
  let* rhs' = Type_env.with_locals bindings (infer rhs) in
  return (typed_pattern, rhs')

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
  | Lt | Leq | Gt | Geq ->
    let* te1 = check e1 TInt in
    let* te2 = check e2 TInt in
    return (EBinary (op, te1, te2, Ann_typed (get_location ann, TBool)))
  | Add ->
    let* te1 = infer e1 in
    let* t1 = get_typ te1 in
    let* te2 = infer e2 in
    let* t2 = get_typ te2 in
    let* _ = Type_env.expected_equal ann t1 t2 in
    let* t = Type_env.apply_subst t1 in
    (match t with
    | TInt | TString -> return (EBinary (op, te1, te2, Ann_typed (get_location ann, t)))
    | _ ->
      let* _ = error ann (Format.asprintf "Operator '+' expects Int or String operands, got '%a'" Ast.pp_typ t) in
      return (EBinary (op, te1, te2, Ann_typed (get_location ann, TError))))
  | Mul | Sub | Div | Mod ->
    let* te1 = check e1 TInt in
    let* te2 = check e2 TInt in
    return (EBinary (op, te1, te2, Ann_typed (get_location ann, TInt)))
  | BSync ->
    let* a1 = Type_env.fresh_type_var () in
    let* a2 = Type_env.fresh_type_var () in
    let* te1 = check e1 (TLater a1) in
    let* te2 = check e2 (TLater a2)in 
    let* a1 = Type_env.apply_subst a1 in
    let* a2 = Type_env.apply_subst a2 in
    return (EBinary (BSync, te1, te2, Ann_typed (get_location ann, TLater (TSync (a1, a2))))) 
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
  | UNot ->
    let* te = check e TBool in
    return (EUnary (UNot, te, Ann_typed (get_location ann, TBool)))
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
and normalize_typed_ann : type stage. (string Type_env.IntMap.t ref) -> stage ann -> stage ann Type_env.t =
  fun id_to_name -> function
  | Ann_typed (loc, typ) ->
    let* typ = Type_env.generalize_type_vars ~id_to_name typ in
    return (Ann_typed (loc, typ))
  | Ann_parsed loc -> return (Ann_parsed loc)
  | Ann_bound (loc, scope) -> return (Ann_bound (loc, scope))

and normalize_typed_name id_to_name ((name, ann) : typed name) : typed name Type_env.t =
  let* ann = normalize_typed_ann id_to_name ann in
  return (name, ann)

and normalize_typed_pattern id_to_name : typed pattern -> typed pattern Type_env.t = function
  | PWildcard ann ->
    let* ann = normalize_typed_ann id_to_name ann in
    return (PWildcard ann)
  | PVar (name, ann) ->
    let* ann = normalize_typed_ann id_to_name ann in
    return (PVar (name, ann))
  | PConst (c, ann) ->
    let* ann = normalize_typed_ann id_to_name ann in
    return (PConst (c, ann))
  | PTuple (p1, p2, ann) ->
    let* p1 = normalize_typed_pattern id_to_name p1 in
    let* p2 = normalize_typed_pattern id_to_name p2 in
    let* ann = normalize_typed_ann id_to_name ann in
    return (PTuple (p1, p2, ann))
  | PSigCons (p1, name, ann) ->
    let* p1 = normalize_typed_pattern id_to_name p1 in
    let* name = normalize_typed_name id_to_name name in
    let* ann = normalize_typed_ann id_to_name ann in
    return (PSigCons (p1, name, ann))
  | PStringCons (p1, name, ann) ->
    let* p1 = normalize_typed_pattern id_to_name p1 in
    let* name = normalize_typed_name id_to_name name in
    let* ann = normalize_typed_ann id_to_name ann in
    return (PStringCons (p1, name, ann))
  | PCtor (name, args, ann) ->
    let* name = normalize_typed_name id_to_name name in
    let* args = Type_env.collect (List.map (normalize_typed_pattern id_to_name) args) in
    let* ann = normalize_typed_ann id_to_name ann in
    return (PCtor (name, args, ann))

and normalize_typed_expr id_to_name : typed expr -> typed expr Type_env.t = function
  | EConst (c, ann) ->
    let* ann = normalize_typed_ann id_to_name ann in
    return (EConst (c, ann))
  | EVar name ->
    let* name = normalize_typed_name id_to_name name in
    return (EVar name)
  | ECtor (name, args, ann) ->
    let* name = normalize_typed_name id_to_name name in
    let* args = Type_env.collect (List.map (normalize_typed_expr id_to_name) args) in
    let* ann = normalize_typed_ann id_to_name ann in
    return (ECtor (name, args, ann))
  | ELet (name, rhs, body, ann) ->
    let* name = normalize_typed_name id_to_name name in
    let* rhs = normalize_typed_expr id_to_name rhs in
    let* body = normalize_typed_expr id_to_name body in
    let* ann = normalize_typed_ann id_to_name ann in
    return (ELet (name, rhs, body, ann))
  | EFun (params, body, ann) ->
    let* params = Type_env.collect (List.map (normalize_typed_name id_to_name) params) in
    let* body = normalize_typed_expr id_to_name body in
    let* ann = normalize_typed_ann id_to_name ann in
    return (EFun (params, body, ann))
  | EApp (fn, args, ann) ->
    let* fn = normalize_typed_expr id_to_name fn in
    let* args = Type_env.collect (List.map (normalize_typed_expr id_to_name) args) in
    let* ann = normalize_typed_ann id_to_name ann in
    return (EApp (fn, args, ann))
  | EUnary (op, expr, ann) ->
    let* expr = normalize_typed_expr id_to_name expr in
    let* ann = normalize_typed_ann id_to_name ann in
    return (EUnary (op, expr, ann))
  | EBinary (op, e1, e2, ann) ->
    let* e1 = normalize_typed_expr id_to_name e1 in
    let* e2 = normalize_typed_expr id_to_name e2 in
    let* ann = normalize_typed_ann id_to_name ann in
    return (EBinary (op, e1, e2, ann))
  | ETuple (e1, e2, ann) ->
    let* e1 = normalize_typed_expr id_to_name e1 in
    let* e2 = normalize_typed_expr id_to_name e2 in
    let* ann = normalize_typed_ann id_to_name ann in
    return (ETuple (e1, e2, ann))
  | ECase (scrutinee, branches, ann) ->
    let* scrutinee = normalize_typed_expr id_to_name scrutinee in
    let* branches =
      Type_env.collect @@ List.map (fun (pattern, body, branch_ann) ->
        let* pattern = normalize_typed_pattern id_to_name pattern in
        let* body = normalize_typed_expr id_to_name body in
        let* branch_ann = normalize_typed_ann id_to_name branch_ann in
        return (pattern, body, branch_ann)) branches
    in
    let* ann = normalize_typed_ann id_to_name ann in
    return (ECase (scrutinee, branches, ann))
  | EIfe (cond, if_true, if_false, ann) ->
    let* cond = normalize_typed_expr id_to_name cond in
    let* if_true = normalize_typed_expr id_to_name if_true in
    let* if_false = normalize_typed_expr id_to_name if_false in
    let* ann = normalize_typed_ann id_to_name ann in
    return (EIfe (cond, if_true, if_false, ann))
  | EAnno (expr, typ, ann) ->
    let* expr = normalize_typed_expr id_to_name expr in
    let* typ = Type_env.generalize_type_vars ~id_to_name typ in
    let* ann = normalize_typed_ann id_to_name ann in
    return (EAnno (expr, typ, ann))

and normalize_typed_top_expr id_to_name : typed top_expr -> typed top_expr Type_env.t = function
  | TopLet (name, expr, ann) ->
    let* name = normalize_typed_name id_to_name name in
    let* expr = normalize_typed_expr id_to_name expr in
    let* ann = normalize_typed_ann id_to_name ann in
    return (TopLet (name, expr, ann))

and normalize_typed_program (program : typed program) : typed program Type_env.t =
  let id_to_name = ref Type_env.IntMap.empty in
  Type_env.collect (List.map (normalize_typed_top_expr id_to_name) program)
