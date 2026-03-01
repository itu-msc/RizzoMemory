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
  let t_free = free_type_vars_typ t in
  let generalized_vars = StringSet.diff t_free env_free in
  return (forall (StringSet.elements generalized_vars) t)
and free_type_vars_typ : typ -> StringSet.t = function
  | TError -> StringSet.empty
  | TVar _ -> StringSet.empty 
  | TTuple (t1, t2) | TSync (t1, t2) -> StringSet.union (free_type_vars_typ t1) (free_type_vars_typ t2)
  | TSignal t | TDelay t | TLater t -> free_type_vars_typ t
  | TUnit | TInt | TBool | TString | TName _ -> StringSet.empty
  | TParam v -> StringSet.singleton v
  | TFun (Cons1(front, rest), ret_type) -> 
    let param_free = List.fold_left (fun acc t -> StringSet.union acc (free_type_vars_typ t)) StringSet.empty (front :: rest) in
    StringSet.union param_free (free_type_vars_typ ret_type)
and free_type_vars_scheme : scheme -> StringSet.t = function
  | Forall (vars, t) -> 
    StringSet.diff (free_type_vars_typ t) (StringSet.of_list vars)
and free_type_vars_env_scheme: Type_env.scheme_env -> StringSet.t = 
  fun scheme_env -> StringMap.fold (fun _ scheme acc -> StringSet.union acc (free_type_vars_scheme scheme)) scheme_env StringSet.empty
and free_type_vars_env : unit -> StringSet.t Type_env.t = fun () ->
  let* env = Type_env.get_state in
  return (StringSet.union (free_type_vars_env_scheme env.global) (free_type_vars_env_scheme env.local))

let rec typecheck : type stage. stage program -> typed program option = fun p -> 
  let builtins = List.map 
    (fun ({name; typ; _}:Rizzo_builtins.builtin_info) -> name, forall (free_type_vars_typ typ |> StringSet.to_list) typ) 
    Rizzo_builtins.builtins 
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
    let* te = Type_env.with_local_scope (infer e) in 
    let* t = get_typ te in
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
    let* t_rhs_generalized = generalize t_rhs in
    let* tbody = Type_env.with_locals [name, t_rhs_generalized] (infer body) in
    let* tbody_type = get_typ tbody in
    let name' = (name, Ann_typed (get_location name_ann, t_rhs)) in
    return (ELet (name', trhs, tbody, Ann_typed (get_location ann, tbody_type)))
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
  | EApp (f, args, ann) -> 
    let* tf = infer f in 
    let* tf_type = Type_env.get_type tf in
    (match tf_type, args with
    | TFun (param_types, ret_type), arg1 :: arg_rest -> 
      infer_application tf param_types ret_type (Cons1 (arg1, arg_rest)) ann
    | TFun _, [] -> 
      let* _ = error ann "Cannot apply a function to no arguments" in
      return (EApp (tf, [], Ann_typed (get_location ann, TError)))
    | TError, _ -> return (EApp (tf, [], Ann_typed (get_location ann, TError))) (* stop cascasing erros *)
    | _ -> 
      let* _ = error (expr_get_ann f) (Format.asprintf "Cannot apply a non-function type '%a'" Ast.pp_typ tf_type) in
      return (EApp (tf, [], Ann_typed (get_location ann, TError)))
    )
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
  | EFun (_,_, ann) -> 
    let* _ = error ann "Inference for functions not implemented yet - unification todo" in
    (* TODO: what about failure causes? can we return a dummy expr? *)
    return (dummy ann)
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
    let* rhs_gen = generalize t_rhs in
    let* tbody = Type_env.with_local name rhs_gen (check body expected) in
    let* tbody_type = get_typ tbody in
    let name' = (name, Ann_typed (get_location name_ann, t_rhs)) in
    return (ELet (name', trhs, tbody, Ann_typed (get_location ann, tbody_type)))
  | EFun (params, body, ann) -> 
    let* Cons1(p1_type, param_types_rest), ret_type = match expected with
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

(* and check_pattern_and_infer_branch env pattern rhs scrutinee_type : ((typed pattern * typed expr), typing_error) Result.t = *)
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
    let* t = match scrutinee_type with
      | TSignal t -> return t
      | _ -> 
        let* _ = error ann "Pattern SigCons type does not match scrutinee type" in
        return TError
    in
    let* rhs' = Type_env.with_locals [(hd_name, mono t); (tail_name, mono (TSignal t))] (infer rhs) in
    let pattern_type = TSignal t in
    let hd_pattern = PVar (hd_name, Ann_typed (get_location hd_ann, t)) in
    let tail_name = (tail_name, Ann_typed (get_location ann, TSignal t)) in
    let pattern = PSigCons (hd_pattern, tail_name, Ann_typed (get_location ann, pattern_type)) in
    return (pattern, rhs')
  | PCtor _ -> failwith "saving ctor patterns for later"
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
    return (EBinary (BSync, te1, te2, Ann_typed (get_location ann, TSync (t1, t2)))) 
  | BOStar ->
    let* te1 = infer e1 in 
    let* t1 = get_typ te1 in
    let* a,b = (match t1 with
    (* typeof(te1) : TFun ([t1; t2; t3; t4], t5) 
      which after lying te2 to it should give a function of type TFun ([t2; t3; t4], t5), makes sense I guess *)
    | TDelay (TFun (Cons1 (a, []), b)) -> return (a,b)
    | TDelay (TFun (Cons1 (a, a' :: rest), b)) -> return (a, TFun(Cons1(a',rest), b))
    | _ -> 
      let* t = get_typ te1 in
      let* _ = error (expr_get_ann e1) (Format.asprintf "Expected a delayed function - but got '%a'" Ast.pp_typ t) in
      return (TError, TError)
    ) in
    let* te2 = check e2 (TDelay a) in
    return (EBinary (BOStar, te1, te2, Ann_typed (get_location ann, TDelay b)))
  | BLaterApp ->
    let* te1 = infer e1 in
    let* t1 = get_typ te1 in
    let* a,b = (match t1 with
    | TDelay (TFun (Cons1 (a, []), b)) -> return (a,b)
    | TDelay (TFun (Cons1 (a, a' :: rest), b)) -> return (a, TFun(Cons1(a',rest), b))
    | _ -> 
      let* t = get_typ te1 in
      let* _ = error (expr_get_ann e1) (Format.asprintf "Expected a delayed function - but got '%a'" Ast.pp_typ t) in
      return (TError, TError)
    ) in
    let* te2 = check e2 (TLater a) in
    return (EBinary (BLaterApp, te1, te2, Ann_typed (get_location ann, TLater b)))

and infer_application : type s. typed expr -> typ list1 -> typ -> s expr list1 -> s ann -> typed expr Type_env.t =
  fun typed_function_expr param_types ret_type args ann ->
  let Cons1(param_type1, param_types_rest) = param_types in
  let Cons1(arg1, args_rest) = args in
  if Ast_helpers.list1_length param_types >= Ast_helpers.list1_length args
  then begin (* either partial or full application *)
    let rec go p_types arg_types acc = match p_types, arg_types with
    | [], [] -> return ([], List.rev acc)
    | param_rest, [] -> return (param_rest, List.rev acc)
    | t :: ts, arg :: args -> 
      let check_arg_result = check arg t in
      go ts args (check_arg_result :: acc)
    | [], _ -> failwith (Printf.sprintf "(%s, %d) oops - more arguments than parameter types - this is handled separately" __FILE__ __LINE__)
    in
    let* remaining_params, go_result = go param_types_rest args_rest [check arg1 param_type1] in
    let* typed_args = Type_env.collect go_result in
    let ret_type = match remaining_params with
    | [] -> ret_type
    | first :: rest -> TFun (Cons1(first, rest), ret_type) 
    in
    let* ret_type = Type_env.apply_subst ret_type in
    return (EApp (typed_function_expr, typed_args, Ann_typed (get_location ann, ret_type)))
  end 
  else (* Overapplied - length(param_types) < length(args) - split into more applications *)  
    let args_to_take = List.length param_types_rest in
    let args_for_this_app = List.take args_to_take args_rest in
    let* checked = 
      let args_and_expected = (List.combine args_for_this_app param_types_rest) in
      let checked_results = (check arg1 param_type1) :: List.map (fun (arg, expected_type) -> check arg expected_type) args_and_expected in
      Type_env.collect checked_results
    in
    let args_for_next_app = List.drop args_to_take args_rest in
    let app = EApp (typed_function_expr, checked, Ann_typed (get_location ann, ret_type)) in
    match ret_type, args_for_next_app with
    | TFun (remaining_param_types, final_ret_type), first_arg :: rest_arg-> 
      infer_application app remaining_param_types final_ret_type (Cons1(first_arg, rest_arg)) ann
    | _ -> 
      let* _ = error ann (Format.asprintf "Too many arguments applied to non-function type '%a'" Ast.pp_typ ret_type) in
      return (dummy ann)
