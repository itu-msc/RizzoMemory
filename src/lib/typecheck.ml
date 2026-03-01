open! Ast

type typing_error = Typing_error of string
let ( let* ) = Result.bind
let return = Result.ok
let error msg = Result.error (Typing_error msg)
type top_level_typing_error = Location.t * string
let top_level_error_message ((loc, msg) : top_level_typing_error) =
  Printf.sprintf "%s: %s" (Location.to_string loc) msg

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

let tfun1 a r = TFun (Cons1 (a, []), r)
let tfun2 a b r = TFun (Cons1 (a, [b]), r)

type infer_state = { subst : (string, typ) Hashtbl.t; mutable next_tvar : int }

let create_infer_state () = { subst = Hashtbl.create 32; next_tvar = 0 }

let fresh_tvar state =
  let name = "'t" ^ string_of_int state.next_tvar in
  state.next_tvar <- state.next_tvar + 1;
  TVar name

let rec apply_subst state t =
  match t with
  | TVar name -> (
      match Hashtbl.find_opt state.subst name with
      | Some t' ->
        let t'' = apply_subst state t' in
        Hashtbl.replace state.subst name t'';
        t''
      | None -> t
    )
  | TFun (Cons1 (arg, rest), ret) ->
    TFun (Cons1 (apply_subst state arg, List.map (apply_subst state) rest), apply_subst state ret)
  | TSignal t' -> TSignal (apply_subst state t')
  | TTuple (a, b) -> TTuple (apply_subst state a, apply_subst state b)
  | TLater t' -> TLater (apply_subst state t')
  | TDelay t' -> TDelay (apply_subst state t')
  | TSync (a, b) -> TSync (apply_subst state a, apply_subst state b)
  | TUnit | TNever | TInt | TString | TBool -> t

let normalize_type state t = apply_subst state t

let rec occurs_in_type state v t =
  match apply_subst state t with
  | TVar name -> name = v
  | TFun (Cons1 (arg, rest), ret) ->
    occurs_in_type state v arg
    || List.exists (occurs_in_type state v) rest
    || occurs_in_type state v ret
  | TSignal t' | TLater t' | TDelay t' -> occurs_in_type state v t'
  | TTuple (a, b) | TSync (a, b) -> occurs_in_type state v a || occurs_in_type state v b
  | TUnit | TNever | TInt | TString | TBool -> false

let typ_to_string t = Format.asprintf "%a" pp_typ t

let bind_tvar state name t =
  let t = apply_subst state t in
  if t = TVar name then
    return ()
  else if occurs_in_type state name t then
    error (Printf.sprintf "Occurs check failed: %s occurs in %s" name (typ_to_string t))
  else (
    Hashtbl.replace state.subst name t;
    return ()
  )

let rec unify state t1 t2 : (unit, typing_error) Result.t =
  let t1 = apply_subst state t1 in
  let t2 = apply_subst state t2 in
  match t1, t2 with
  | TVar name, t
  | t, TVar name ->
    bind_tvar state name t
  | TUnit, TUnit
  | TNever, TNever
  | TInt, TInt
  | TString, TString
  | TBool, TBool -> return ()
  | TSignal a, TSignal b
  | TLater a, TLater b
  | TDelay a, TDelay b ->
    unify state a b
  | TTuple (a1, b1), TTuple (a2, b2)
  | TSync (a1, b1), TSync (a2, b2) ->
    let* () = unify state a1 a2 in
    unify state b1 b2
  | TFun (Cons1 (a1, r1), ret1), TFun (Cons1 (a2, r2), ret2) ->
    let* () = unify state a1 a2 in
    let* () = unify_type_lists state r1 r2 in
    unify state ret1 ret2
  | _ ->
    error (Printf.sprintf "Type mismatch: %s vs %s" (typ_to_string t1) (typ_to_string t2))

and unify_type_lists state ts1 ts2 =
  match ts1, ts2 with
  | [], [] -> return ()
  | t1 :: rest1, t2 :: rest2 ->
    let* () = unify state t1 t2 in
    unify_type_lists state rest1 rest2
  | _ -> error "Function arity mismatch"

let list1_of_list_result (types : typ list) =
  match types with
  | [] -> error "Expected at least one type"
  | t :: ts -> return (Cons1 (t, ts))

let builtin_default_type state name =
  match name with
  | "start_event_loop" -> tfun1 TUnit TUnit
  | "output_int_signal" -> tfun1 (TSignal TInt) TUnit
  | "add" | "sub" | "mul" | "div" -> tfun2 TInt TInt TInt
  | "lt" | "leq" -> tfun2 TInt TInt TBool
  | "eq" ->
    let a = fresh_tvar state in
    tfun2 a a TBool
  | "head" ->
    let a = fresh_tvar state in
    tfun1 (TSignal a) a
  | "fst" ->
    let a = fresh_tvar state in
    let b = fresh_tvar state in
    tfun1 (TTuple (a, b)) a
  | "snd" ->
    let a = fresh_tvar state in
    let b = fresh_tvar state in
    tfun1 (TTuple (a, b)) b
  | _ ->
    let a = fresh_tvar state in
    let b = fresh_tvar state in
    tfun1 a b

let seed_builtin_globals state env =
  let builtin_names =
    Rizzo_builtins.builtins_map
    |> Rizzo_builtins.M.bindings
    |> List.map fst
  in
  List.fold_left
    (fun acc name ->
       let t = builtin_default_type state name in
       { acc with global = StringMap.add name (mono t) acc.global })
    env
    builtin_names

let predeclare_top_levels state env (program : 's program) =
  List.fold_left
    (fun acc top ->
       match top with
       | TopLet (name, _, _) ->
         if StringMap.mem name acc.global then
           acc
         else
           { acc with global = StringMap.add name (mono (fresh_tvar state)) acc.global })
    env
    program

let rec typecheck : type stage. stage program -> (typed program, typing_error) Result.t =
  fun p ->
  let typed_program, errors = typecheck_collect p in
  if errors = [] then
    return typed_program
  else
    error (errors |> List.map top_level_error_message |> String.concat "\n")

and typecheck_collect : type stage. stage program -> (typed program * top_level_typing_error list) =
  fun p ->
  let state = create_infer_state () in
  let env0 = predeclare_top_levels state (seed_builtin_globals state empty_env) p in
  let rec fold env typed_rev errors_rev program =
    match program with
    | [] -> (List.rev typed_rev, List.rev errors_rev)
    | TopLet (name, e, ann) :: rest ->
      let top_loc = get_location ann in
      let continue msg = fold env typed_rev ((top_loc, msg) :: errors_rev) rest in
      match infer state env e with
      | Ok te ->
        let t = normalize_type state (get_typ te) in
        let env' = { env with global = StringMap.add name (mono t) env.global } in
        let typed_top = TopLet (name, te, Ann_typed (get_location ann, t)) in
        fold env' (typed_top :: typed_rev) errors_rev rest
      | Error (Typing_error msg) ->
        continue msg
      | exception Failure msg ->
        continue ("Internal typechecker failure in `" ^ name ^ "`: " ^ msg)
  in
  fold env0 [] [] p

and infer : type stage. infer_state -> typing_env -> stage expr -> (typed expr, typing_error) Result.t = fun state env e ->
  match e with
  | EConst (CNever, ann) ->
    let a = fresh_tvar state in
    return (EConst (CNever, Ann_typed (get_location ann, TLater (TSignal a))))
  | EConst (c, ann) ->
    let* const_type = infer_const_type c in
    let const_type = normalize_type state const_type in
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
    let var_type = normalize_type state var_type in
    return (EVar (name, Ann_typed (get_location ann, var_type)))
  | ETuple (e1, e2, ann) ->
    let* te1 = infer state env e1 in
    let* te2 = infer state env e2 in
    let t1 = normalize_type state (get_typ te1) in
    let t2 = normalize_type state (get_typ te2) in
    return (ETuple (te1, te2, Ann_typed (get_location ann, TTuple (t1, t2))))
  | EBinary (SigCons, e1, e2, ann) ->
    let* te1 = infer state env e1 in
    let t1 = normalize_type state (get_typ te1) in
    let* te2 = check state env e2 (TLater (TSignal t1)) in
    return (EBinary (SigCons, te1, te2, Ann_typed (get_location ann, TSignal t1)))
  | EBinary (Eq, e1, e2, ann) ->
    let* te1 = infer state env e1 in
    let t1 = normalize_type state (get_typ te1) in
    let* te2 = check state env e2 t1 in
    return (EBinary (Eq, te1, te2, Ann_typed (get_location ann, TBool)))
  | EBinary (Lt | Leq as op, e1, e2, ann) ->
    let* te1 = check state env e1 TInt in
    let* te2 = check state env e2 TInt in
    return (EBinary (op, te1, te2, Ann_typed (get_location ann, TBool)))
  | EBinary (Add | Mul | Sub | Div as op, e1, e2, ann) ->
    let* te1 = check state env e1 TInt in
    let* te2 = check state env e2 TInt in
    return (EBinary (op, te1, te2, Ann_typed (get_location ann, TInt)))
  | EBinary (BSync, e1, e2, ann) ->
    let* te1 = infer state env e1 in
    let t1 = normalize_type state (get_typ te1) in
    let* te2 = infer state env e2 in
    let t2 = normalize_type state (get_typ te2) in
    return (EBinary (BSync, te1, te2, Ann_typed (get_location ann, TSync (t1, t2))))
  | EBinary (BOStar, e1, e2, ann) ->
    let* te1 = infer state env e1 in
    let* (a, b) = match normalize_type state (get_typ te1) with
    | TDelay (TFun (Cons1(a, []), b)) -> return (a, b)
    | TDelay (TFun (Cons1(a, a' :: rest), b)) -> return (a, TFun (Cons1 (a', rest), b))
    | _ -> error "not too sure here"
    in
    let* te2 = check state env e2 (TDelay a) in
    return (EBinary (BOStar, te1, te2, Ann_typed (get_location ann, TDelay b)))
  | EBinary (BLaterApp, e1, e2, ann) ->
    let* te1 = infer state env e1 in
    let* (a, b) = match normalize_type state (get_typ te1) with
    | TDelay (TFun (Cons1(a, []), b)) -> return (a, b)
    | TDelay (TFun (Cons1(a, a' :: rest), b)) -> return (a, TFun (Cons1 (a', rest), b))
    | _ -> error "not too sure here"
    in
    let* te2 = check state env e2 (TLater a) in
    return (EBinary (BLaterApp, te1, te2, Ann_typed (get_location ann, TLater b)))
  | ECase (scrutinee, branches, ann) ->
    let* tscrutinee = infer state env scrutinee in
    let t_scrutinee = normalize_type state (get_typ tscrutinee) in
    let branch_mapper : type s. (s pattern * s expr * s ann) -> (typed pattern * typed expr, typing_error) Result.t =
      fun (pattern, branch, _) -> check_pattern_and_infer_branch state env pattern branch t_scrutinee in
    let* tbranches = result_combine (List.map branch_mapper branches) in
    let branch_types = List.map (fun (_, branch) -> normalize_type state (get_typ branch)) tbranches in
    (match branch_types with
    | [] -> error "Case expression must have at least one branch"
    | t :: ts ->
      let rec unify_branch_types = function
        | [] -> return ()
        | t' :: rest ->
          let* () = unify state t t' in
          unify_branch_types rest
      in
      let* () = unify_branch_types ts in
      let result_t = normalize_type state t in
      let typed_branches =
        List.map2
          (fun (_, _, ann) (typed_pattern, typed_body) ->
             let body_t = normalize_type state (get_typ typed_body) in
             (typed_pattern, typed_body, Ann_typed (get_location ann, body_t)))
          branches
          tbranches
      in
      return (ECase (tscrutinee, typed_branches, Ann_typed (get_location ann, result_t))))
  | ELet ((name, _ ) as n, value, body, ann) ->
    let* tvalue = infer state env value in
    let t = normalize_type state (get_typ tvalue) in
    let env' = { env with local = StringMap.add name (mono t) env.local } in
    let* tbody = infer state env' body in
    let body_t = normalize_type state (get_typ tbody) in
    return (ELet (stage_name_to_typed_name n t, tvalue, tbody, Ann_typed (get_location ann, body_t)))
  | EFun (params, body, ann) ->
    let param_types = List.map (fun _ -> fresh_tvar state) params in
    let env_with_params =
      List.fold_left2
        (fun env_acc (name, _) param_t ->
           { env_acc with local = StringMap.add name (mono param_t) env_acc.local })
        env
        params
        param_types
    in
    let* tbody = infer state env_with_params body in
    let body_t = normalize_type state (get_typ tbody) in
    let param_types = List.map (normalize_type state) param_types in
    let typed_params = List.map2 stage_name_to_typed_name params param_types in
    let* param_list = list1_of_list_result param_types in
    return (EFun (typed_params, tbody, Ann_typed (get_location ann, TFun (param_list, body_t))))
  | EApp (fn, args, ann) ->
    let* tfn = infer state env fn in
    let* targs = result_combine (List.map (infer state env) args) in
    let arg_types = List.map (fun arg -> normalize_type state (get_typ arg)) targs in
    let* arg_list = list1_of_list_result arg_types in
    let result_t = fresh_tvar state in
    let* () = unify state (get_typ tfn) (TFun (arg_list, result_t)) in
    let app_t = normalize_type state result_t in
    return (EApp (tfn, targs, Ann_typed (get_location ann, app_t)))
  | _ -> error "E*** not implemented yet: todo!"

and stage_name_to_typed_name : type stage. stage name -> typ -> typed name =
  fun (n, ann) t -> (n, Ann_typed (get_location ann, t))

and infer_const_type : const -> (typ, typing_error) Result.t = function
  | CUnit -> return TUnit
  | CInt _ -> return TInt
  | CBool _ -> return TBool
  | CString _ -> return TString
  | CNever -> return TNever

and check_pattern_and_infer_branch : type stage. infer_state -> typing_env -> stage pattern -> stage expr -> typ -> ((typed pattern * typed expr), typing_error) Result.t =
  fun state env pattern rhs scrutinee_type ->
  match pattern with
  | PWildcard ->
    let* rhs' = infer state env rhs in
    return (PWildcard, rhs')
  | PVar (name, ann) ->
    let pattern_type = normalize_type state scrutinee_type in
    let env' = { env with local = StringMap.add name (mono pattern_type) env.local } in
    let pattern = PVar (name, Ann_typed (get_location ann, pattern_type)) in
    let* rhs' = infer state env' rhs in
    return (pattern, rhs')
  | PConst (c, ann) ->
    let* const_type = infer_const_type c in
    let* () = unify state const_type scrutinee_type in
    let const_type = normalize_type state const_type in
    let* rhs' = infer state env rhs in
    return (PConst (c, Ann_typed (get_location ann, const_type)), rhs')
  | PTuple (PVar (n1,_) , PVar (n2,_), ann) ->
    let t1 = fresh_tvar state in
    let t2 = fresh_tvar state in
    let* () = unify state scrutinee_type (TTuple (t1, t2)) in
    let t1 = normalize_type state t1 in
    let t2 = normalize_type state t2 in
    let env' = { env with local = StringMap.add n1 (mono t1) env.local } in
    let env' = { env' with local = StringMap.add n2 (mono t2) env'.local } in
    let p1 = PVar (n1, Ann_typed (get_location ann, t1)) in
    let p2 = PVar (n2, Ann_typed (get_location ann, t2)) in
    let pattern_type = TTuple (t1, t2) in
    let pattern : typed pattern = PTuple (p1, p2, Ann_typed (get_location ann, pattern_type)) in
    let* rhs' = infer state env' rhs in
    return (pattern, rhs')
  | PSigCons (PVar (hd_name, hd_ann), (tail_name, tail_ann), ann) ->
    let t = fresh_tvar state in
    let* () = unify state scrutinee_type (TSignal t) in
    let t = normalize_type state t in
    let env' = { env with local = StringMap.add hd_name (mono t) env.local } in
    let env' = { env' with local = StringMap.add tail_name (mono (TLater (TSignal t))) env'.local } in
    let* rhs' = infer state env' rhs in
    let pattern_type = TSignal t in
    let hd_pattern = PVar (hd_name, Ann_typed (get_location hd_ann, t)) in
    let tail_name = (tail_name, Ann_typed (get_location tail_ann, TLater (TSignal t))) in
    let pattern = PSigCons (hd_pattern, tail_name, Ann_typed (get_location ann, pattern_type)) in
    return (pattern, rhs')
  | PCtor _ -> failwith "saving ctor patterns for later"
  | _ -> error "Pattern type checking not implemented yet"

and check : type stage. infer_state -> typing_env -> stage expr -> typ -> (typed expr, typing_error) Result.t =
  fun state env e expected_typ ->
    let* te = infer state env e in
    let* () = unify state (get_typ te) expected_typ in
    return te
