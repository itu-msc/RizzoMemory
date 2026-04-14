open! Refcount_core
open Collections

module Analysis = Refcount_analysis
module Eq = Refcount_eq

let insert_inc (x : primitive) v f beta_env =
  match x with
  | Const _ -> f
  | Var x ->
      match lookup beta_env x with
      | Owned when not (StringSet.mem x v) -> f
      | _ -> FnInc (x, f)

let insert_dec (x : primitive) f env =
  match x with
  | Const _ -> f
  | Var x ->
      match lookup env x with
      | Owned when not (StringSet.mem x (Analysis.free_vars f)) -> FnDec (x, f)
      | _ -> f

let rec insert_dec_many xs f beta_env =
  match xs with
  | [] -> f
  | x :: rest -> insert_dec_many rest (insert_dec x f beta_env) beta_env

let rec insert_rc (_f : fn_body) (var_ownerships : beta_env) func_ownerships : fn_body =
  match _f with
  | FnRet x -> insert_inc x StringSet.empty _f var_ownerships
  | FnCase (x, fs) as case ->
      let ys = StringSet.to_list (Analysis.free_vars case) |> List.map (fun s -> Var s) in
      let compiled_fs =
        List.map
          (fun arm ->
            { arm with
              body = insert_dec_many ys (insert_rc arm.body var_ownerships func_ownerships) var_ownerships })
          fs
      in
      FnCase (x, compiled_fs)
  | FnLet (y, RConst c, f) -> FnLet (y, RConst c, insert_rc f var_ownerships func_ownerships)
  | FnLet (y, RProj (i, x), f) ->
      (match lookup var_ownerships x with
       | Owned ->
           let compiled_f = insert_dec (Var x) (insert_rc f var_ownerships func_ownerships) var_ownerships in
           FnLet (y, RProj (i, x), FnInc (y, compiled_f))
       | Borrowed ->
           let beta_env' = StringMap.add y Borrowed var_ownerships in
           let compiled_f = insert_rc f beta_env' func_ownerships in
           FnLet (y, RProj (i, x), compiled_f))
  | FnLet (z, RCall (c, ys), f) ->
      let compiled_f = insert_rc f var_ownerships func_ownerships in
      c_app ys (lookup_params func_ownerships c) (FnLet (z, RCall (c, ys), compiled_f)) var_ownerships
  | FnLet (z, RPartialApp (c, ys), f) ->
      let compiled_f = insert_rc f var_ownerships func_ownerships in
      c_app ys (lookup_params func_ownerships c) (FnLet (z, RPartialApp (c, ys), compiled_f)) var_ownerships
  | FnLet (z, RVarApp (x, y), f) ->
      let compiled_f = insert_rc f var_ownerships func_ownerships in
      c_app [Var x; y] [Owned; Owned] (FnLet (z, RVarApp (x, y), compiled_f)) var_ownerships
  | FnLet (z, (RCtor Ctor { fields; _ } as ctor), f) ->
      let ownerships = List.map (fun _ -> Owned) fields in
      let compiled_f = insert_rc f var_ownerships func_ownerships in
      c_app fields ownerships (FnLet (z, ctor, compiled_f)) var_ownerships
  | FnLet (z, RCtor Signal { head; tail }, f) ->
      let compiled_f = insert_rc f var_ownerships func_ownerships in
      c_app [head; tail] [Owned; Owned] (FnLet (z, RCtor (Signal { head; tail }), compiled_f)) var_ownerships
  | FnLet (z, RReset x, f) ->
      let compiled_f = insert_rc f var_ownerships func_ownerships in
      FnLet (z, RReset x, compiled_f)
  | FnLet (z, (RReuse (_, Signal { head; tail }) as reuse), f) ->
      let compiled_f = insert_rc f var_ownerships func_ownerships in
      c_app [head; tail] [Owned; Owned] (FnLet (z, reuse, compiled_f)) var_ownerships
  | FnLet (z, (RReuse (_, Ctor { fields; _ }) as reuse), f) ->
      let compiled_f = insert_rc f var_ownerships func_ownerships in
      c_app fields (List.map (fun _ -> Owned) fields) (FnLet (z, reuse, compiled_f)) var_ownerships
  | FnInc _ | FnDec _ -> failwith "Increment and Decrement should not exist prior to this step!"

and c_app (vars : primitive list) (ownerships : ownership list) (_f : fn_body) beta_env : fn_body =
  match vars, ownerships, _f with
  | y :: ys', Owned :: bs', FnLet (_, _, f) ->
      let alive_variables =
        StringSet.union (StringSet.of_list @@ List.filter_map (function | Var v -> Some v | _ -> None) ys') (Analysis.free_vars f)
      in
      insert_inc y alive_variables (c_app ys' bs' _f beta_env) beta_env
  | y :: ys', Borrowed :: bs', FnLet (z, e, f) ->
      let compiled_f = insert_dec y f beta_env in
      c_app ys' bs' (FnLet (z, e, compiled_f)) beta_env
  | [], _, FnLet (z, e, f) -> FnLet (z, e, f)
  | _ -> failwith "c_app: mismatch - more variables than ownership annotations"

let projection_partial_app_index = function
  | "head" | "list_head" | "fst" -> Some 0
  | "list_tail" | "snd" -> Some 1
  | _ -> None

let insert_owned_partial_app_wrapper func_ownership (RefProg { functions; globals } : program) =
  let all_owned_funcs = ref [] in
  let wrapper_params c =
    match List.find_opt (fun (name, _) -> name = c) functions with
    | Some (_, Fun (params, _)) -> params
    | None ->
        lookup_params func_ownership c
        |> List.mapi (fun i _ -> Printf.sprintf "%s_arg%d" c i)
  in
  let rec aux f =
    match f with
    | FnRet _ -> f
    | FnLet (x, RPartialApp (c, []), let_body) ->
        (match projection_partial_app_index c with
         | Some proj_idx ->
             let param = Printf.sprintf "%s_arg0" c in
             let function_name = Utilities.new_name (c ^ "_PROJ") in
             let ret = Utilities.new_var () in
             let body = FnLet (ret, RProj (proj_idx, param), FnRet (Var ret)) in
             all_owned_funcs := (function_name, Fun ([param], body), [Owned]) :: !all_owned_funcs;
             FnLet (x, RPartialApp (function_name, []), aux let_body)
         | None ->
             if List.exists (fun b -> not (is_owned b)) (lookup_params func_ownership c) then
               let params = wrapper_params c in
               let function_name = Utilities.new_name (c ^ "_ALL_OWNED") in
               let ret = Utilities.new_var () in
               let body = FnLet (ret, RCall (c, List.map (fun p -> Var p) params), FnRet (Var ret)) in
               let ownerships = List.map (fun _ -> Owned) params in
               all_owned_funcs := (function_name, Fun (params, body), ownerships) :: !all_owned_funcs;
               FnLet (x, RPartialApp (function_name, []), aux let_body)
             else
               FnLet (x, RPartialApp (c, []), aux let_body))
    | FnLet (x, RPartialApp (c, args), let_body) when List.exists (fun b -> not (is_owned b)) (lookup_params func_ownership c) ->
        let params = wrapper_params c in
        let function_name = Utilities.new_name (c ^ "_ALL_OWNED") in
        let ret = Utilities.new_var () in
        let body = FnLet (ret, RCall (c, List.map (fun p -> Var p) params), FnRet (Var ret)) in
        let ownerships = List.map (fun _ -> Owned) params in
        all_owned_funcs := (function_name, Fun (params, body), ownerships) :: !all_owned_funcs;
        FnLet (x, RPartialApp (function_name, args), aux let_body)
    | FnLet (x, rhs, f) -> FnLet (x, rhs, aux f)
    | FnDec (x, f) -> FnDec (x, aux f)
    | FnInc (x, f) -> FnInc (x, aux f)
    | FnCase (x, cases) -> FnCase (x, List.map (fun arm -> { arm with body = aux arm.body }) cases)
  in
  let functions' = List.map (fun (name, Fun (params, body)) -> name, Fun (params, aux body)) functions in
  let globals' = List.map (fun (name, body) -> name, aux body) globals in
  let functions' = List.map (fun (name, f, _) -> name, f) !all_owned_funcs @ functions' in
  let func_ownership' = List.fold_left (fun acc (name, _, ownerships) -> StringMap.add name ownerships acc) func_ownership !all_owned_funcs in
  (RefProg { functions = functions'; globals = globals' }, func_ownership')

let rec insert_reset_and_reuse_pairs_program (RefProg { functions; globals } : program) : program =
  let globals' = List.map (fun (name, body) -> name, insert_reset_and_reuse_pairs_fn body) globals in
  let functions' = List.map (fun (name, Fun (params, body)) -> name, Fun (params, insert_reset_and_reuse_pairs_fn body)) functions in
  RefProg { functions = functions'; globals = globals' }

and insert_reset_and_reuse_pairs_fn body =
  match body with
  | FnRet _ -> body
  | FnLet (x, e, f) -> FnLet (x, e, insert_reset_and_reuse_pairs_fn f)
  | FnCase (x, cases) ->
      FnCase (x,
        List.map
          (fun arm ->
            let compiled_body = insert_reset_and_reuse_pairs_fn arm.body in
            match arm.num_fields with
            | Some num_fields -> { arm with body = insert_reset x num_fields compiled_body }
            | None -> { arm with body = compiled_body })
          cases)
  | FnInc _ | FnDec _ ->
      failwith "no inc/dec should exist before reset/reuse transformation - this transformation should be called before 'insert_rc'"

and insert_reset z num_fields fn_body =
  match fn_body with
  | FnCase (s, cases) -> FnCase (s, List.map (fun arm -> { arm with body = insert_reset z num_fields arm.body }) cases)
  | FnRet _ -> fn_body
  | FnLet (x, e, f) when StringSet.mem z (Analysis.free_vars_rexpr e) || StringSet.mem z (Analysis.free_vars f) ->
      FnLet (x, e, insert_reset z num_fields f)
  | FnInc _ | FnDec _ ->
      failwith "no inc/dec should exist before reset/reuse transformation - this transformation should be called before 'insert_rc'"
  | _ ->
      let w = Utilities.new_var () in
      let reuse_fn_body = insert_reuse w num_fields fn_body in
      if not (Eq.eq_fnbody reuse_fn_body fn_body) then FnLet (w, RReset z, reuse_fn_body) else fn_body

and insert_reuse w num_fields fn_body : fn_body =
  match fn_body with
  | FnCase (s, cases) -> FnCase (s, List.map (fun arm -> { arm with body = insert_reuse w num_fields arm.body }) cases)
  | FnRet _ -> fn_body
  | FnLet (x, RCtor Ctor { tag; fields }, f) when List.length fields = num_fields ->
      FnLet (x, RReuse (w, Ctor { tag; fields }), f)
  | FnLet (x, RCtor Signal { head; tail }, f) when num_fields = 5 ->
      FnLet (x, RReuse (w, Signal { head; tail }), f)
  | FnLet (x, e, f) -> FnLet (x, e, insert_reuse w num_fields f)
  | FnInc _ | FnDec _ ->
      failwith "no inc/dec should exist before reset/reuse transformation - this transformation should be called before 'insert_rc'"

let reference_count_program builtins (RefProg { globals; _ } as p : program) =
  Analysis.set_global_names builtins p;
  let reset_reuse_program = insert_reset_and_reuse_pairs_program p in
  let func_ownerships = Analysis.infer_all ~builtins reset_reuse_program in
  let RefProg { globals = g; functions = f }, func_ownerships = insert_owned_partial_app_wrapper func_ownerships reset_reuse_program in
  Analysis.set_global_names builtins (RefProg { globals = g; functions = f });
  let globals_env = List.fold_left (fun env (name, _) -> StringMap.add name Borrowed env) StringMap.empty globals in
  let functions' =
    f
    |> List.map (fun (c_name, Fun (params, c_body)) ->
         let params_ownership = lookup_params func_ownerships c_name in
         let var_env = List.fold_left2 (fun env param ownership -> StringMap.add param ownership env) globals_env params params_ownership in
         let ref_counted_body = insert_rc c_body var_env func_ownerships in
         (c_name, Fun (params, insert_dec_many (List.map (fun s -> Var s) params) ref_counted_body var_env)))
  in
  let globals' = List.map (fun (name, body) -> name, insert_rc body globals_env func_ownerships) g in
  (func_ownerships, RefProg { functions = functions'; globals = globals' })