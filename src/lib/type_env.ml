open! Ast
open Collections

type typing_error = Typing_error of Location.t * string

type scheme = Forall of string list * typ
type scheme_env = scheme StringMap.t

type constructor_defintion = {
  name: string;
  arg_types: typ list;
  result_typ: string;
  tag: int;
}
type typedefinition = { 
  constructors: constructor_defintion list;
  type_params: string list;
}
type typedefinition_env = {
  types: typedefinition StringMap.t;
  constructors: constructor_defintion StringMap.t; (* name -> constructor, a short cut*)
}

module TypeDefinitionEnv = struct
  let to_ctor_mappings t = StringMap.map (fun (c : constructor_defintion) -> c.tag) t.constructors
end

type unification_env = typ IntMap.t (* id -> typ - union find *)

type typing_state = {
  errors: typing_error list;
  global : scheme_env; 
  local : scheme_env; 
  typedefinitions: typedefinition_env;
  unification_env : unification_env;
  tvar_counter : int;
}

let empty_env = {
  errors = [];
  global = StringMap.empty;
  local = StringMap.empty;
  typedefinitions = { types = StringMap.empty; constructors = StringMap.empty };
  unification_env = IntMap.empty;
  tvar_counter = 0;
}

type 'a t = Env of (typing_state -> 'a * typing_state)

let run ?builtins:(bo = None) (Env m) = 
  match bo with
  | None -> m empty_env
  | Some bs -> 
    let global_env = StringMap.add_seq (List.to_seq bs) empty_env.global in
    m { empty_env with global = global_env }

let return a = Env (fun state -> (a, state))

let bind (Env m: 'a t) (f: 'a -> 'b t) : 'b t =
  Env (fun state ->
    let (a, state') = m state in
    let Env m' = f a in
    m' state')

let get_state : typing_state t = Env (fun state -> (state, state))
let modify_state f : unit t = Env (fun state -> ((), f state))

let report_error (ann: _ ann) (msg: string) : unit t =
  modify_state (fun state -> 
    let err = Typing_error (get_location ann, msg) in
    { state with errors = err :: state.errors })

module Operators = struct
  let ( let* ) = bind
end

let map f m = bind m (Fun.compose return f)

let collect (ms : 'a t list) : 'a list t =
  let open Operators in
  let* results = 
    List.fold_left 
    (fun acc m -> let* xs = acc in let* x = m in return (x :: xs)) 
    (return []) 
    ms 
  in
  return (List.rev results)

let add_local name scheme : unit t =
  modify_state (fun env -> 
    { env with local = StringMap.add name scheme env.local }) 
  
let add_locals names_and_schemes : unit t = 
  modify_state (fun env -> 
    { env with local = StringMap.add_seq (List.to_seq names_and_schemes) env.local })

let add_global name scheme : unit t = 
  modify_state (fun env -> 
    { env with global = StringMap.add name scheme env.global })

let add_globals names_and_schemes : unit t = 
  modify_state (fun env -> 
    { env with global = StringMap.add_seq (List.to_seq names_and_schemes) env.global })

let with_local_scope action : 'a t = 
  let open Operators in
  let* original_local = get_state |> map (fun s -> s.local) in
  let* result = action in
  modify_state (fun s -> { s with local = original_local }) |> map (fun () -> result)

let with_locals bindings (m : 'a t) : 'a t =
  let open Operators in
  with_local_scope (let* () = add_locals bindings in m)

let with_local name scheme m = 
  let open Operators in
  with_local_scope (let* () = add_local name scheme in m)

let add_type_def name tparams : unit t =
  let open Operators in
  let* typ_def_env = get_state |> map (fun s -> s.typedefinitions) in
  let typ_def = {constructors = []; type_params = tparams} in
  let typ_def_env' = { typ_def_env with types = StringMap.add name typ_def typ_def_env.types } in
  modify_state (fun s -> { s with typedefinitions = typ_def_env' })
  
let add_constructor_of type_name ctor_name arg_types : unit t =
  let open Operators in
  let* typ_def_env = get_state |> map (fun s -> s.typedefinitions) in
  let* typ_def_env' = 
    match StringMap.find_opt type_name typ_def_env.types with
    | None -> failwith "Compiler error - attempting to add a constructor for a type that doesn't exist"
    | Some typ_def -> 
      let ctor_def = { name = ctor_name; arg_types; result_typ = type_name; tag = List.length typ_def.constructors } in
      let updated_typ_def = { typ_def with constructors = ctor_def :: typ_def.constructors } in
      let types = StringMap.add type_name updated_typ_def typ_def_env.types in
      let constructors = StringMap.add ctor_name ctor_def typ_def_env.constructors in
      return { types; constructors }
  in
  modify_state (fun s -> { s with typedefinitions = typ_def_env' })

let prepare_with builtin_vars builtin_types action =
  let open Operators in
  (* print builtin_vars *)
  let* _ = let* vars = collect builtin_vars in add_globals vars in
  let* _ = 
    builtin_types
    |> List.fold_left (fun acc (type_name, type_params, ctors) -> 
        let* _ = acc in
        List.fold_left (fun acc (name, arg_types) -> 
          let* _ = acc in 
          add_constructor_of type_name name arg_types) 
        (add_type_def type_name type_params) ctors
      )
    (return ())
  in
  let* action_result = action in
  return action_result
  
(*  |-----------------------|
    |     UNIFICATION       |
    |-----------------------| *)

let rec occurs_in (tyvar_id: int) (t: typ) : bool =
  match t with
  | TVar id -> tyvar_id = id
  | TError -> false
  | TUnit | TInt | TBool | TString | TName _ | TParam _-> false
  | TSignal t | TLater t | TDelay t  | TChan t-> occurs_in tyvar_id t
  | TTuple (t1, t2) -> occurs_in tyvar_id t1 || occurs_in tyvar_id t2
  | TApp (t, ts) -> occurs_in tyvar_id t || List.exists (occurs_in tyvar_id) ts
  | TFun (Cons1(front, rest), t2) ->
    occurs_in tyvar_id front || List.exists (occurs_in tyvar_id) rest || occurs_in tyvar_id t2

(* TODO: monadic? *)
let fresh_type_var () : typ t = 
  let open Operators in
  let* {tvar_counter = id; _} = get_state in
  let* () = modify_state (fun env -> { env with tvar_counter = env.tvar_counter + 1 }) in
  return (TVar id)

let rec unify ann (t1: typ) (t2: typ) : unit t =
  let open Operators in
  let* t1 = find t1 in
  let* t2 = find t2 in
  match t1, t2 with
  | TError, _ | _, TError -> return () (* don't report cascading errors *)
  | TUnit, TUnit | TInt, TInt | TBool, TBool | TString, TString -> return ()
  | TName n1, TName n2 when n1 = n2 -> return ()
  | TParam p1, TParam p2 when p1 = p2 -> return ()
  | TSignal t1, TSignal t2 | TLater t1, TLater t2 | TDelay t1, TDelay t2 
  | TChan t1, TChan t2 -> 
    unify ann t1 t2
  | TTuple (t1a, t1b), TTuple (t2a, t2b) ->
    let* () = unify ann t1a t2a in
    unify ann t1b t2b
  | TApp (t1, ts1), TApp (t2, ts2) ->
    if List.length ts1 <> List.length ts2
    then report_error ann (Format.asprintf "Type mismatch: cannot unify '%a' with '%a' because they have different number of type arguments" Ast.pp_typ t1 Ast.pp_typ t2)
    else 
      let* () = unify ann t1 t2 in
      List.fold_left2 (fun acc t1 t2 -> let* _ = acc in unify ann t1 t2) (return ()) ts1 ts2
  | TFun (ts1, rt1), TFun (ts2, rt2) ->
    if List1.length ts1 <> List1.length ts2
    then
      (* unify up to length of the shortest one *)
      let Cons1(ts1_hd, ts1_rest) = ts1 in
      let Cons1(ts2_hd, ts2_rest) = ts2 in
      let* () = unify ann ts1_hd ts2_hd in
      let unify_params shortest shortest_ret_typ longest longest_ret_typ = 
        let min_length_rest = List.length shortest in
        let longest_to_unify = List.take min_length_rest longest in
        let* _ = List.fold_left2 (fun acc t1 t2 -> let* _ = acc in unify ann t1 t2) (return ()) shortest longest_to_unify in
        let longest_remaining = List.drop min_length_rest longest in
        let longest_remaining_list1 = List1.of_list longest_remaining in
        unify ann shortest_ret_typ (TFun (longest_remaining_list1, longest_ret_typ))
      in

      if List.length ts1_rest < List.length ts2_rest 
      then unify_params ts1_rest rt1 ts2_rest rt2 
      else unify_params ts2_rest rt2 ts1_rest rt1
    else
      let* () = unify ann rt1 rt2 in
      List1.fold_left2 (fun acc t1 t2 -> let* _ = acc in unify ann t1 t2) (return ()) ts1 ts2
  | TVar id1 , TVar id2 when id1 = id2 -> return ()
  | TVar id, t | t, TVar id -> 
    if occurs_in id t 
    then report_error ann "Occurs check failed: cannot unify type variable with type that contains it"
    else 
      modify_state (fun env ->
        let uni_env' = IntMap.add id t env.unification_env in
        { env with unification_env = uni_env' })
  | TParam p, other | other, TParam p -> 
    let* other_type = find other in
    report_error ann (Format.asprintf "Unable to unify type parameter '%s' with '%a'" p Ast.pp_typ other_type)
  | _ -> report_error ann (Format.asprintf "Type mismatch: cannot unify '%a' with '%a'" Ast.pp_typ t1 Ast.pp_typ t2)

and find (t: typ) : typ t =
  let open Operators in
  match t with
  | TVar id -> 
    let* {unification_env; _} = get_state in 
    (match IntMap.find_opt id unification_env with
    | Some t' -> find t'
    | None -> return t)
  | _ -> return t

let rec apply_subst ?(subst_map = None) (t: typ) : typ t =
  let open Operators in
  let apply_subst = apply_subst ~subst_map in
  let* t = find t in
  match t with
  | TError -> return t
  | TUnit | TInt | TBool | TString | TName _ -> return t
  | TParam p when Option.is_some subst_map -> (
    match StringMap.find_opt p (Option.get subst_map) with
    | Some t' -> return t'
    | None -> return t)
  | TParam _ -> return t
  | TApp (t, ts) -> 
    let* t = apply_subst t in
    let* ts = collect (List.map apply_subst ts) in
    return (TApp (t, ts))
  | TSignal t -> 
    let* t = apply_subst t in
    return (TSignal t)
  | TLater t -> 
    let* t = apply_subst t in
    return (TLater t)
  | TDelay t -> 
    let* t = apply_subst t in
    return (TDelay t)
  | TTuple (t1, t2) -> 
    let* t1 = apply_subst t1 in
    let* t2 = apply_subst t2 in
    return (TTuple (t1,t2))
  | TChan t -> 
    let* t = apply_subst t in
    return (TChan t)
  | TVar id -> find (TVar id)
  | TFun (param_types, ret_type) ->
    let* ret_type'    = apply_subst ret_type in
    let param_results = List1.fold_left (fun acc a -> apply_subst a :: acc) [] param_types in
    let* param_types' = collect (List.rev param_results) in
    return (TFun (Cons1(List.hd param_types', List.tl param_types'), ret_type'))

let instantiate_scheme : scheme -> typ t = function
| Forall ([], t) -> return t
| Forall (vars, t) -> 
  let open Operators in
  let* vars_t = collect (vars |> List.map (fun s -> let* t = fresh_type_var () in return (s, t))) in
  let subst_map = StringMap.of_list vars_t in 
  apply_subst ~subst_map:(Some subst_map) t

let expected_equal ann expected t = unify ann expected t

let get_type te = bind (find (expr_get_ann te |> ann_get_type)) apply_subst

let flatten_unification_env : unit t =
  let open Operators in
  let* {unification_env; _} = get_state in
  let bindings = IntMap.bindings unification_env in
  let* flattened_bindings = collect (List.map (fun (id, t) -> let* t' = apply_subst t in return (id, t')) bindings) in
  let flattened_map = IntMap.of_list flattened_bindings in
  modify_state (fun env -> { env with unification_env = flattened_map })

let print_unification_env : unit t = 
  let open Operators in
  let* {unification_env; _} = get_state in
  print_endline ("SIZE OF ENV: " ^ string_of_int (IntMap.cardinal unification_env));
  let bindings = IntMap.bindings unification_env in
  let binding_strs = List.map (fun (id, t) -> Format.asprintf "T%d -> %a" id Ast.pp_typ t) bindings in
  let result = String.concat "\n" binding_strs in
  return (Format.printf "Unification environment:\n%s\n" result)

(* return the typ but every TVar which has not been unified with anything(?) is 
  lifted to a TParam ('a) ... *)
let generalize_type_vars ?(id_to_name = ref IntMap.empty) typ : typ t = 
  let open Operators in
  let name_index = ref 0 in
  
  let rec go typ = 
    match typ with
    | TError -> return TError
    | TUnit | TInt | TBool | TString | TName _ | TParam _ -> return typ
    | TApp (t, ts) -> 
      let* t = go t in
      let* ts = collect (List.map go ts) in
      return (TApp (t, ts))
    | TSignal t -> let* t = go t in return (TSignal t)
    | TLater t  -> let* t = go t in return (TLater t)
    | TDelay t  -> let* t = go t in return (TDelay t)
    | TChan t   -> let* t = go t in return (TChan t)
    | TTuple (t1, t2) ->
      let* t1 = go t1 in let* t2 = go t2 in return (TTuple (t1, t2))
    | TFun (Cons1(front, rest), ret) ->
      let* params = collect (List.map go (front :: rest)) in
      let* ret = go ret in
      return (TFun (Cons1(List.hd params, List.tl params), ret))
    | TVar id -> 
      match IntMap.find_opt id !id_to_name with
      | Some name -> return (TParam name)
      | None -> 
        incr name_index;
        let param_name = "'inferred" ^ string_of_int !name_index in 
        id_to_name := IntMap.add id param_name !id_to_name;
        return (TParam param_name)
  in
  let* typ = apply_subst typ in
  go typ

let get_constructor_signature ann ctor_name : (typ list * typ) t =
  let open Operators in
  let* typ_def_env = get_state |> map (fun s -> s.typedefinitions) in
  match StringMap.find_opt ctor_name typ_def_env.constructors with
  | None -> 
    report_error ann (Format.asprintf "Unknown constructor: '%s'" ctor_name) 
    |> map (fun () -> ([], TError))
  | Some ctor_def -> 
    match StringMap.find_opt ctor_def.result_typ typ_def_env.types with
    | None -> failwith (Printf.sprintf "Compiler error - constructor '%s' refers to a type '%s' that doesn't exist" ctor_name ctor_def.result_typ)
    | Some type_def ->
      let* type_params = collect (List.map (fun v -> let* t = fresh_type_var () in return (v,t)) type_def.type_params) in
      let subst_map = StringMap.of_list type_params in
      let* arg_types = List.map (fun t -> apply_subst ~subst_map:(Some subst_map) t) ctor_def.arg_types |> collect in
      let result_typ = TApp (TName ctor_def.result_typ, List.map snd type_params) in
      return (arg_types, result_typ)

let has_type_definition type_name : bool t =
  let open Operators in
  let* typ_def_env = get_state |> map (fun s -> s.typedefinitions) in
  return (StringMap.mem type_name typ_def_env.types)

let has_ctor_definition ctor_name : bool t =
  let open Operators in
  let* typ_def_env = get_state |> map (fun s -> s.typedefinitions) in
  return (StringMap.mem ctor_name typ_def_env.constructors)
