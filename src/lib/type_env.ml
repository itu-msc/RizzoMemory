open! Ast
module StringMap = Map.Make(String)
module IntMap = Map.Make(Int)

type typing_error = Typing_error of Location.t * string

type scheme = Forall of string list * typ
type scheme_env = scheme StringMap.t

type typedefinition = typ list
type typedefinition_env = typedefinition StringMap.t

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
  typedefinitions = StringMap.empty;
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

(*  |-----------------------|
    |     UNIFICATION       |
    |-----------------------| *)

let rec occurs_in (tyvar_id: int) (t: typ) : bool =
  match t with
  | TVar id -> tyvar_id = id
  | TError -> false
  | TUnit | TInt | TBool | TString | TName _ | TParam _-> false
  | TSignal t | TLater t | TDelay t | TOption t | TChan t-> occurs_in tyvar_id t
  | TSync (t1, t2) | TTuple (t1, t2) -> occurs_in tyvar_id t1 || occurs_in tyvar_id t2
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
  | TChan t1, TChan t2 | TOption t1, TOption t2 -> 
    unify ann t1 t2
  | TSync (t1a, t1b), TSync (t2a, t2b) | TTuple (t1a, t1b), TTuple (t2a, t2b) ->
    let* () = unify ann t1a t2a in
    unify ann t1b t2b
  | TFun (ts1, rt1), TFun (ts2, rt2) ->
    if Ast_helpers.list1_length ts1 <> Ast_helpers.list1_length ts2
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
        let longest_remaining_list1 = Ast_helpers.list1_of_list longest_remaining in
        unify ann shortest_ret_typ (TFun (longest_remaining_list1, longest_ret_typ))
      in

      if List.length ts1_rest < List.length ts2_rest 
      then unify_params ts1_rest rt1 ts2_rest rt2 
      else unify_params ts2_rest rt2 ts1_rest rt1
    else
      let* () = unify ann rt1 rt2 in
      Ast_helpers.list1_fold_left2 (fun acc t1 t2 -> let* _ = acc in unify ann t1 t2) (return ()) ts1 ts2
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
  | TSignal t -> 
    let* t = apply_subst t in
    return (TSignal t)
  | TLater t -> 
    let* t = apply_subst t in
    return (TLater t)
  | TDelay t -> 
    let* t = apply_subst t in
    return (TDelay t)
  | TSync (t1, t2) -> 
    let* t1 = apply_subst t1 in
    let* t2 = apply_subst t2 in
    return (TSync (t1,t2))
  | TTuple (t1, t2) -> 
    let* t1 = apply_subst t1 in
    let* t2 = apply_subst t2 in
    return (TTuple (t1,t2))
  | TOption t -> 
    let* t = apply_subst t in
    return (TOption t)
  | TChan t -> 
    let* t = apply_subst t in
    return (TChan t)
  | TVar id -> find (TVar id)
  | TFun (param_types, ret_type) ->
    let* ret_type'    = apply_subst ret_type in
    let param_results = Ast_helpers.list1_fold_left (fun acc a -> apply_subst a :: acc) [] param_types in
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
  
  let rec go typ = 
    match typ with
    | TError -> return TError
    | TUnit | TInt | TBool | TString | TName _ | TParam _ -> return typ
    | TSignal t -> let* t = go t in return (TSignal t)
    | TLater t  -> let* t = go t in return (TLater t)
    | TDelay t  -> let* t = go t in return (TDelay t)
    | TOption t -> let* t = go t in return (TOption t)
    | TChan t   -> let* t = go t in return (TChan t)
    | TTuple (t1, t2) ->
      let* t1 = go t1 in let* t2 = go t2 in return (TTuple (t1, t2))
    | TSync (t1, t2) ->
      let* t1 = go t1 in let* t2 = go t2 in return (TSync (t1, t2))
    | TFun (Cons1(front, rest), ret) ->
      let* params = collect (List.map go (front :: rest)) in
      let* ret = go ret in
      return (TFun (Cons1(List.hd params, List.tl params), ret))
    | TVar id -> 
      match IntMap.find_opt id !id_to_name with
      | Some name -> return (TParam name)
      | None -> 
        let param_name = Utilities.new_name "'inferred" in
        id_to_name := IntMap.add id param_name !id_to_name;
        return (TParam param_name)
  in
  let* typ = apply_subst typ in
  go typ
