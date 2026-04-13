open! Refcount_core

let add_if_member (p : primitive) (env : StringSet.t) : StringSet.t =
  match p with
  | Var x -> if StringSet.mem x env then env else StringSet.add x env
  | Const _ -> env

let name_of_primitive_opt = function
  | Var x -> Some x
  | Const _ -> None

let global_names = ref StringSet.empty

let set_global_names builtins (RefProg { globals; functions } : program) =
  global_names :=
    StringSet.of_list
      (List.map fst globals
      @ List.map fst functions
      @ (StringMap.to_list builtins |> List.map fst))

let rec free_vars fn = StringSet.diff (free_vars_fn fn) !global_names
and free_vars_rexpr rexpr = StringSet.diff (free_vars_rexpr_inner rexpr) !global_names

and free_vars_rexpr_inner rexpr : StringSet.t =
  match rexpr with
  | RConst _ -> StringSet.empty
  | RCall (c, args) | RPartialApp (c, args) ->
      StringSet.of_list (c :: List.filter_map name_of_primitive_opt args)
  | RVarApp (f, arg) ->
      StringSet.add f
        (match name_of_primitive_opt arg with
         | Some x -> StringSet.singleton x
         | None -> StringSet.empty)
  | RCtor Ctor { tag = _; fields } ->
      List.fold_left (fun acc y -> add_if_member y acc) StringSet.empty fields
  | RProj (_, name) -> StringSet.singleton name
  | RCtor Signal { head; tail } ->
      StringSet.of_list @@ List.filter_map name_of_primitive_opt [head; tail]
  | RReset name -> StringSet.singleton name
  | RReuse (name, Ctor { fields; _ }) ->
      StringSet.of_list (name :: List.filter_map name_of_primitive_opt fields)
  | RReuse (name, Signal { head; tail }) ->
      StringSet.of_list (name :: List.filter_map name_of_primitive_opt [head; tail])

and free_vars_fn = function
  | FnRet x ->
      name_of_primitive_opt x
      |> Option.fold ~none:StringSet.empty ~some:StringSet.singleton
  | FnLet (x, rhs, f) ->
      StringSet.union (free_vars_rexpr_inner rhs) (StringSet.remove x @@ free_vars f)
  | FnCase (scrutinee, branches) ->
      StringSet.singleton scrutinee
      |> StringSet.union (List.fold_left (fun acc { body; _ } -> StringSet.union acc (free_vars body)) StringSet.empty branches)
  | FnInc (v, f) | FnDec (v, f) ->
      StringSet.union (StringSet.singleton v) (free_vars f)

let rec collect func_ownerships (_f : fn_body) : StringSet.t =
  match _f with
  | FnRet _ -> StringSet.empty
  | FnCase (_, cases) ->
      List.fold_left (fun acc case -> StringSet.union acc (collect func_ownerships case)) StringSet.empty (get_cases cases)
  | FnInc _ | FnDec _ -> failwith "no inc/dec in collect"
  | FnLet (_, RConst _, rest) -> collect func_ownerships rest
  | FnLet (_, RCtor _, rest) -> collect func_ownerships rest
  | FnLet (_, RCall (c, xs), rest) ->
      let owned_args =
        List.combine xs (lookup_params func_ownerships c)
        |> List.filter_map (function
          | Var x, Owned -> Some x
          | _, _ -> None)
        |> StringSet.of_list
      in
      StringSet.union (collect func_ownerships rest) owned_args
  | FnLet (_, RVarApp (x, Var y), rest) ->
      StringSet.union (collect func_ownerships rest) (StringSet.of_list [x; y])
  | FnLet (_, RVarApp (x, Const _), rest) ->
      StringSet.union (collect func_ownerships rest) (StringSet.singleton x)
  | FnLet (_, RPartialApp (_, xs), rest) ->
      StringSet.union (collect func_ownerships rest) (StringSet.of_list @@ List.filter_map (function | Var v -> Some v | _ -> None) xs)
  | FnLet (_, RReset x, f) -> StringSet.union (collect func_ownerships f) (StringSet.singleton x)
  | FnLet (_, RReuse _, rest) -> collect func_ownerships rest
  | FnLet (z, RProj (_, x), rest) ->
      let fcol = collect func_ownerships rest in
      if StringSet.mem z fcol then StringSet.add x fcol else fcol

let infer_all_simple (RefProg { functions; _ } : program) : parameter_ownership =
  let func_ownership = ref StringMap.empty in
  List.iter
    (fun (c, Fun (params, _)) ->
      func_ownership := StringMap.add c (List.map (fun _ -> Borrowed) params) !func_ownership)
    functions;
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter
      (fun (fun_name, Fun (params, body)) ->
        let owned = collect !func_ownership body in
        let old_sig = lookup_params !func_ownership fun_name in
        let new_sig = List.map2 (fun y b -> if StringSet.mem y owned then Owned else b) params old_sig in
        if new_sig <> old_sig then (
          changed := true;
          func_ownership := StringMap.add fun_name new_sig !func_ownership))
      functions
  done;
  !func_ownership

let build_callers functions : StringSet.t StringMap.t =
  let add_caller callers ~callee ~caller =
    let s = Option.value (StringMap.find_opt callee callers) ~default:StringSet.empty in
    StringMap.add callee (StringSet.add caller s) callers
  in
  let rec scan_body caller callers = function
    | FnRet _ -> callers
    | FnInc (_, f) | FnDec (_, f) -> scan_body caller callers f
    | FnCase (_, arms) -> List.fold_left (scan_body caller) callers (get_cases arms)
    | FnLet (_, rhs, rest) ->
        let callers =
          match rhs with
          | RCall (c, _) -> add_caller callers ~callee:c ~caller
          | RPartialApp (c, _) -> add_caller callers ~callee:c ~caller
          | _ -> callers
        in
        scan_body caller callers rest
  in
  List.fold_left
    (fun callers (f_name, Fun (_params, body)) -> scan_body f_name callers body)
    StringMap.empty functions

let infer_all ?(builtins : parameter_ownership = StringMap.empty) (RefProg { functions; _ } : program) : parameter_ownership =
  let callers = build_callers functions in
  let beta0 =
    List.fold_left
      (fun beta (name, Fun (params, _body)) -> StringMap.add name (List.map (fun _ -> Borrowed) params) beta)
      builtins functions
  in
  let prog_map = List.fold_left (fun acc (name, fn) -> StringMap.add name fn acc) StringMap.empty functions in
  let beta = ref beta0 in
  let q : string Queue.t = Queue.create () in
  let in_q : bool StringMap.t ref = ref StringMap.empty in
  let enqueue f =
    if Option.value (StringMap.find_opt f !in_q) ~default:false then ()
    else (
      Queue.add f q;
      in_q := StringMap.add f true !in_q)
  in
  List.iter (fun (name, _) -> enqueue name) functions;
  while not (Queue.is_empty q) do
    let f_name = Queue.take q in
    in_q := StringMap.add f_name false !in_q;
    let Fun (params, body) =
      match StringMap.find_opt f_name prog_map with
      | Some fn -> fn
      | None -> failwith ("infer_all: missing function " ^ f_name)
    in
    let owned_vars = collect !beta body in
    let old_sig = lookup_params !beta f_name in
    let new_sig = List.map2 (fun param old -> if StringSet.mem param owned_vars then Owned else old) params old_sig in
    if new_sig <> old_sig then (
      beta := StringMap.add f_name new_sig !beta;
      let callers = Option.value (StringMap.find_opt f_name callers) ~default:StringSet.empty in
      StringSet.iter enqueue callers)
  done;
  !beta