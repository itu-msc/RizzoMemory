open! Ast
module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

type primitive =
  | Const of Ast.const
  | Var of string

type rexpr = (* expr in their RC IR *)
  | RCall of string * primitive list
  | RPartialApp of string * primitive list
  | RVarApp of string * primitive  (* TODO: do we want n-ary var-app? *)
  | RCtor of ctor
  | RSignal of { head: primitive; tail: primitive; } (* at runtime this will also include next/prev fields of heap, the tail can only be a var or never *)
  | RProj of int * string
  | RReset of string
  | RReuse of string * ctor
and ctor = { tag: int; fields: primitive list }

type fn_body = 
  | FnRet of primitive
  | FnLet of string * rexpr * fn_body
  | FnCase of string * fn_body list
  | FnInc of string * fn_body
  | FnDec of string * fn_body

type fn = Fun of string list * fn_body

type program = (string * fn) list

let pp_primitive out = function
  | Var x -> Format.pp_print_string out x
  | Const c -> Ast.pp_const out c

let pp_rexpr out = 
  let comma_separated pp out = Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out ", ") pp out in
  function
  | RCall (c, ys) -> Format.fprintf out "%s(%a)" c (comma_separated pp_primitive) ys
  | RPartialApp (c, ys) -> Format.fprintf out "pap %s(%a)" c (comma_separated pp_primitive) ys
  | RVarApp (x, y) -> Format.fprintf out "%s %a" x pp_primitive y
  | RCtor {tag; fields} -> Format.fprintf out "Ctor%d(%a)" tag (comma_separated pp_primitive) fields
  | RSignal {head; tail} -> Format.fprintf out "Signal(%a, %a)" pp_primitive head pp_primitive tail
  | RProj (i, x) -> Format.fprintf out "proj_%d %s" i x
  | RReset x -> Format.fprintf out "reset %s" x
  | RReuse (x, {tag; fields}) -> Format.fprintf out "reuse %s in Ctor%d(%a)" x tag (comma_separated pp_primitive) fields

let rec pp_fnbody out = function
  | FnRet x -> Format.fprintf out "ret %a" pp_primitive x
  | FnLet (y, r, f) ->
        Format.fprintf out "let %s = %a in@ %a" y pp_rexpr r pp_fnbody f
  | FnCase (x, fs) ->
      let pp_branch out f =
        Format.fprintf out "@[<hov 2>| %a@]" pp_fnbody f
      in
      Format.fprintf out "match %s with@,  @[%a@]" x
        (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out "@,") pp_branch) fs
  | FnInc (x, f) -> Format.fprintf out "inc %s;@ %a" x pp_fnbody f
  | FnDec (x, f) -> Format.fprintf out "dec %s;@ %a" x pp_fnbody f

let pp_ref_counted_program out (p: program) =
  let pp_fn out (name, Fun (params, body)) =
    Format.fprintf out "fun %s(%a) =@,  @[<v>%a@]" name
      (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out ", ") Format.pp_print_string) params
      pp_fnbody body
  in
  Format.fprintf out "%a" (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out "@\n\n") pp_fn) p

(* The parameter list *)
let delta (p:program) (x:string) = List.assoc_opt x p

let add_if_member (p:primitive) (env: StringSet.t) : StringSet.t =
  match p with
  | Var x -> if StringSet.mem x env then env else StringSet.add x env
  | Const _ -> env

let rec free_vars_expr (env: StringSet.t) = function
  | RCall (_c, ys) | RPartialApp (_c, ys) ->
      List.fold_left (fun acc y -> add_if_member y acc) StringSet.empty ys
  | RVarApp (x, y) ->
      let acc = if StringSet.mem x env then StringSet.empty else StringSet.singleton x in
      add_if_member y acc
  | RCtor { tag = _; fields } ->
      List.fold_left (fun acc y -> add_if_member y acc) StringSet.empty fields
  | RProj ( _, x) ->
      if StringSet.mem x env then StringSet.empty else StringSet.singleton x
  | RSignal { head; tail } ->
      let acc =
        match head with
        | Var h -> if StringSet.mem h env then StringSet.empty else StringSet.singleton h
        | _ -> StringSet.empty 
      in
      add_if_member tail acc
  | RReset x ->
      if StringSet.mem x env then StringSet.empty else StringSet.singleton x
  | RReuse (x, { tag = _; fields }) ->
      let acc = if StringSet.mem x env then StringSet.empty else StringSet.singleton x in
      List.fold_left (fun acc y -> add_if_member y acc) acc fields
and free_vars_fn (env: StringSet.t) = function
  | FnRet x -> 
    (match x with
    | Var r -> if StringSet.mem r env then StringSet.empty else StringSet.singleton r
    | _ -> StringSet.empty)
  | FnLet (x, rhs, f) -> StringSet.union (free_vars_expr env rhs) (free_vars_fn (StringSet.add x env) f)
  | FnCase (x, fs) ->
      let env' = StringSet.add x env in
      List.fold_left (fun acc f -> StringSet.union acc (free_vars_fn env' f)) StringSet.empty fs
  | FnInc (x, f) | FnDec (x, f) ->
      if StringSet.mem x env then free_vars_fn env f
      else StringSet.add x (free_vars_fn env f)
and free_vars fn = free_vars_fn StringSet.empty fn

type ownership =
  | Owned
  | Borrowed

let is_owned = function
  | Owned -> true 
  | Borrowed -> false

(** Environment for keeping track of borrow status of local variables.
    Ullirch & De Moura call it 'βₗ'. *)
type beta_env = ownership StringMap.t
let lookup env x = 
  match StringMap.find_opt x env with
  | None -> Owned
  | Some b -> b

(** Mapping of 'borrowing signatures'. For every function it returns a list describing 
    the ownership it requires for each of its parameters.

    Ullirch & De Moura call it 'β' (beta)
*)
type parameter_ownership = ownership list StringMap.t
let lookup_params (b:parameter_ownership) (c:string) : ownership list =
  match StringMap.find_opt c b with
  | Some xs -> xs
  | None -> failwith (Printf.sprintf "unknown function in beta: '%s'" c)

(** Helper function to prepare [x] for use in an owned context. *)
let insert_inc (x:primitive) v f beta_env = 
  match x with
  | Var x -> 
    (match lookup beta_env x with
    | Owned when not (StringSet.mem x v) -> f
    | _ -> FnInc (x, f))
  | Const _ -> f

(** Inserts a decrement instruction if [x] is owned and no longer needed in [f] *)
let insert_dec (x:primitive) f env =
  match x with
  | Var x ->
    (match lookup env x with
    | Owned when not (StringSet.mem x (free_vars f)) -> FnDec (x, f)
    | _ -> f)
  | Const _ -> f

(** calls [insert_dec] multiple times :) *)
let rec insert_dec_many xs f beta_env = 
  match xs with
  | [] -> f
  | x :: xs -> insert_dec_many xs (insert_dec x f beta_env) beta_env

(** Inserts inc/dec instructions. Ullirch & De Moura call it 'C'.
    
  [var_ownership]: environment of borrow status of local variables
  
  [func_ownership]: environment of borrow status for parameters of all global functions
*)
let rec insert_rc (_f:fn_body) (var_ownerships: beta_env) func_ownerships : fn_body = 
  match _f with
  | FnRet x -> insert_inc x StringSet.empty _f var_ownerships
  | FnCase (x, fs) as case -> 
    let ys = StringSet.to_list (free_vars case) |> List.map (fun s -> Var s) in
    let compiled_fs = List.map (fun f -> insert_dec_many ys (insert_rc f var_ownerships func_ownerships) var_ownerships) fs in
    FnCase (x, compiled_fs)
  (* The Lets*)
  | FnLet (y, RProj (i, x), f) -> 
    (match lookup var_ownerships x with
    | Owned ->
      let compiled_f = insert_dec (Var x) (insert_rc f var_ownerships func_ownerships) var_ownerships in
      FnLet (y, RProj (i, x), FnInc(y, compiled_f))
    | Borrowed -> 
      let beta_env' = StringMap.add y Borrowed var_ownerships in
      let compiled_f = insert_rc f beta_env' func_ownerships in
      FnLet (y, RProj (i, x), FnInc(y, compiled_f))
    )
  | FnLet (z, RCall(c, ys), f) -> 
    let compiled_f = insert_rc f var_ownerships func_ownerships in
    c_app ys (lookup_params func_ownerships c) (FnLet (z, RCall(c,ys), compiled_f)) var_ownerships
  | FnLet (z, RPartialApp(c, ys), f) ->
    let compiled_f = insert_rc f var_ownerships func_ownerships in
    c_app ys (lookup_params func_ownerships c) (FnLet (z, RPartialApp(c,ys), compiled_f)) var_ownerships
  | FnLet (z, RVarApp (x,y), f) ->
    let compiled_f = insert_rc f var_ownerships func_ownerships in
    c_app [Var x;y] [Owned; Owned] (FnLet (z, RVarApp(x,y), compiled_f)) var_ownerships
  | FnLet (z, (RCtor { tag = _; fields} as ctor), f) ->
    let bs = List.map (fun _ -> Owned) fields in
    let compiled_f = insert_rc f var_ownerships func_ownerships in
    c_app fields bs (FnLet (z, ctor, compiled_f)) var_ownerships
  | FnLet (z, RSignal { head; tail }, f) ->
    let compiled_f = insert_rc f var_ownerships func_ownerships in
    c_app [head; tail] [Owned; Owned] (FnLet (z, RSignal { head; tail }, compiled_f)) var_ownerships
  | FnLet (z, RReset x, f) ->
    let compiled_f = insert_rc f var_ownerships func_ownerships in FnLet (z, RReset x, compiled_f)
  | FnLet (z, (RReuse (_, { tag = _; fields }) as reuse), f) ->
    let compiled_f = insert_rc f var_ownerships func_ownerships in
    c_app fields (List.map (fun _ -> Owned) fields) (FnLet (z, reuse, compiled_f)) var_ownerships
  | FnInc _ | FnDec _ -> failwith "Increment and Decrement should not exist prior to this step!"

and c_app (vars: primitive list) (bs: ownership list) (_f:fn_body) beta_env : fn_body = 
  match vars, bs, _f with
  | y::ys', Owned::bs', FnLet (_,_, f) ->
    let alive_variables = StringSet.union (StringSet.of_list @@ List.filter_map (function | Var v -> Some v | _ -> None) ys') (free_vars f) in
    insert_inc y alive_variables (c_app ys' bs' _f beta_env) beta_env
  | y::ys', Borrowed::bs', FnLet (z, e, f) ->
    let compiled_f = insert_dec y f beta_env in
    c_app ys' bs' (FnLet (z, e, compiled_f)) beta_env
  | [], _, FnLet(z, e, f) -> FnLet(z, e, f)
  | _ -> failwith "Not implemented"

(** Collects variables of an [fn_body] that can not be marked as [Borrowed].
    Ullirch & De Moura call it 'collect' *)
let rec collect func_ownerships (_f:fn_body) : StringSet.t = 
  match _f with
  | FnRet _ -> StringSet.empty
  | FnCase (_, cases) ->
    List.fold_left (fun acc case -> StringSet.union acc (collect func_ownerships case)) StringSet.empty cases
  | FnInc _ | FnDec _ -> failwith "no inc/dec in collect"
  (* The lets *)
  | FnLet (_, RCtor _, rest)
  | FnLet (_, RSignal _, rest) -> collect func_ownerships rest
  | FnLet (_, RCall (c, xs), rest) -> 
    let owned_args = 
      List.combine xs (lookup_params func_ownerships c)
      |> List.filter_map (function
      | Var x, Owned -> Some x
      | _, _ -> None)
      |> StringSet.of_list
    in 
    StringSet.union (collect func_ownerships rest) owned_args
  | FnLet (_, RVarApp (x,Var y), rest) ->
    StringSet.union (collect func_ownerships rest) (StringSet.of_list [x;y])
  | FnLet (_, RVarApp (x, Const _), rest) ->
    StringSet.union (collect func_ownerships rest) (StringSet.singleton x)
  | FnLet (_, RPartialApp (_, xs), rest) -> (*c must be the OWNED VERSION - THAT IS A COPY OF c WHERE ALL PARAMS ARE OWNED*)
    StringSet.union (collect func_ownerships rest) (StringSet.of_list @@ List.filter_map (function | Var v -> Some v | _ -> None) xs)
  | FnLet (_, RReset x, f) -> StringSet.union (collect func_ownerships f) (StringSet.singleton x)
  | FnLet (_, RReuse _, rest) -> collect func_ownerships rest
  | FnLet (z, RProj (_, x), rest) ->
    let fcol = collect func_ownerships rest in
    match StringSet.mem z fcol with
      | true -> StringSet.add x fcol
      | false -> fcol

let infer_all_simple (p:program) : parameter_ownership =
  let func_ownership = ref StringMap.empty in
  List.iter (fun (c, Fun (params, _)) ->
    func_ownership := StringMap.add c (List.map (fun _ -> Borrowed) params) !func_ownership
  ) p;

  let changed = ref true in
  while !changed do
    changed := false;
    (* Careful: if fun_name is used in a partial application, then we need a fresh name 
       fun_name_ALL_OWNED - something like that
      *)
    List.iter (fun (fun_name, Fun (params, body)) ->
      let owned = collect !func_ownership body in
      let old_sig = lookup_params !func_ownership fun_name in
      let new_sig =
        List.map2 (fun y b -> if StringSet.mem y owned then Owned else b)
          params old_sig
      in
      if new_sig <> old_sig then (
        changed := true;
        func_ownership := StringMap.add fun_name new_sig !func_ownership
      )
    ) p
  done;
  !func_ownership


(* callers_of[g] = set of functions that call g *)
let build_callers (p:program) : StringSet.t StringMap.t =
  let add_caller callers ~callee ~caller =
    let s = Option.value (StringMap.find_opt callee callers) ~default:StringSet.empty in
    StringMap.add callee (StringSet.add caller s) callers
  in
  let rec scan_body caller callers = function
    | FnRet _ -> callers
    | FnInc (_, f) | FnDec (_, f) -> scan_body caller callers f
    | FnCase (_, arms) ->
        List.fold_left (scan_body caller) callers arms
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
    StringMap.empty p

let infer_all ?(builtins:parameter_ownership = StringMap.empty) (p:program) : parameter_ownership =
  let callers = build_callers p in

  (* init: all Borrowed *)
  let beta0 =
    List.fold_left
      (fun b (name, Fun (params, _body)) ->
        StringMap.add name (List.map (fun _ -> Borrowed) params) b)
      builtins p
  in

  let prog_map =
    List.fold_left (fun m (n, fn) -> StringMap.add n fn m) StringMap.empty p
  in

  let beta = ref beta0 in
  let q : string Queue.t = Queue.create () in
  let in_q : bool StringMap.t ref = ref StringMap.empty in
  let enqueue f =
    if Option.value (StringMap.find_opt f !in_q) ~default:false then ()
    else (
      Queue.add f q;
      in_q := StringMap.add f true !in_q
    )
  in

  (* seed queue with all functions *)
  List.iter (fun (n, _) -> enqueue n) p;

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

    let new_sig =
      List.map2
        (fun param old ->
          if StringSet.mem param owned_vars then Owned else old)
        params old_sig
    in

    if new_sig <> old_sig then (
      beta := StringMap.add f_name new_sig !beta;

      (* if f changed, revisit its callers *)
      let cs =
        Option.value (StringMap.find_opt f_name callers)
          ~default:StringSet.empty
      in
      StringSet.iter enqueue cs
    )
  done;
  !beta

(** Inserts reset/reuse pairs into all global functions of a program [p] 
    This transformation should be performed before calling [insert_rc]
*)
let rec insert_reset_and_reuse_pairs_program (p: program) : program =
  p

(** Inserts reset/reuse pairs into [body]. Ullrich & De Moura call it 'R' *)
and insert_reset_and_reuse_pairs_fn body = match body with
  | FnRet _ -> body
  | FnLet (x, e, f) -> FnLet (x, e, insert_reset_and_reuse_pairs_fn f)
  | FnCase (s, cases) -> 
    let num_fields: int = failwith "what to do?" in
    FnCase (s, List.map (fun c -> 
      (* TODO: We need to get the actual number of fields from the constructors. type inference??*) 
      insert_reset s num_fields c) cases)
  | FnInc _ | FnDec _ -> 
    failwith "no inc/dec should exist before reset/reuse transformation - this transformation should be called before 'insert_rc'"

(** Inserts resets of variable [z] with number of constructor fields [num_fields] into [fn_body]. 
    Ullrich & De Moura call it 'D' *)
and insert_reset z num_fields fn_body = 
  match fn_body with
  | FnCase (s, cases) -> FnCase (s, List.map (insert_reset z num_fields) cases)
  | FnRet _ -> fn_body
  | FnLet (x, e, f) when free_vars_expr StringSet.empty e |> StringSet.mem z -> FnLet (x, e, insert_reset z num_fields f)
  | FnInc _ | FnDec _ -> failwith "no inc/dec should exist before reset/reuse transformation - this transformation should be called before 'insert_rc'"
  | _ ->
    let w = Utilities.new_var () in
    let reuse_fn_body = insert_reuse w num_fields fn_body in
    if reuse_fn_body <> fn_body then
      FnLet(w, RReset z, reuse_fn_body)
    else
      fn_body

(** Inserts reuse instructions into [fn_body] for variable [w] with number of constructor fields [num_fields] 
    Ullrich & De Moura call it 'S' *)
and insert_reuse w num_fields fn_body: fn_body =
  match fn_body with
  | FnCase (s, cases) -> FnCase (s, List.map (insert_reuse w num_fields) cases)
  | FnRet _ -> fn_body
  | FnLet (x, RCtor{tag; fields}, f) when List.length fields = num_fields ->
      FnLet (x, RReuse (w, {tag; fields}), f) 
  | FnLet (x, e, f) -> FnLet (x, e, insert_reuse w num_fields f)
  | FnInc _ | FnDec _ -> failwith "no inc/dec should exist before reset/reuse transformation - this transformation should be called before 'insert_rc'"
