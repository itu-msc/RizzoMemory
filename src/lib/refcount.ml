open! Ast
module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

type primitive =
  | Const of Ast.const
  | Var of string

type rexpr = (* expr in their RC IR *)
  | RConst of Ast.const
  | RCall of string * primitive list
  | RPartialApp of string * primitive list
  | RVarApp of string * primitive  (* TODO: do we want n-ary var-app? *)
  | RCtor of ctor
  | RProj of int * string
  | RReset of string
  | RReuse of string * ctor
and ctor = 
  | Ctor of { tag: int; fields: primitive list }
  | Signal of { head: primitive; tail: primitive }

type fn_body = 
  | FnRet of primitive
  | FnLet of string * rexpr * fn_body
  | FnCase of string * (int * fn_body) list
  | FnInc of string * fn_body
  | FnDec of string * fn_body

type rc_fun = Fun of string list * fn_body

type program = 
  | RefProg of { functions: (string * rc_fun) list; globals: (string * fn_body) list } 

let get_cases = List.map snd

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

let pp_primitive out = function
  | Var x -> Format.fprintf out "@{<lightcyan>%s@}" x
  | Const c -> Ast.pp_const out c

let pp_rexpr out = 
  let comma_separated pp out = Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out ", ") pp out in
  function
  | RConst c -> pp_primitive out (Const c)
  | RCall (c, ys) ->
      Format.fprintf out "@{<green>%s@}(%a)" c (comma_separated pp_primitive) ys
  | RPartialApp (c, ys) ->
      Format.fprintf out "@{<magenta>pap@} @{<yellow>%s@}(%a)" c (comma_separated pp_primitive) ys
  | RVarApp (x, y) ->
      Format.fprintf out "@{<lightcyan>%s@} %a" x pp_primitive y
  | RCtor Ctor {tag; fields} ->
      Format.fprintf out "@{<green>Ctor%d@}(%a)" tag (comma_separated pp_primitive) fields
  | RCtor Signal {head; tail} ->
      Format.fprintf out "@{<green>Signal@}(%a, %a)" pp_primitive head pp_primitive tail
  | RProj (i, x) ->
      Format.fprintf out "@{<yellow>proj_%d@} @{<lightcyan>%s@}" i x
  | RReset x ->
      Format.fprintf out "@{<magenta>reset@} @{<lightcyan>%s@}" x
  | RReuse (x, Ctor {tag; fields}) ->
      Format.fprintf out "@{<magenta>reuse@} @{<lightcyan>%s@} @{<magenta>in@} @{<green>Ctor%d@}(%a)"
        x tag (comma_separated pp_primitive) fields
  | RReuse (x, Signal {head; tail}) ->
      Format.fprintf out "@{<magenta>reuse@} @{<lightcyan>%s@} @{<magenta>in@} @{<green>Signal@}(%a, %a)"
        x pp_primitive head pp_primitive tail

let rec pp_fnbody out = function
  | FnRet x -> Format.fprintf out "@{<magenta>ret@} %a" pp_primitive x
  | FnLet (y, r, f) ->
        Format.fprintf out "@[<v 0>@{<magenta>let@} @{<lightcyan>%s@} = %a @{<magenta>in@}@,%a@]" y pp_rexpr r pp_fnbody f
  | FnCase (x, fs) ->
      let pp_branch out f =
        Format.fprintf out "@{<blue>|@} @[<hov 2>%a@]" pp_fnbody f
      in
      Format.fprintf out "@[<v 0>@{<magenta>match@} @{<lightcyan>%s@} @{<magenta>with@}@,%a@]" x
        (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out "@,") pp_branch) (get_cases fs)
  | FnInc (x, f) -> Format.fprintf out "@[<v 0>@{<magenta>inc@} @{<lightcyan>%s@};@,%a@]" x pp_fnbody f
  | FnDec (x, f) -> Format.fprintf out "@[<v 0>@{<magenta>dec@} @{<lightcyan>%s@};@,%a@]" x pp_fnbody f

let pp_rcfun ?ownerships:(own = None) out (name, Fun (params, body)) =
  let pp_ownership out o = match o with
    | Owned -> Format.fprintf out "@{<blue>Owned@}"
    | Borrowed -> Format.fprintf out "@{<orange>Borrowed@}"
  in
  let pp_params_with_ownership out (own: parameter_ownership) = 
    let own_list = StringMap.find_opt name own |> Option.value ~default:(List.map (fun _ -> Owned) params) in
    let params_with_ownership = List.combine params own_list in
    Format.fprintf out "%a" 
      (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out ", ")
        (fun out (p, o) -> Format.fprintf out "@{<lightcyan>%s@}: %a" p pp_ownership o)) params_with_ownership
  in
  Format.fprintf out "%a\n@{<magenta>fun@} @{<yellow>%s@}(%a) =@,  @[<v>%a@]" 
    (Format.pp_print_option pp_params_with_ownership) own
    name
    (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out ", ")
        (fun out p -> Format.fprintf out "@{<lightcyan>%s@}" p)) params
    pp_fnbody body

let pp_ref_counted_program ?ownerships:(own= None) out (RefProg { functions; globals }: program) =
  let pp_named_fnbody out (name, body) = Format.fprintf out "@{<magenta>let@} @{<lightcyan>%s@} = %a" name pp_fnbody body in
  Format.fprintf out "%a%a"
    (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out "@\n\n") pp_named_fnbody) globals
    (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out "@\n\n") (pp_rcfun ~ownerships:own)) functions


let rec eq_program (RefProg { functions = f1; globals = g1 }:program) (RefProg { functions = f2; globals = g2 }:program) = 
  List.length f1 = List.length f2
  && List.length g1 = List.length g2
  && List.for_all2 eq_fn f1 f2
  && List.for_all2 (fun (n1, b1) (n2, b2) -> n1 = n2 && eq_fnbody b1 b2) g1 g2
and eq_fn (n1, Fun (p1, b1)) (n2, Fun (p2, b2)) = 
  n1 = n2 
  && List.for_all2 String.equal p1 p2 
  && eq_fnbody b1 b2
and eq_fnbody f1 f2 =
  match f1, f2 with
  | FnRet p1, FnRet p2 -> p1 = p2
  | FnLet (x1, r1, f1), FnLet (x2, r2, f2) -> x1 = x2 && r1 = r2 && eq_fnbody f1 f2
  | FnCase (x1, fs1), FnCase (x2, fs2) ->
      x1 = x2 &&
      List.length fs1 = List.length fs2 &&
      List.for_all2 (fun (i1, f1) (i2, f2) -> i1 = i2 && eq_fnbody f1 f2) fs1 fs2
  | FnInc (x1, f1), FnInc (x2, f2) -> x1 = x2 && eq_fnbody f1 f2
  | FnDec (x1, f1), FnDec (x2, f2) -> x1 = x2 && eq_fnbody f1 f2
  | _ -> false
and eq_rexpr a b = match a, b with
  | RCall (c1, ps1), RCall (c2, ps2) -> c1 = c2 && ps1 = ps2
  | RPartialApp (c1, ps1), RPartialApp (c2, ps2) -> c1 = c2 && ps1 = ps2
  | RVarApp (x1, p1), RVarApp (x2, p2) -> x1 = x2 && p1 = p2
  | RCtor Ctor { tag = t1; fields = f1 }, RCtor Ctor { tag = t2; fields = f2 } -> t1 = t2 && f1 = f2
  | RCtor Signal { head = h1; tail = t1 }, RCtor Signal { head = h2; tail = t2 } -> h1 = h2 && t1 = t2
  | RProj (i1, x1), RProj (i2, x2) -> i1 = i2 && x1 = x2
  | RReset x1, RReset x2 -> x1 = x2
  | RReuse (x1, c1), RReuse (x2, c2) -> x1 = x2 && eq_rexpr_ctor c1 c2
  | _ -> false
and eq_rexpr_ctor a b = match a, b with
  | Ctor { tag = t1; fields = f1 }, Ctor { tag = t2; fields = f2 } -> t1 = t2 && f1 = f2
  | Signal { head = h1; tail = t1 }, Signal { head = h2; tail = t2 } -> h1 = h2 && t1 = t2
  | _ -> false

let add_if_member (p:primitive) (env: StringSet.t) : StringSet.t =
  match p with
  | Var x -> if StringSet.mem x env then env else StringSet.add x env
  | Const _ -> env

let check_var env x = if StringSet.mem x env then StringSet.empty else StringSet.singleton x

let name_of_primitive_opt = function
  | Var x -> Some x
  | Const _ -> None

(** all names (constants in the paper language) defined at a top - level
  Why not pass it through as parameters? Because it is called in multiple places, and it easier to assign global_names once at the beginning. *)
let global_names = ref StringSet.empty

(** Free variables of an [fn_body] minus all global names *)
let rec free_vars fn = StringSet.diff (free_vars_fn fn) !global_names
and free_vars_rexpr rexpr = StringSet.diff (free_vars_rexpr_inner rexpr) !global_names

(** Inner version so we don't have to do a diff on EVERY rexpr *)
and free_vars_rexpr_inner rexpr : StringSet.t = 
  match rexpr with
  | RConst _ -> StringSet.empty
  | RCall (c, args) | RPartialApp (c, args) ->
    StringSet.of_list (c :: List.filter_map name_of_primitive_opt args)
  | RVarApp (f, arg) -> 
    StringSet.add f (match name_of_primitive_opt arg with Some x -> StringSet.singleton x | None -> StringSet.empty)
  | RCtor Ctor { tag = _; fields } ->
      List.fold_left (fun acc y -> add_if_member y acc) StringSet.empty fields
  | RProj (_, name) -> StringSet.singleton name
  | RCtor Signal { head; tail } ->
    StringSet.of_list @@ List.filter_map name_of_primitive_opt [head; tail]
  | RReset name -> StringSet.singleton name
  | RReuse (name, Ctor { fields; _ }) ->
    StringSet.of_list (name :: List.filter_map name_of_primitive_opt fields)
  | RReuse (name, Signal {head; tail}) ->
    StringSet.of_list (name :: List.filter_map name_of_primitive_opt [head; tail])
and free_vars_fn = function
  | FnRet x -> 
    name_of_primitive_opt x
    |> Option.fold ~none:StringSet.empty ~some:StringSet.singleton
  | FnLet (x, rhs, f) -> 
    StringSet.union (free_vars_rexpr_inner rhs) (StringSet.remove x @@ free_vars f)
  | FnCase (scrutinee, branches) ->
    StringSet.singleton scrutinee
    |> StringSet.union (List.fold_left (fun acc (_, branch) -> StringSet.union acc (free_vars branch)) StringSet.empty branches)
  | FnInc (v, f) | FnDec (v, f) ->
    StringSet.union (StringSet.singleton v) (free_vars f)

(** Helper function to prepare [x] for use in an owned context. *)
let insert_inc (x:primitive) v f beta_env = 
  match x with
  | Const _ -> f
  | Var x -> 
    match lookup beta_env x with
    | Owned when not (StringSet.mem x v) -> f
    | _ -> FnInc (x, f)

(** Inserts a decrement instruction if [x] is owned and no longer needed in [f] *)
let insert_dec (x:primitive) f env =
  match x with
  | Const _ -> f
  | Var x ->
    match lookup env x with
    | Owned when not (StringSet.mem x (free_vars f)) -> FnDec (x, f)
    | _ -> f

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
    let compiled_fs = List.map (fun (n, f) -> (n, insert_dec_many ys (insert_rc f var_ownerships func_ownerships) var_ownerships)) fs in
    FnCase (x, compiled_fs) 
  (* The Lets*)
  | FnLet (y, RConst c, f) -> FnLet (y, RConst c, insert_rc f var_ownerships func_ownerships)
  | FnLet (y, RProj (i, x), f) -> 
    (match lookup var_ownerships x with
    | Owned ->
      let compiled_f = insert_dec (Var x) (insert_rc f var_ownerships func_ownerships) var_ownerships in
      FnLet (y, RProj (i, x), FnInc(y, compiled_f))
    | Borrowed -> 
      let beta_env' = StringMap.add y Borrowed var_ownerships in
      let compiled_f = insert_rc f beta_env' func_ownerships in
      FnLet (y, RProj (i, x), compiled_f)
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
  | FnLet (z, (RCtor Ctor { fields; _ } as ctor), f) ->
    let bs = List.map (fun _ -> Owned) fields in
    let compiled_f = insert_rc f var_ownerships func_ownerships in
    c_app fields bs (FnLet (z, ctor, compiled_f)) var_ownerships
  | FnLet (z, RCtor Signal { head; tail }, f) ->
    let compiled_f = insert_rc f var_ownerships func_ownerships in
    c_app [head; tail] [Owned; Owned] (FnLet (z, RCtor (Signal { head; tail }), compiled_f)) var_ownerships
  | FnLet (z, RReset x, f) ->
    let compiled_f = insert_rc f var_ownerships func_ownerships in FnLet (z, RReset x, compiled_f)
  | FnLet (z, (RReuse (_, Signal { head; tail }) as reuse), f) -> 
    let compiled_f = insert_rc f var_ownerships func_ownerships in
    c_app [head; tail] [Owned; Owned] (FnLet (z, reuse, compiled_f)) var_ownerships
  | FnLet (z, (RReuse (_, Ctor { fields; _ }) as reuse), f) ->
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
  | _ -> failwith "c_app: mismatch - more variables than ownership annotations"

(** Collects variables of an [fn_body] that can not be marked as [Borrowed].
    Ullirch & De Moura call it 'collect' *)
let rec collect func_ownerships (_f:fn_body) : StringSet.t = 
  match _f with
  | FnRet _ -> StringSet.empty
  | FnCase (_, cases) ->
    List.fold_left (fun acc case -> StringSet.union acc (collect func_ownerships case)) StringSet.empty (get_cases cases)
  | FnInc _ | FnDec _ -> failwith "no inc/dec in collect"
  (* The lets *)
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

let insert_owned_partial_app_wrapper func_ownership (RefProg{functions; globals}: program) = 
  
  let all_owned_funcs = ref [] in
  let rec aux f = 
    match f with
    | FnRet _ -> f
    | FnLet (x, RPartialApp (c, args), let_body) when List.exists (fun b -> not (is_owned b)) (lookup_params func_ownership c) -> 
      let opt = List.find_opt (fun (name, _) -> name = c) functions in
      (match opt with
      | None -> failwith ("prepare_for_collect: missing function " ^ c)
      | Some (_, Fun (params, _)) ->
        let function_name = Utilities.new_name (c ^ "_ALL_OWNED") in
        let ret = Utilities.new_var () in
        let body = FnLet (ret, RCall (c, List.map (fun p -> Var p) params), FnRet (Var ret)) in
        let ownerships = List.map (fun _ -> Owned) params in
        all_owned_funcs := (function_name, Fun (params, body), ownerships) :: !all_owned_funcs;
        FnLet (x, RPartialApp (function_name, args), aux let_body)
      )
    | FnLet (x, rhs, f)  -> FnLet (x, rhs, aux f)
    | FnDec (x, f) -> FnDec (x, aux f)
    | FnInc (x, f) -> FnInc (x, aux f)
    | FnCase (x, cases) -> 
      FnCase (x, List.map (fun (fields, body) -> (fields, aux body)) cases)
  in 
  let functions' = List.map (fun (name, Fun(params, body)) -> name, Fun(params, aux body)) functions in
  let functions' = (List.map (fun (name, f, _) -> name, f) !all_owned_funcs) @ functions' in
  let globals' = List.map (fun (name, body) -> name, aux body) globals in
  let func_ownership' = List.fold_left (fun m (name, _, ownerships) -> StringMap.add name ownerships m) func_ownership !all_owned_funcs in
  RefProg { functions = functions'; globals = globals' }, func_ownership'

let infer_all_simple (RefProg{functions; _}:program) : parameter_ownership =
  let func_ownership = ref StringMap.empty in
  List.iter (fun (c, Fun (params, _)) ->
    func_ownership := StringMap.add c (List.map (fun _ -> Borrowed) params) !func_ownership
  ) functions;

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
    ) functions
  done;
  !func_ownership


(* callers_of[g] = set of functions that call g *)
let build_callers functions : StringSet.t StringMap.t =
  let add_caller callers ~callee ~caller =
    let s = Option.value (StringMap.find_opt callee callers) ~default:StringSet.empty in
    StringMap.add callee (StringSet.add caller s) callers
  in
  let rec scan_body caller callers = function
    | FnRet _ -> callers
    | FnInc (_, f) | FnDec (_, f) -> scan_body caller callers f
    | FnCase (_, arms) ->
        List.fold_left (scan_body caller) callers (get_cases arms)
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

let infer_all ?(builtins:parameter_ownership = StringMap.empty) (RefProg{functions;_}:program) : parameter_ownership =
  let callers = build_callers functions in

  (* init: all Borrowed *)
  let beta0 =
    List.fold_left
      (fun b (name, Fun (params, _body)) ->
        StringMap.add name (List.map (fun _ -> Borrowed) params) b)
      builtins functions
  in

  let prog_map =
    List.fold_left (fun m (n, fn) -> StringMap.add n fn m) StringMap.empty functions
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
  List.iter (fun (n, _) -> enqueue n) functions;

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
let rec insert_reset_and_reuse_pairs_program (RefProg{functions; globals}: program) : program =
  let globals' = List.map (fun (name, body) -> name, insert_reset_and_reuse_pairs_fn body) globals in
  let functions' = List.map (fun (name, Fun(params, body)) -> name, (Fun(params, insert_reset_and_reuse_pairs_fn body))) functions
  in RefProg { functions = functions'; globals = globals' }

(** Inserts reset/reuse pairs into [body]. Ullrich & De Moura call it 'R' *)
and insert_reset_and_reuse_pairs_fn body = match body with
  | FnRet _ -> body
  | FnLet (x, e, f) -> FnLet (x, e, insert_reset_and_reuse_pairs_fn f)
  | FnCase (x, cases) ->                                  (*______D_____ x   ni             R(c)                        *)
    FnCase (x, List.map (fun (num_fields,c) -> (num_fields, insert_reset x num_fields (insert_reset_and_reuse_pairs_fn c))) cases)
  | FnInc _ | FnDec _ -> 
    failwith "no inc/dec should exist before reset/reuse transformation - this transformation should be called before 'insert_rc'"

(** Inserts resets of variable [z] with number of constructor fields [num_fields] into [fn_body]. 
    Ullrich & De Moura call it 'D' *)
and insert_reset z num_fields fn_body = 
  match fn_body with
  | FnCase (s, cases) -> FnCase (s, List.map (fun (i, arm) -> (i, insert_reset z num_fields arm)) cases)
  | FnRet _ -> fn_body
  | FnLet (x, e, f) when StringSet.mem z (free_vars_rexpr e) || StringSet.mem z (free_vars f) -> 
    (* (z in e) or (z in F) *)
    FnLet (x, e, insert_reset z num_fields f)
  | FnInc _ | FnDec _ -> failwith "no inc/dec should exist before reset/reuse transformation - this transformation should be called before 'insert_rc'"
  | _ ->
    let w = Utilities.new_var () in
    let reuse_fn_body = insert_reuse w num_fields fn_body in
    if not (eq_fnbody reuse_fn_body fn_body) then
      FnLet(w, RReset z, reuse_fn_body)
    else
      fn_body

(** Inserts reuse instructions into [fn_body] for variable [w] with number of constructor fields [num_fields] 
    Ullrich & De Moura call it 'S' *)
and insert_reuse w num_fields fn_body: fn_body =
  match fn_body with
  | FnCase (s, cases) -> FnCase (s, List.map (fun (i,c) -> i, insert_reuse w num_fields c) cases)
  | FnRet _ -> fn_body
  | FnLet (x, RCtor Ctor {tag; fields}, f) when List.length fields = num_fields ->
      FnLet (x, RReuse (w, Ctor {tag; fields}), f) 
  | FnLet (x, RCtor Signal { head; tail }, f) when num_fields = 5 ->
      FnLet (x, RReuse (w, Signal { head = head; tail = tail }), f)
   (* if w is free in e or F, we need to insert reuse in F *)
  | FnLet (x, e, f) -> FnLet (x, e, insert_reuse w num_fields f)
  | FnInc _ | FnDec _ -> failwith "no inc/dec should exist before reset/reuse transformation - this transformation should be called before 'insert_rc'"

let reference_count_program builtins (RefProg{globals; functions} as p:program) =
  global_names := StringSet.of_list (List.map fst globals @ List.map fst functions @ (StringMap.to_list builtins |> List.map fst));
  let reset_reuse_program = insert_reset_and_reuse_pairs_program p in
  let func_ownerships = infer_all ~builtins:builtins reset_reuse_program in
  
  let RefProg{globals = g; functions = f}, func_ownerships = insert_owned_partial_app_wrapper func_ownerships reset_reuse_program in
  global_names := StringSet.of_list (List.map fst g @ List.map fst f @ (StringMap.to_list builtins |> List.map fst));
  
  (* make all global variables borrowed ?*)
  let globals_env = List.fold_left (fun env (name, _) -> StringMap.add name Borrowed env) StringMap.empty globals in

  let functions' = f
  |> List.map (fun (c_name, Fun (params, c_body)) ->
      let params_ownership = lookup_params func_ownerships c_name in
      let var_env = List.fold_left2 (fun env param ownership -> StringMap.add param ownership env) globals_env params params_ownership in
      let ref_counted_body = insert_rc c_body var_env func_ownerships in
      (c_name, Fun (params, insert_dec_many (List.map (fun s -> Var s) params) ref_counted_body var_env)))
  in

  (* USE BEFORE DECLARATION on globals should have been caught by type checker, we assume good-to-go
    let y = x + 5
    let x = 5 *)
  let globals' = List.map (fun (name, body) -> name, insert_rc body globals_env func_ownerships) g in
  (func_ownerships, RefProg { functions = functions'; globals = globals' })