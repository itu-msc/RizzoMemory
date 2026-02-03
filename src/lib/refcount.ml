open! Ast
module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

type rexpr = (* expr in their RC IR *)
  | RCall of string * string list
  | RPartialApp of string * string list
(*They write:
'All presented program transformations can be 
readily extended to a system with n-ary variable applications' 
*)
  | RVarApp of string * string 
  | RCtor of int * string list
  | RProj of int * string

type fn_body = 
  | FnRet of string
  | FnLet of string * rexpr * fn_body
  | FnCase of string * fn_body list
  | FnInc of string * fn_body
  | FnDec of string * fn_body

type fn = Fun of string list * fn_body

type program = (string * fn) list

let delta (p:program) (x:string) = List.assoc x p

let rec free_vars_expr (env: StringSet.t) = function
  | RCall (_c, ys) | RPartialApp (_c, ys) ->
      List.fold_left (fun acc y -> if StringSet.mem y env then acc else StringSet.add y acc) StringSet.empty ys
  | RVarApp (x, y) ->
      let acc = if StringSet.mem x env then StringSet.empty else StringSet.singleton x in
      if StringSet.mem y env then acc else StringSet.add y acc
  | RCtor (_i, ys) ->
      List.fold_left (fun acc y -> if StringSet.mem y env then acc else StringSet.add y acc) StringSet.empty ys
  | RProj (_i, x) ->
      if StringSet.mem x env then StringSet.empty else StringSet.singleton x
and free_vars_fn (env: StringSet.t) = function
  | FnRet x -> if StringSet.mem x env then StringSet.empty else (StringSet.singleton x)
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

type beta_env = ownership StringMap.t
let lookup env x = 
  match StringMap.find_opt x env with
  | None -> Owned
  | Some b -> b

type parameter_ownership = ownership list StringMap.t
let beta_global = ref StringMap.empty (* TODO *)
let lookup_params c = StringMap.find c !beta_global

let insert_inc x v f beta_env = match lookup beta_env x with
  | Owned when not (StringSet.mem x v) -> f
  | _ -> FnInc (x, f)

(* var -> rexpr -> beta_env -> rexpr *)
let insert_dec x f env = match lookup env x with
  | Owned when not (StringSet.mem x (free_vars f)) -> FnDec (x, f)
  | _ -> f

let rec insert_dec_many xs f beta_env = match xs with
  | [] -> f
  | x :: xs -> insert_dec_many xs (insert_dec x f beta_env) beta_env

(* delta(c) *)
let rec insert_rc (_f:fn_body) (ownerships: beta_env) : fn_body = match _f with
  | FnRet x -> insert_inc x StringSet.empty _f ownerships
  | FnCase (x, fs) as case -> 
    let ys = StringSet.to_list (free_vars case) in
    let compiled_fs = List.map (fun f -> insert_dec_many ys (insert_rc f ownerships) ownerships) fs in
    FnCase (x, compiled_fs)
  | FnLet (y, RProj (i, x), f) -> 
    (match lookup ownerships x with
    | Owned -> 
      let compiled_f = insert_dec x (insert_rc f ownerships) ownerships in
      FnLet (y, RProj (i, x), FnInc(y, compiled_f))
    | Borrowed -> 
      let beta_env' = StringMap.add y Borrowed ownerships in
      let compiled_f = insert_rc f beta_env' in
      FnLet (y, RProj (i, x), FnInc(y, compiled_f))
    )
  | FnLet (z, RCall(c, ys), f) -> 
    let compiled_f = insert_rc f ownerships in
    c_app ys (lookup_params c) (FnLet (z, RCall(c,ys), compiled_f)) ownerships
  | FnLet (z, RPartialApp(c, ys), f) ->
    let compiled_f = insert_rc f ownerships in
    c_app ys (lookup_params c) (FnLet (z, RPartialApp(c,ys), compiled_f)) ownerships
  | FnLet (z, RVarApp (x,y), f) ->
    let compiled_f = insert_rc f ownerships in
    c_app [x;y] [Owned; Owned] (FnLet (z, RVarApp(x,y), compiled_f)) ownerships
  | FnLet (z, RCtor(i,ys), f) ->
    let bs = List.map (fun _ -> Owned) ys in
    let compiled_f = insert_rc f ownerships in
    c_app ys bs (FnLet (z, RCtor(i,ys), compiled_f)) ownerships
  | FnInc _ | FnDec _ -> failwith "Increment and Decrement should not exist prior to this step!"

and c_app (vars: string list) (bs: ownership list) (_f:fn_body) beta_env : fn_body = 
  match vars, bs, _f with
  | y::ys', Owned::bs', FnLet (_,_, f) ->
    let alive_variables = StringSet.union (StringSet.of_list ys') (free_vars f) in
    insert_inc y alive_variables (c_app ys' bs' _f beta_env) beta_env
  | y::ys', Borrowed::bs', FnLet (z, e, f) ->
    let compiled_f = insert_dec y f beta_env in
    c_app ys' bs' (FnLet (z, e, compiled_f)) beta_env
  | [], _, FnLet(z, e, f) -> FnLet(z, e, f)
  | _ -> failwith "Not implemented"

let pp_rexpr out = function
  | RCall (c, ys) -> Format.fprintf out "%s(%a)" c (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out ", ") Format.pp_print_string) ys
  | RPartialApp (c, ys) -> Format.fprintf out "pap %s(%a)" c (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out ", ") Format.pp_print_string) ys
  | RVarApp (x, y) -> Format.fprintf out "%s %s" x y
  | RCtor (i, ys) -> Format.fprintf out "Ctor%d(%a)" i (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out ", ") Format.pp_print_string) ys
  | RProj (i, x) -> Format.fprintf out "proj_%d %s" i x

let rec pp_fnbody out = function
  | FnRet x -> Format.fprintf out "ret %a" Format.pp_print_string x
  | FnLet (y, r, f) ->
        Format.fprintf out "let %s = %a in@ %a" y pp_rexpr r pp_fnbody f
  | FnCase (x, fs) ->
      Format.fprintf out "case %s of@ @[<v>%a@]" x
        (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out "@ | ") pp_fnbody) fs
  | FnInc (x, f) -> Format.fprintf out "inc %s;@ %a" x pp_fnbody f
  | FnDec (x, f) -> Format.fprintf out "dec %s;@ %a" x pp_fnbody f
