open! Common

type env = {
  available: int StringMap.t;
  reused: StringSet.t;
}

type 'a t = M of (env -> 'a * env)

let empty_env : env = {
  available = StringMap.empty;
  reused = StringSet.empty;
}

let run : 'a t -> 'a = 
  fun (M f) -> f empty_env |> fst

let run_env env (M f) = 
  f env |> fst

let run_get_env =
  fun (M f) -> f empty_env

let return x = M (fun env -> (x, env))

let bind (M m) f = M (
  fun state ->
    let (a, state') = m state in
    let M m' = f a in
    m' state') 
  
let map f m = bind m (Fun.compose return f)

let get_env : env t = M (fun env -> (env, env))
let modify f : unit t = M (fun env -> ((), f env))

module Operators = struct
  let (let*) = bind
end

let collect ls = 
  let open Operators in
  List.fold_left (
    fun acc c -> 
      let* xs = acc in 
      let* x = c in 
      return (x :: xs)) 
    (return []) ls
  |> map List.rev

let add_reuse_token r size = 
  modify (fun env -> 
    let available = StringMap.add r size env.available in
    { env with available }  
  )
let is_reused r : bool t = 
  get_env
  |> map (fun env -> StringSet.mem r env.reused)

let is_available r : bool t =
  get_env
  |> map (fun env -> StringMap.mem r env.available)

let remove r : unit t = 
  modify (fun env ->
    let available = StringMap.remove r env.available in
    { env with available }
  )

let find_reusable_opt n = 
  let open Operators in 
  let* env = get_env in
  let r = 
    env.available
    |> StringMap.bindings 
    |> List.find_opt (Fun.compose ((=) n) snd)
  in
  match r with
  | None -> return None
  | Some (r,_) ->
    let* _ = 
      modify (fun env ->
        let available = StringMap.remove r env.available in
        let reused = StringSet.add r env.reused in
        { available; reused })
    in
    return (Some r)

(** Snapshots before performing [action] then any unused reuse token not consumed by [action] is restored *)
let snapshot_restore action =
  let open Operators in
  let* before_env = get_env in
  let* result = action in
  modify (fun after_env -> 
    let available =
      after_env.available
      |> StringMap.filter (fun r _ -> not (StringSet.mem r after_env.reused))
      |> StringMap.union (fun _ v1 _ -> Some v1) before_env.available
    in
    { after_env with available }
  )
  |> map (fun () -> result)


let pp_env out env = 
  let reused_pp = Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out ", ") (fun out r -> Format.fprintf out "%s" r) in
  let available_pp = Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out ", ") (fun out (r, size) -> Format.fprintf out "%s: %d" r size) in
  Format.fprintf out "ReuseEnv { available = {%a}; reused = {%a} }" 
    available_pp (StringMap.bindings env.available)
    reused_pp (StringSet.to_list env.reused)

let println =
  get_env
  |> map (fun env -> Format.printf "%a\n" pp_env env)