(* 
 * Attempt at an implementation of 
 * Lorenzen, Anton, and Daan Leijen. “Reference Counting with Frame Limited Reuse.” Implementation and Benchmarks for “Reference Counting with Frame Limited Reuse” 6, no. ICFP (2022): 103:357-103:380. https://doi.org/10.1145/3547634.
 * *)

open! Lambda1
open! Lambda1.PP
open! Lambda1.Utilities
open! Lambda1.Factory

let return = Reusem.return
let (let*) = Reusem.bind

let consume resources acc = 
  VarMSet.fold_n (Fun.compose ldrop var) acc resources

let rec insert_dup_drop borrows resources e = 
  match e with
  | LVal v -> insert_dup_drop_val borrows resources v
  | LApp (f, Var x) -> 
    if not (VarMSet.mem x resources) then 
      (* there is no x in resources, so we must dup one, then try again *)
      insert_dup borrows resources x e
    else
      let delta = VarMSet.add x borrows in
      let gamma = VarMSet.pop x resources in
      let f' = insert_dup_drop delta gamma f in
      lapp f' (var x)
  | LLet (Var x, e1, e2) ->
    let vars_used_in_e2 = VarSet.diff (fv e2) (VarSet.singleton x) in
    let gamma1, gamma2 = 
      VarSet.fold (fun var (gamma1, gamma2) ->
        let cnt, gamma1 = VarMSet.pop_all var gamma1 in
        let gamma2 = VarMSet.add_many var cnt gamma2 in
        (gamma1, gamma2)
      ) vars_used_in_e2 (resources, VarMSet.add x VarMSet.empty)
    in
    let delta' = VarMSet.union gamma2 borrows in
    let e1' = insert_dup_drop delta' gamma1 e1 in
    let e2' = insert_dup_drop borrows gamma2 e2 in
    lletn x e1' e2'
  | LMatch (Var x, branches) ->
    let branches' = 
      branches
      |> List.map (
        fun (pat, body) -> 
          let zs = pattern_bindings pat in
          let resources' = VarMSet.union resources (VarMSet.of_key_list zs) in
          let body' = insert_dup_drop borrows resources' body in
          pat, List.fold_left (fun acc z -> ldupn z acc) body' zs)
    in
    if VarMSet.mem x resources || VarMSet.mem x borrows 
    then lmatch (var x) branches'
    else failwith "insert_dup_drop: match variable not in resources nor borrows"
  | LDrop _ -> failwith "insert_dup_drop: drop is inserted by consuming resources"
  | LDup _  -> failwith "insert_dup_drop: dup is inserted by insert_dup, not expected to be seen here"
  | LDropru _ -> failwith "insert_dup_drop: TODO: not sure what to do with dropru"

and insert_dup_drop_val borrows resources v = 
  match v with
  | VVar Var x when VarMSet.mem x resources -> 
    let resources' = VarMSet.pop x resources in
    consume resources' (lval v)
  | VVar Var x -> (* not in resources *)
    insert_dup borrows resources x (lval v)
  | VCtor { args = xs; _ } ->
    let vctor_with_dups, resources' = 
      List.fold_left (fun (expr, resources') (Var x) -> 
        if VarMSet.mem x resources' then (expr, VarMSet.pop x resources')
        else (insert_dup borrows resources' x expr, resources')
      ) (lval v, resources) xs
    in 
    consume resources' vctor_with_dups
  | VLambda {x = Var x; body; _} as lam -> 
    let free_vars = fv_val lam in
    let gamma' = VarMSet.add x (VarSet.fold VarMSet.add free_vars VarMSet.empty) in 
    let body' = insert_dup_drop VarMSet.empty gamma' body in
    let env = VarSet.to_list free_vars |> List.map var in
    let resources = VarSet.fold VarMSet.pop free_vars resources in
    consume resources (llambda ~env (var x) body')

and insert_dup delta gamma x e = 
  if VarMSet.mem x gamma || VarMSet.mem x delta 
  then 
    let gamma' = VarMSet.add x gamma in
    let e' = insert_dup_drop delta gamma' e in
    ldupn x e'
  else failwith (Format.asprintf "insert_dup: variable %s not in resources nor borrows" x)


(* module StringMap = Map.Make(String) *)
(* S : mapping from name to cell size *)
open Common
type size_env = int StringMap.t
(* R : mapping from name of reuse token to available heap size *)

let rec run_drop_guided_reuse e : lexpr =
  Reusem.run (drop_guided_reuse StringMap.empty e)

and  drop_guided_reuse (var_size : size_env) e : lexpr Reusem.t = 
  match e with
  | LVal v -> drop_guided_reuse_val var_size v
  | LApp (e, x) -> 
    let* e' = drop_guided_reuse var_size e in
    return (lapp e' x)
  | LDup (x, e) -> 
    let* e' = drop_guided_reuse var_size e in
    return (ldup x e')
  | LMatch (Var x as xvar, branches) ->
    let* branches' = 
      branches
      |> List.map (fun (PCtor(_, xs) as c, body) -> 
        let var_size' = StringMap.add x (List.length xs) var_size in
        let* body' = drop_guided_reuse var_size' body in
        return (c, body'))  
      |> Reusem.collect
    in
    return (lmatch xvar branches')
  | LDrop (Var x, e) ->
    let* is_reuse_token = Reusem.is_available x in 
    if is_reuse_token then (* RDROPR *)
      let* _ = Reusem.remove x in
      let* e' = drop_guided_reuse var_size e in
      return (ldropn x e')
    else if StringMap.mem x var_size then (* DECISION POINT - either use RDROP-REUSE or RDROP *)
      let n = StringMap.find x var_size in
      let r = Utilities.fresh_name "r" in
      let* () = Reusem.add_reuse_token r n in
      let* e' = drop_guided_reuse var_size e in
      let* is_reused = Reusem.is_reused r in
      if is_reused then return (ldropru (var r) (var x) e')
      else 
        let* () = Reusem.remove r in
        return (ldropn x e')
    else (* RDROP *)
      drop_guided_reuse var_size e
      |> Reusem.map (fun e' -> ldropn x e')
  | LLet (x, e1, e2) -> 
    let* e1' = Reusem.snapshot_restore (drop_guided_reuse var_size e1) in
    let* e2' = drop_guided_reuse var_size e2 in
    return (llet x e1' e2')
  | LDropru { r = Var r; x = Var x; e }  -> 
    let* () = Reusem.add_reuse_token r 0 in
    let* e' = drop_guided_reuse var_size e in
    let* () = Reusem.remove r in
    return (ldropru (var r) (var x) e')

(** insert reuse for leafs - should these consumes any remaining reuse tokens? *)
and drop_guided_reuse_val _ v : lexpr Reusem.t = 
  match v with
  | VVar _ -> return (lval v)
  | VLambda{env; x; body} -> 
    let body' = Reusem.run (drop_guided_reuse VarMSet.empty body) in
    return (llambda ~env x body')
  | VCtor {name; args; _} -> 
    let* reuse_token = Reusem.find_reusable_opt (List.length args) in
    match reuse_token with
    | None -> return (lval v) 
    | Some reuse -> return (lval @@ vctor_reusen name args reuse)
