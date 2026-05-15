module VarMSet = Multiset.Make(String)
module VarSet = Set.Make(String)

type var = Var of string
let get_name (Var x) = x
let get_names = List.map get_name

type pattern_leaf =
  | PWildcard
  | PVar of var
and pattern =
  | PCtor of string * pattern_leaf list

type value =
  | VVar of var
  | VCtor of { name: string; args: var list; reuse_token: var option }
  | VLambda of { env: var list; x: var; body: lexpr }

and lexpr = 
  | LVal of value
  | LApp of lexpr * var
  | LLet of var * lexpr * lexpr
  | LMatch of var * (pattern * lexpr) list
  | LDup of var * lexpr
  | LDrop of var * lexpr
  | LDropru of { r: var; x: var; e: lexpr }

type borrow_env = VarMSet.t
type resource_env = VarMSet.t

module Utilities = struct 
  let pattern_bindings p = 
    match p with
    | PCtor (_, xs) -> List.filter_map (function PVar v -> Some (get_name v) | _ -> None) xs
  let bv = pattern_bindings

  let rec fv e = 
    match e with
    | LVal v -> fv_val v
    | LApp (e, Var x) -> VarSet.add x (fv e)
    | LLet (Var x, e1, e2) -> VarSet.union (fv e1) (VarSet.remove x (fv e2))
    | LMatch (Var x, branches) -> 
      let fv_branches = 
        List.fold_left 
          (* acc + (fv e - bv pat) *)
          (fun acc (pat, e) -> VarSet.union acc (VarSet.diff (fv e) (VarSet.of_list (bv pat)))) 
          VarSet.empty 
          branches 
      in
      VarSet.add x fv_branches
    | LDup (Var x, e) -> VarSet.add x (fv e)
    | LDrop (Var x, e) -> VarSet.add x (fv e)
    | LDropru { r = Var r; x = Var x; e } -> VarSet.add r (VarSet.add x (fv e))
  and fv_val v = 
    match v with
    | VVar Var x -> VarSet.singleton x
    | VCtor { args = xs; reuse_token; _ } -> 
      let acc = match reuse_token with
        | None -> VarSet.empty
        | Some (Var r) -> VarSet.singleton r
      in
      List.fold_left (fun acc x -> VarSet.add x acc) acc (get_names xs)
    | VLambda { x = Var x; body; _ } -> VarSet.remove x (fv body)

  let fresh_name =
    let counter = ref 0 in
    fun prefix -> 
      let var = Format.asprintf "%s_%d" prefix !counter in
      incr counter;
      var
end

module PP = struct 
  let rec pp_lexpr out e = 
    match e with
    | LVal v -> pp_value out v
    | LApp (e, Var x) -> Format.fprintf out "%a %s" pp_lexpr e x
    | LLet (Var x, e1, e2) -> Format.fprintf out "let %s = %a in %a" x pp_lexpr e1 pp_lexpr e2
    | LMatch (Var x, branches) -> 
      Format.fprintf out "match %s with " x;
      List.iter (fun (pat, e) -> Format.fprintf out "| %a -> %a" pp_pattern pat pp_lexpr e) branches
    | LDup (Var x, e) -> Format.fprintf out "dup %s; %a" x pp_lexpr e
    | LDrop (Var x, e) -> Format.fprintf out "drop %s; %a" x pp_lexpr e
    | LDropru { r = Var r; x = Var x; e } -> Format.fprintf out "%s <- dropru %s; %a" r x pp_lexpr e
  and pp_value out v = 
    match v with
    | VVar Var x -> Format.fprintf out "%s" x
    | VCtor {name; args = xs; reuse_token = Some Var r} -> 
      Format.fprintf out "%s@%s(%s)" name r (String.concat ", " (get_names xs))
    | VCtor {name; args = xs; _ } -> 
      Format.fprintf out "%s(%s)" name (String.concat ", " (get_names xs))
    | VLambda {env = _; x = Var x; body} -> 
      Format.fprintf out "(\\%s -> %a)" x pp_lexpr body
  and pp_pattern out pat =
    match pat with
    | PCtor (name, xs) -> 
      let format_args = Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out ", ") pp_pattern_leaf in
      Format.fprintf out "%s(%a)" name format_args xs
  and pp_pattern_leaf out leaf = 
    match leaf with
    | PWildcard -> Format.fprintf out "_"
    | PVar v -> Format.fprintf out "%s" (get_name v)
    
  let pp_resources out res = 
    Format.fprintf out "%a" (VarMSet.pp Format.pp_print_string) res
end

module Factory = struct
  let var x = Var x
  let var_val x = VVar (Var x)

  let vvar x = VVar x
  let lvar x = LVal (vvar x)
  let vctor name args = VCtor {name; args; reuse_token = None} 
  let vctor0 name = VCtor { name; args = []; reuse_token = None }
  let vctor_reuse name args reuse_token = VCtor { name; args; reuse_token } 
  let vctor_reusen name args r = VCtor { name; args; reuse_token = Some (var r) } 
  let vlambda ?(env = []) x body = VLambda {env; x ; body}
  let llambda ?(env = []) x body = LVal (vlambda ~env x body)

  let lval v = LVal v
  let lapp f x = LApp (f, x)
  let llet v rhs body = LLet(v, rhs, body)
  let lletn name rhs body = LLet(Var name, rhs, body)
  let lmatch x branches = LMatch (x, branches)
  let ldup x e = LDup (x, e)
  let ldupn name e = LDup (Var name, e)
  let ldrop x e = LDrop(x, e)
  let ldropn name e = LDrop(Var name, e)
  let ldropru r x e = LDropru{ r ; x; e }

  let pwildcard () = PWildcard
  let pvar x = PVar x
  let pvarn n = pvar (var n)
  let pctor name args = PCtor(name, args)
  let pctorv name arg_vars = PCtor(name, List.map pvar arg_vars)
  let pctorn name arg_names = PCtor (name, List.map pvarn arg_names)
  let pctor0 name = PCtor(name, [])
end
