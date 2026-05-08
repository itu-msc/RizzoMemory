open Ast

(*
Before Copy Propagation 
y = x
z = 3 + y

After Copy Propagation
z = 3 + x
*)
(*
  let x = y in
  if x then 1 else 0
  => 
  if y then 1 else 0
*)

let eliminate_copy_propagation (e: _ expr) : _ expr =
  let remove_keys keys env =
    List.filter (fun (k, _) -> not (List.mem k keys)) env
  in
  let rec const_of_expr = function
    | EConst (c, _) -> Some c
    | EAnno (e, _, _) -> const_of_expr e
    | _ -> None
  in
  let pattern_matches_const const = function
    | PConst (pattern_const, _) -> const = pattern_const
    | PVar _ | PWildcard _ -> true
    | _ -> false
  in
  let folded_binary op left right ann =
    match op, const_of_expr left, const_of_expr right with
    | Add, Some (CInt i), Some (CInt j) -> Some (EConst (CInt (i + j), ann))
    | Add, Some (CString s1), Some (CString s2) -> Some (EConst (CString (s1 ^ s2), ann))
    | Mul, Some (CInt i), Some (CInt j) -> Some (EConst (CInt (i * j), ann))
    | Sub, Some (CInt i), Some (CInt j) -> Some (EConst (CInt (i - j), ann))
    | Div, Some (CInt _), Some (CInt 0) -> None
    | Div, Some (CInt i), Some (CInt j) -> Some (EConst (CInt (i / j), ann))
    | Mod, Some (CInt _), Some (CInt 0) -> None
    | Mod, Some (CInt i), Some (CInt j) -> Some (EConst (CInt (i mod j), ann))
    | Eq, Some c1, Some c2 -> Some (EConst (CBool (c1 = c2), ann))
    | Lt, Some (CInt i), Some (CInt j) -> Some (EConst (CBool (i < j), ann))
    | Leq, Some (CInt i), Some (CInt j) -> Some (EConst (CBool (i <= j), ann))
    | Gt, Some (CInt i), Some (CInt j) -> Some (EConst (CBool (i > j), ann))
    | Geq, Some (CInt i), Some (CInt j) -> Some (EConst (CBool (i >= j), ann))
    | _ -> None
  in
  let folded_builtin_app name args ann =
    match name, List.map const_of_expr args with
    | "add", [Some (CInt i); Some (CInt j)] -> Some (EConst (CInt (i + j), ann))
    | "sub", [Some (CInt i); Some (CInt j)] -> Some (EConst (CInt (i - j), ann))
    | "mul", [Some (CInt i); Some (CInt j)] -> Some (EConst (CInt (i * j), ann))
    | "div", [Some (CInt _); Some (CInt 0)] -> None
    | "div", [Some (CInt i); Some (CInt j)] -> Some (EConst (CInt (i / j), ann))
    | "mod", [Some (CInt _); Some (CInt 0)] -> None
    | "mod", [Some (CInt i); Some (CInt j)] -> Some (EConst (CInt (i mod j), ann))
    | "eq", [Some c1; Some c2] -> Some (EConst (CBool (c1 = c2), ann))
    | "lt", [Some (CInt i); Some (CInt j)] -> Some (EConst (CBool (i < j), ann))
    | "leq", [Some (CInt i); Some (CInt j)] -> Some (EConst (CBool (i <= j), ann))
    | "gt", [Some (CInt i); Some (CInt j)] -> Some (EConst (CBool (i > j), ann))
    | "geq", [Some (CInt i); Some (CInt j)] -> Some (EConst (CBool (i >= j), ann))
    | "string_concat", [Some (CString s1); Some (CString s2)] -> Some (EConst (CString (s1 ^ s2), ann))
    | "string_eq", [Some (CString s1); Some (CString s2)] -> Some (EConst (CBool (s1 = s2), ann))
    | _ -> None
  in
  let rec aux (env: (string * _ expr) list) (e: _ expr) : _ expr =
    match e with
    | EConst _ -> e
    | EVar (x, _) -> Option.value (List.assoc_opt x env) ~default:e
    | ECtor (name, args, ann) -> ECtor (name, List.map (aux env) args, ann)
    | EApp (f, args, ann) ->
        let f' = aux env f in
        let args' = List.map (aux env) args in
        (match f' with
        | EVar (name, _) ->
            Option.value (folded_builtin_app name args' ann) ~default:(EApp (f', args', ann))
        | _ -> EApp (f', args', ann))
    | EBinary (op, e1, e2, ann) ->
        let e1' = aux env e1 in
        let e2' = aux env e2 in
        Option.value (folded_binary op e1' e2' ann) ~default:(EBinary (op, e1', e2', ann))
    | EUnary (op, e, ann) -> EUnary (op, aux env e, ann)
    | ELet ((s, _) as x, e1, e2, ann) ->
        let e1' = aux env e1 in
        (match e1' with
          | EVar _ (* let x = y *)
          | EConst _ (* let x = 3 *)
          | EAnno ((EVar _ | EConst _), _,_) -> (* let x = (y : t) or let x = (3 : t) *) 
            let env' = (s, e1') :: env in 
            aux env' e2
          | _ -> ELet (x, e1', aux env e2, ann)
        )
    | ETuple (e1, e2, ann) -> ETuple (aux env e1, aux env e2, ann)
    | EIfe (cond, e1, e2, ann) -> 
        (match aux env cond with
        | EConst (CBool true, _) -> aux env e1
        | EConst (CBool false, _) -> aux env e2
        | EAnno (EConst (CBool true, _), _, _) -> aux env e1
        | EAnno (EConst (CBool false, _), _, _) -> aux env e2
        | cond -> EIfe (cond, aux env e1, aux env e2, ann))
    | ECase (scrutinee, branches, ann) ->
        let scrutinee' = aux env scrutinee in
        (match const_of_expr scrutinee' with
        | Some const ->
            (match List.find_opt (fun (pattern, _, _) -> pattern_matches_const const pattern) branches with
            | Some (PVar (name, _), branch, _) -> aux ((name, scrutinee') :: env) branch
            | Some (_, branch, _) -> aux env branch
            | None -> ECase (scrutinee', [] , ann))
        | None ->
            ECase (
              scrutinee',
              List.map
                (fun (pattern, branch, ann) ->
                  let env' = remove_keys (Ast.pattern_bound_vars pattern) env in
                  (pattern, aux env' branch, ann))
                branches, ann))
    | EFun (params, body, ann) ->
        let params_bound = List.concat_map (Ast.Core.pattern_bound_vars) params in
        let env' = remove_keys params_bound env in
        EFun (params, aux env' body, ann)
    | EAnno (e, t, ann) -> EAnno (aux env e, t, ann)
  in
  aux [] e

let copy_propagate (p: _ program) : _ program =
  p
  |> List.map (function 
    | TopTypeDef _ as e -> e
    | TopLet (x, e, ann) -> TopLet(x, eliminate_copy_propagation e, ann))
