open Ast

let is_name = function
| EVar _ -> true
| EConst _ -> true
| _ -> false

let new_var () = Utilities.new_var ()

(* Pretty much exactly: Flanagan et. al 1993 *)
let rec normalize_expr m = normalize m Fun.id
and normalize (m: _ expr) k = match m with
| EConst _ | EVar _ -> k m
| ECtor (name, args, loc) ->
  normalize_name_mult args (fun args' -> k (ECtor (name, args', loc)))
| EFun (params, body, loc) -> k (EFun (params, normalize_expr body, loc))
| EAnno (e, t, loc) -> k (EAnno (normalize_expr e, t, loc))
| ELet (x, m1, m2, loc) -> normalize m1 (
  fun n1 ->
    let body = normalize m2 k in
    ELet(x, n1, body, loc)
  )
| EIfe (cond, e1, e2, loc) -> 
  normalize_name cond (fun cond' -> 
    let e1' = normalize e1 k in
    let e2' = normalize e2 k in
    EIfe (cond', e1', e2', loc)
  )
| ECase (x, cases, loc) ->
  (* we cannot allow creating let #var = case ... in
    id (case ls of (Nil) (tail ls) =>
    must become:
    case ls of
    (id Nil)
    (id (tail ls)) *)
  normalize_name x (fun x' -> ECase (x', List.map (fun (p, c', l) -> (p, normalize c' k, l)) cases, loc))
| EApp (f, args, loc) -> 
  normalize_name f (fun t -> normalize_name_mult args (fun ts -> k (EApp (t, ts, loc))))
| EBinary (op, e1, e2, loc) ->
  normalize_name e1 (fun e1' -> normalize_name e2 (fun e2' -> k (EBinary (op, e1', e2', loc))))
| EUnary (op, e, loc) -> normalize_name e (fun e' -> k (EUnary (op, e', loc)))
| ETuple (e1, e2, loc) -> 
  normalize_name e1 (fun e1' -> normalize_name e2 (fun e2' -> k (ETuple (e1', e2', loc))))
and normalize_name m k = (* TODO: can case cause problems here? *)
  normalize m (fun n -> 
    if is_name n then k n
    else 
      let m_ann = expr_get_ann m in
      let x = (new_var (), m_ann) in let body = k (EVar x) in ELet (x, n, body, expr_get_ann n))
and normalize_name_mult ms k = match ms with
| [] -> k []
| m :: ms -> normalize_name m (fun t -> normalize_name_mult ms (fun t' -> k (t :: t')))

let normalize_program (p: parsed program) : parsed program =
  List.map (fun (TopLet (x,e, loc)) -> TopLet (x, normalize_expr e, loc)) p