open Ast
open Refcount

module ANF = struct  
  let is_name = function
  | EVar _ -> true
  | _ -> false

  let new_var = 
    let cnt = ref 0 in
    fun () -> incr cnt; Printf.sprintf "#var%d" !cnt

  (* Pretty much exactly: Flanagan et. al 1993 *)
  let rec normalize_expr m = normalize m Fun.id
  and normalize (m:expr) k = match m with
  | EConst _ | EVar _ -> k m
  | EFun (params, body) -> k (EFun (params, normalize_expr body))
  | ELet (x, m1, m2) -> normalize m1 (
    fun n1 -> 
      let body = normalize m2 k in
      ELet(x, n1, body)
    )
  | ECase (x, cases) ->
    (* we cannot allow creating let #var = case ... in
      id (case ls of (Nil) (tail ls) =>
      must become:
      case ls of
      (id Nil)
      (id (tail ls)) *)
    normalize_name x (fun x' -> ECase (x', List.map (fun c' -> normalize c' k) cases))
  | EApp (f, args) -> 
    normalize_name f (fun t -> normalize_name_mult args (fun ts -> k (EApp (t, ts))))
  | EBinary (SigCons, e1, e2) ->
    (* convert to 
      let #var_e1 = e1 in 
      let #var_e2 = e2 in 
      let #var_s = #var_e1 :: #var_e2 in 
      EApp ("ref", [#var_s]) *)
    normalize_name e1 (fun e1' -> normalize_name e2 (fun e2' -> 
      let s = new_var () in
      let body = k (EApp (EVar "ref", [EVar s])) in
      ELet(s, EBinary (SigCons, e1', e2'), body)))
  (* TODO: general case binary op*)
  (* | EBinary (op, e1, e2) ->
    normalize_name e1 (fun e1' -> normalize_name e2 (fun e2' -> k (EBinary (op, e1', e2')))) *)
  | EUnary (op, e) -> normalize_name e (fun e' -> k (EUnary (op, e')))
  | ETuple (e1, e2) -> 
    normalize_name e1 (fun e1' -> normalize_name e2 (fun e2' -> k (ETuple (e1', e2'))))
  and normalize_name m k = (* TODO: can case cause problems here? *)
    normalize m (fun n -> 
      if is_name n then k n
      else let x = new_var () in let body = k (EVar x) in ELet (x, n, body))
  and normalize_name_mult ms k = match ms with
  | [] -> k []
  | m :: ms -> normalize_name m (fun t -> normalize_name_mult ms (fun t' -> k (t :: t')))

  let normalize_program (p: Ast.program) : Ast.program =
    List.map (fun (TLet (x,e)) -> TLet (x, normalize_expr e)) p
end

(* after all other transformations *)
let get_name = function
  | EVar x -> x
  | _ -> failwith "Assumption broken - expected all application arguments to be names"

let rec expr_to_rexpr (e: Ast.expr) : Refcount.rexpr = 
  match e with
  (* assumption: f (EVar x1) (EVar x2) (EVar x3) ... *)
  | EApp (EVar f, xs) -> RCall (f, List.map get_name xs)(* TODO: partial apps *)
  | EBinary (SigCons, EVar n1, EVar n2)
  | ETuple (EVar n1, EVar n2) -> RCtor (2, [n1; n2])
  | EUnary (Fst, EVar x)  -> RCall ("fst", [x])
  | EUnary (Snd, EVar x)  -> RCall ("snd", [x])
  | EConst (const) -> (
      match const with
      | CUnit -> RCtor (0, [])
      | CInt n -> RCtor (1, [string_of_int n])
      | CBool b -> RCtor (1, [string_of_bool b])
    )
  | _ -> failwith "invalid expression to convert"

and expr_to_fn_body (e: Ast.expr) : Refcount.fn_body = 
  match e with
  | EVar x -> FnRet x
  | ELet (x, rhs, e') ->
    FnLet(x, expr_to_rexpr rhs, expr_to_fn_body e')
  | ECase (EVar x, cases) -> 
    FnCase(x, List.map expr_to_fn_body cases)
  | ECase (e,cases) ->
    (* SHOULD NEVER HAPPEN but do 'case e of cases -> let x = e in case x of cases' *)
    let x = ANF.new_var () in 
    FnLet (x, expr_to_rexpr e, FnCase (x, List.map expr_to_fn_body cases))
  | _ -> 
    let x = ANF.new_var () in 
    FnLet (x, expr_to_rexpr e, FnRet x)

let to_rc_intermediate_representation (p: Ast.program) : Refcount.program =
  let to_fun = function
  | EFun (params, body) -> Fun (params, expr_to_fn_body (ANF.normalize_expr body)) 
  | _ -> failwith "TODO: only top level functions"
  in
  List.map (fun (TLet (name, rhs)) -> name, to_fun rhs) p
