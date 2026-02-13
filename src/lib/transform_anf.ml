open Ast

let is_name = function
| EVar _ -> true
| _ -> false

let new_var () = Utilities.new_var ()

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
| EIfe (cond, e1, e2) -> 
  normalize_name cond (fun cond' -> 
    let e1' = normalize e1 k in
    let e2' = normalize e2 k in
    EIfe (cond', e1', e2')
  )
| ECase (x, cases) ->
  (* we cannot allow creating let #var = case ... in
    id (case ls of (Nil) (tail ls) =>
    must become:
    case ls of
    (id Nil)
    (id (tail ls)) *)
  normalize_name x (fun x' -> ECase (x', List.map (fun (p, c') -> (p, normalize c' k)) cases))
| EApp (f, args) -> 
  normalize_name f (fun t -> normalize_name_mult args (fun ts -> k (EApp (t, ts))))
| EBinary (op, e1, e2) ->
  normalize_name e1 (fun e1' -> normalize_name e2 (fun e2' -> k (EBinary (op, e1', e2'))))
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
(* and normalize_pattern p name acc = 
  match p with
  | PWildcard | PConst _ -> acc
  | PVar x -> failwith ""
  | PTuple (p1, p2) as p -> 
    let p1 = fst p in let p2 = snd p in failwith ""
    (* match mytuple with 
      | (x,y) -> x + y
      becomes
      case mytuple of 
      ( let x = proj1 mytuple in let y = proj2 mytuple i x + y )
    *)

    (* match x with (p1, p2) => let p1 = fst x in let p2 = snd p2 in body ? *)
    (* match x with (p1, (p2, p3) => let p1 = fst x in let p2 = )*)
    (* let acc = normalize_pattern p1 acc in

    normalize_pattern p2 acc *)
  | _ -> acc *)

let normalize_program (p: program) : program =
  List.map (fun (TLet (x,e)) -> TLet (x, normalize_expr e)) p