(*
 let mycons = Ctor3 (...) in
 case mycons of
 (do something ctor1)
 (do something ctor2)
 (do something ctor3) <-- 

 So for code:
 match myint with
 | 4 -> (do something if 4)
 | 5 -> (do something if 5)
 | _ -> (do something default)

 it becomes:
 let myint = Ctor1 5 in
 let boolRCtor = equality myint 4 in
 case boolRCtor of
 (if boolRCtor = RCtor0 do thing)
 (if boolRCtor = RCtor1
  do let boolRCtor2 = equality myint 5 in
  case boolRCtor2 of
  (if boolRCtor2 = RCtor0 do thing)
  (do default ...)) 


let mytuple = ETuple (RCtor 3, RCtor 4) in
match mytuple with
| ((3,4), c) -> (do something with a, b, c)
| (a,b) -> (do something with a, b)


Source:
match mytuple with
| ((3,4), c) -> e1
| (a,b) -> e2

Lowered shape (using your equality + bool case):
let t0 = proj_0 mytuple in
let t1 = proj_1 mytuple in
let t00 = proj_0 t0 in
let t01 = proj_1 t0 in
let b0 = equality t00 3 in
case b0 of
| True  ->
let b1 = equality t01 4 in
case b1 of
| True  -> let c = t1 in e1
| False -> let a = t0 in let b = t1 in e2
| False -> let a = t0 in let b = t1 in e2

*)
open Ast

(* todo: move these constants to some builtins *)
let left_elim = (Rizzo_builtins.get "left_elim").name
let right_elim = (Rizzo_builtins.get "right_elim").name
let both_elim_fst = (Rizzo_builtins.get "both_elim_fst").name
let both_elim_snd = (Rizzo_builtins.get "both_elim_snd").name

let rec transform_patterns (p: 's Ast.program) = 
  List.map (fun (TLet (name, expr, ann)) -> TLet (name, compile_match expr, ann)) p

and compile_pattern p scrutinee good bad = 
  match p with
  | PWildcard -> good scrutinee
  | PVar (s, ann) -> ELet ((s, ann), scrutinee, good (EVar (s, ann)), ann)
  | PConst (c, ann) ->
    let b = Utilities.new_name "test" in
    let equality = EApp (EVar ("eq", ann), [scrutinee; EConst (c, ann)], ann) in
    ELet ((b, ann), equality, EIfe (EVar (b, ann), good scrutinee, bad (), ann), ann)
  (* TODO: should we special case wildcards? or should this just be left to dead code analysis? *)
  | PTuple (PWildcard, PWildcard, _) -> good scrutinee
  | PTuple (p1, PWildcard, ann) -> 
    let t = Utilities.new_name "t", ann in
    ELet (t, EUnary (Snd, scrutinee, ann), compile_pattern p1 (EVar t) good bad, ann)
  | PTuple (PWildcard, p2, ann) -> 
    let t = Utilities.new_name "t", ann in
    ELet (t, EUnary (Snd, scrutinee, ann), compile_pattern p2 (EVar t) good bad, ann)
  | PTuple (p1, p2, ann) as pat ->
    let t0 = Utilities.new_name "t", ann in
    let t1 = Utilities.new_name "t", ann in
    (* TODO: we leave a match so later transformation stages can still
      emit how many fields a constructor has. Keep match/case or type info *)
    ECase(scrutinee, [(pat,
      ELet (t0, EUnary (Fst, scrutinee, ann), compile_pattern p1 (EVar t0) 
      (fun _ -> ELet (t1, EUnary (Snd, scrutinee, ann), compile_pattern p2 (EVar t1) good bad, ann))
      bad, ann), ann)], expr_get_ann scrutinee)
  | PSigCons (hd, (_tl_name, tl_ann as tl), ann) -> 
    let hd_name = Utilities.new_name "hd",ann in
    let hd_proj = EApp (EVar hd_name, [scrutinee], ann) in
    ECase (scrutinee, [( (* p -> let hd = head scrutinee in let tl = tail scrutinee ... *)
      p, ELet (hd_name, hd_proj, compile_pattern hd (EVar hd_name) 
      (fun _ -> ELet (tl, EUnary (UTail, scrutinee, tl_ann), good (EVar tl), ann))
      bad, ann), ann)], ann)
  | PLeft (p, ann) -> 
    let t = Utilities.new_name "t", ann in
    let t_proj = EApp (EVar (left_elim,ann), [scrutinee], ann) in
    (* (Left p) as x -> let t = rz_left_elim x in (compile p ...) *)
    ECase (scrutinee, [(p, ELet(t, t_proj, compile_pattern p scrutinee good bad, ann), ann)], ann)
  | PRight (p, ann) -> 
    let t = Utilities.new_name "t", ann in
    let t_proj = EApp (EVar (right_elim, ann), [scrutinee], ann) in
    (* (Right p) as x -> let t = rz_right_elim x in (compile p ...) *)
    ECase (scrutinee, [(p, ELet(t, t_proj, compile_pattern p scrutinee good bad, ann), ann)], ann)
  | PBoth (p1, p2, ann) -> 
    (* Both (p1,p2) -> let a = both_left scrutinee in let b = both_right scrutinee in ...*)
    let a_name = Utilities.new_name "p", ann in
    let b_name = Utilities.new_name "p", ann in
    let a_proj = EApp (EVar (both_elim_fst, ann), [scrutinee], ann) in
    let b_proj = EApp (EVar (both_elim_snd, ann), [scrutinee], ann) in
    let compiled_nested_patterns = 
      compile_pattern p1 (EVar a_name) 
        (fun _ -> ELet(b_name, b_proj, compile_pattern p2 (EVar b_name) good bad, ann))
        bad 
    in
    ECase(scrutinee, [(p, ELet(a_name, a_proj, compiled_nested_patterns, ann), ann)], ann)
and compile_match_cases scrutinee cases = 
  match cases with
  | [] -> failwith "Tried created another match branch - but there were no more cases in match expression"
  | (pat, case, _) :: rest -> 
    compile_pattern pat scrutinee
      (fun _ -> case) 
      (fun () -> compile_match_cases scrutinee rest)
and compile_match e = 
  match e with
  | EVar _ | EConst _ -> e 
  | ECase (scrutinee, cases, _) -> compile_match_cases scrutinee cases
  | ELet (name, e1, e2, ann) -> ELet (name, compile_match e1, compile_match e2, ann) 
  | EApp (e, args, ann) -> EApp (e, List.map compile_match args, ann)
  | EUnary (u, e, ann) -> EUnary (u, compile_match e, ann)
  | ETuple (e1, e2, ann) -> ETuple (compile_match e1, compile_match e2, ann)
  | EFun (args, body, ann) -> EFun (args, compile_match body, ann)
  | EBinary (op, e1, e2, ann) -> EBinary (op, compile_match e1, compile_match e2, ann)
  | EIfe (cond, e1, e2, ann) -> EIfe (compile_match cond, compile_match e1, compile_match e2, ann)

(* let rec expr_apply f e =
  let self = expr_apply f in
  match e with
  | EVar _ | EConst _ -> e 
  | ECase (scrutinee, cases, ann) -> ECase (self scrutinee, List.map (fun (p, c, a) -> (p, self c, a)) cases, ann)
  | ELet (name, e1, e2, ann) -> ELet (name, self e1, self e2, ann) 
  | EApp (e, args, ann) -> EApp (self e, List.map self args, ann)
  | EUnary (u, e, ann) -> EUnary (u, self e, ann)
  | ETuple (e1, e2, ann) -> ETuple (self e1, self e2, ann)
  | EFun (args, body, ann) -> EFun (args, self body, ann)
  | EBinary (op, e1, e2, ann) -> EBinary (op, self e1, self e2, ann)
  | EIfe (cond, e1, e2, ann) -> EIfe (self cond, self e1, self e2, ann)  *)