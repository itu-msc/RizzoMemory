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
let head_elim = (Rizzo_builtins.get "head").name

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
    let hd_proj = EApp (EVar (head_elim, ann), [scrutinee], ann) in
    ECase (scrutinee, [( (* p -> let hd = head scrutinee in let tl = tail scrutinee ... *)
      p, ELet (hd_name, hd_proj, compile_pattern hd (EVar hd_name) 
      (fun _ -> ELet (tl, EUnary (UTail, scrutinee, tl_ann), good (EVar tl), ann))
      bad, ann), ann)], ann)
  | PCtor (_ctor_name, ps, ann) as pat ->
    let proj_of_i i = EUnary (UProj i, scrutinee, ann) in
    let names_and_projs = List.mapi (fun i _ -> (Utilities.new_name "p", ann), proj_of_i i) ps in
    let bindings = List.combine ps names_and_projs in
    let rec compile_fields bs =
      match bs with
      | [] -> good scrutinee
      | (subpat, (pname, pproj)) :: rest ->
        ELet (pname, pproj, compile_pattern subpat (EVar pname)
            (fun _ -> compile_fields rest) 
            bad,
          ann)
    in
    ECase (scrutinee, [ (pat, compile_fields bindings, ann) ], ann)

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
