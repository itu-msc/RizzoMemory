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
let rec compile_pattern p scrutinee good bad = 
  match p with
  | PWildcard -> good scrutinee
  | PVar (s, ann) -> ELet ((s, ann), scrutinee, good (EVar (s, ann)), ann)
  | PConst (c, ann) ->
    let b = Utilities.new_name "test" in
    let equality = EApp (EVar ("equality", ann), [scrutinee; EConst (c, ann)], ann) in
    ELet ((b, ann), equality, EIfe (EVar (b, ann), good scrutinee, bad (), ann), ann)
  | PTuple (p1, p2, ann) ->
    let t0 = Utilities.new_name "t", ann in
    let t1 = Utilities.new_name "t", ann in
    ELet (t0, EUnary (Fst, scrutinee, ann), compile_pattern p1 (EVar t0) 
      (fun _ -> ELet (t1, EUnary (Snd, scrutinee, ann), compile_pattern p2 (EVar t1) good bad, ann))
      bad, ann)
  | _ -> failwith "todo"
and compile_match_cases scrutinee cases = 
  match cases with
  | [] -> failwith "Tried created another match branch - but there were no more cases in match expression"
  | (pat, case, _) :: rest -> 
    compile_pattern pat scrutinee
      (fun _ -> case) 
      (fun () -> compile_match_cases scrutinee rest)
and compile_match e = match e with
  | ECase (scrutinee, cases, _) -> compile_match_cases scrutinee cases
  | _ -> e
