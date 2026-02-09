open Rizzoc
open Rizzoc.Ast
open Rizzoc.Transformations

let testable_expr = Alcotest.testable Ast.pp_expr Ast.eq_expr

let test_copy_prop_removes_unused_let () =
  (* let y = x in z = 3 :: y in z *)
  let e = ELet ("y", EVar "x", ELet ("z", EBinary (SigCons, EConst (CInt 3), EVar "y"), EVar "z")) in
  let e' = eliminate_copy_propagation e in
  
  (* also does dead code elimination of the let y = x *)
  (* let z = 3 :: x in z *)
  let expected = ELet ("z", EBinary(SigCons, EConst (CInt 3), EVar "x"), EVar "z") in

  Alcotest.(check testable_expr) "copy propagation removes unused let" expected e'

let test_copy_keeps_signal () = 
  (* let s = 3 :: y in 
     let map_s1 = map id s in
     let map_s2 = map id s in
     map_s1
  *)
  let e = ELet ("s", EBinary (SigCons, EConst (CInt 3), EVar "y"), 
            ELet ("map_s1", EApp (EVar "map", [EVar "id"; EVar "s"]),
            ELet ("map_s2", EApp (EVar "map", [EVar "id"; EVar "s"]),
            EVar "map_s1"))) in
  let e' = eliminate_copy_propagation e in

  Alcotest.(check testable_expr) "copy propagation keeps signal if used multiple times" e e'

let test_copy_prop_chained_lets_eliminate_all () =
  (* let y = x in let z = y in z *)
  let e = ELet ("y", EVar "x", ELet ("z", EVar "y", EVar "z")) in
  let e' = eliminate_copy_propagation e in

  (* both lets are removed by copy propagation + DCE *)
  let expected = EVar "x" in

  Alcotest.(check testable_expr) "chained lets collapse" expected e'

let test_copy_prop_respects_fun_shadowing () =
  (* let x = y in fun x -> x *)
  let e = ELet ("x", EVar "y", EFun (["x"], EVar "x")) in
  let e' = eliminate_copy_propagation e in

  (* outer let is removed, inner x is untouched *)
  (* only does local let elimination no copy propergation *)
  let expected = EFun (["x"], EVar "x") in

  Alcotest.(check testable_expr) "respects fun shadowing" expected e'

let test_copy_prop_keeps_non_var_let () =
  (* let y = 3 :: x in y *)
  let e = ELet ("y", EBinary (SigCons, EConst (CInt 3), EVar "x"), EVar "y") in
  let e' = eliminate_copy_propagation e in

  (* non-var let is preserved *)
  let expected = e in

  Alcotest.(check testable_expr) "keeps non-var let" expected e'

let tests_copy_propagation = [
	"removes let", `Quick, test_copy_prop_removes_unused_let;
  "keeps signal if used multiple times", `Quick, test_copy_keeps_signal;
  "chained lets collapse", `Quick, test_copy_prop_chained_lets_eliminate_all;
  "respects fun shadowing", `Quick, test_copy_prop_respects_fun_shadowing;
  "keeps non-var let", `Quick, test_copy_prop_keeps_non_var_let;
]
