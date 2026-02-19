open Rizzoc.Ast
open Rizzoc.Transformations
open Ast_test_helpers

let test_copy_prop_removes_unused_let () =
  (* let y = x in z = 3 :: y in z *)
  let e = let_ "y" (var "x") (let_ "z" (binary SigCons (int 3) (var "y")) (var "z")) in
  let e' = eliminate_copy_propagation e in
  
  (* also does dead code elimination of the let y = x *)
  (* let z = 3 :: x in z *)
  let expected = let_ "z" (binary SigCons (int 3) (var "x")) (var "z") in

  Alcotest.(check expr_testable) "copy propagation removes unused let" expected e'

let test_copy_keeps_signal () = 
  (* let s = 3 :: y in 
     let map_s1 = map id s in
     let map_s2 = map id s in
     map_s1
  *)
  let e =
    let_ "s" (binary SigCons (int 3) (var "y"))
      (let_ "map_s1" (app (var "map") [var "id"; var "s"])
         (let_ "map_s2" (app (var "map") [var "id"; var "s"])
            (var "map_s1")))
  in
  let e' = eliminate_copy_propagation e in

  Alcotest.(check expr_testable) "copy propagation keeps signal if used multiple times" e e'

let test_copy_prop_chained_lets_eliminate_all () =
  (* let y = x in let z = y in z *)
  let e = let_ "y" (var "x") (let_ "z" (var "y") (var "z")) in
  let e' = eliminate_copy_propagation e in

  (* both lets are removed by copy propagation + DCE *)
  let expected = var "x" in

  Alcotest.(check expr_testable) "chained lets collapse" expected e'

let test_copy_prop_respects_fun_shadowing () =
  (* let x = y in fun x -> x *)
  let e = let_ "x" (var "y") (fun_ ["x"] (var "x")) in
  let e' = eliminate_copy_propagation e in

  (* outer let is removed, inner x is untouched *)
  (* only does local let elimination no copy propergation *)
  let expected = fun_ ["x"] (var "x") in

  Alcotest.(check expr_testable) "respects fun shadowing" expected e'

let test_copy_prop_keeps_non_var_let () =
  (* let y = 3 :: x in y *)
  let e = let_ "y" (binary SigCons (int 3) (var "x")) (var "y") in
  let e' = eliminate_copy_propagation e in

  (* non-var let is preserved *)
  let expected = e in

  Alcotest.(check expr_testable) "keeps non-var let" expected e'

let tests_copy_propagation = [
	"removes let", `Quick, test_copy_prop_removes_unused_let;
  "keeps signal if used multiple times", `Quick, test_copy_keeps_signal;
  "chained lets collapse", `Quick, test_copy_prop_chained_lets_eliminate_all;
  "respects fun shadowing", `Quick, test_copy_prop_respects_fun_shadowing;
  "keeps non-var let", `Quick, test_copy_prop_keeps_non_var_let;
]
