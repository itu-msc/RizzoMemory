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

let test_copy_prop_folds_constant_if_condition () =
  (* let hit_wall = true in if hit_wall then 1 else 0 *)
  let e = let_ "hit_wall" (bool true) (ife (var "hit_wall") (int 1) (int 0)) in
  let e' = eliminate_copy_propagation e in

  Alcotest.(check expr_testable) "folds constant if condition" (int 1) e'

let test_copy_prop_reduces_let_in_if_condition () =
  (* let x = true in if (let y = x in y) then "true" else "false" *)
  let e =
    let_ "x" (bool true)
      (ife
        (let_ "y" (var "x") (var "y"))
        (str "true")
        (str "false"))
  in
  let e' = eliminate_copy_propagation e in

  Alcotest.(check expr_testable) "reduces let in if condition" (str "true") e'

let test_copy_prop_folds_constant_case_scrutinee () =
  (* let x = true in match x with | true -> 1 | false -> 0 *)
  let e =
    let_ "x" (bool true)
      (case (var "x") [
        (pconst (CBool true), int 1);
        (pconst (CBool false), int 0);
      ])
  in
  let e' = eliminate_copy_propagation e in

  Alcotest.(check expr_testable) "folds constant case scrutinee" (int 1) e'

let test_copy_prop_reduces_let_in_case_scrutinee () =
  (* let x = true in match (let y = x in y) with | true -> "true" | _ -> "false" *)
  let e =
    let_ "x" (bool true)
      (case
        (let_ "y" (var "x") (var "y"))
        [
          (pconst (CBool true), str "true");
          (pwild, str "false");
        ])
  in
  let e' = eliminate_copy_propagation e in

  Alcotest.(check expr_testable) "reduces let in case scrutinee" (str "true") e'

let test_copy_prop_folds_constant_binary_equality () =
  let true_eq_true = eliminate_copy_propagation (binary Eq (bool true) (bool true)) in
  let true_eq_false = eliminate_copy_propagation (binary Eq (bool true) (bool false)) in

  Alcotest.(check expr_testable) "true == true" (bool true) true_eq_true;
  Alcotest.(check expr_testable) "true == false" (bool false) true_eq_false

let test_copy_prop_folds_constant_int_arithmetic () =
  (* 1 + 2 + 3 *)
  let e = binary Add (binary Add (int 1) (int 2)) (int 3) in
  let e' = eliminate_copy_propagation e in

  Alcotest.(check expr_testable) "folds chained int add" (int 6) e'

let test_copy_prop_folds_constant_string_add () =
  (* "hello" + " world" + "!" *)
  let e = binary Add (binary Add (str "hello") (str " world")) (str "!") in
  let e' = eliminate_copy_propagation e in

  Alcotest.(check expr_testable) "folds chained string add" (str "hello world!") e'

let test_copy_prop_does_not_fold_division_by_zero () =
  let e = binary Div (int 1) (int 0) in
  let e' = eliminate_copy_propagation e in

  Alcotest.(check expr_testable) "does not fold division by zero" e e'

let tests_copy_propagation = [
	"removes let", `Quick, test_copy_prop_removes_unused_let;
  "keeps signal if used multiple times", `Quick, test_copy_keeps_signal;
  "chained lets collapse", `Quick, test_copy_prop_chained_lets_eliminate_all;
  "respects fun shadowing", `Quick, test_copy_prop_respects_fun_shadowing;
  "keeps non-var let", `Quick, test_copy_prop_keeps_non_var_let;
  "folds constant if condition", `Quick, test_copy_prop_folds_constant_if_condition;
  "reduces let in if condition", `Quick, test_copy_prop_reduces_let_in_if_condition;
  "folds constant case scrutinee", `Quick, test_copy_prop_folds_constant_case_scrutinee;
  "reduces let in case scrutinee", `Quick, test_copy_prop_reduces_let_in_case_scrutinee;
  "folds constant binary equality", `Quick, test_copy_prop_folds_constant_binary_equality;
  "folds constant int arithmetic", `Quick, test_copy_prop_folds_constant_int_arithmetic;
  "folds constant string add", `Quick, test_copy_prop_folds_constant_string_add;
  "does not fold division by zero", `Quick, test_copy_prop_does_not_fold_division_by_zero;
]
