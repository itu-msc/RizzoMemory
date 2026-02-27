open Rizzoc
open Rizzoc.Ast
open Rizzoc.Transformations
open Ast_test_helpers

let test_anf_case_is_lifted_to_top () = 
  Utilities.new_name_reset ();

  let id = var "id" in
  let y = var "y" in
  let case_expr = case y [ (pvar "y", y); (pwild, app id [y]) ] in
  (* let x = id (case y of y; id y) *)
  let p : parsed program = [
    toplet "x" (app id [case_expr])
  ] in

  let transformed = ANF.anf p in

  (* let x = case y of (id y) | (let #var1 = id y in id #var1) *)
  Utilities.new_name_reset ();
  let varName = Utilities.new_var () in
  let varValue = var varName in
  let expected : parsed program = [
    toplet "x"
      (case y
         [ (pvar "y", app id [y]);
           (pwild, let_ varName (app id [y]) (app id [varValue])) ])
  ] in

  Alcotest.check program_testable "case is lifted to top level" expected transformed

let test_anf_signal_cons_unchanged () =
  Utilities.new_name_reset ();
  let x = var "x" in
  let y = var "y" in
  (* let z = x :: y *)
  let p : parsed program = [
    toplet "z" (binary SigCons x y)
  ] in

  let transformed = ANF.anf p in

  (* EXPECT UNALTERED PROGRAM  *)
  Utilities.new_name_reset ();
  let expected : parsed program = p in
  Alcotest.check program_testable "signal cons becomes let" expected transformed

let test_anf_case_and_signal () =
  (* (case x of x | 0) :: never *)
  Utilities.new_name_reset ();
  let x = var "x" in
  let case_expr = case x [ (pvar "x", x); (pwild, int 0) ] in
  let e = binary SigCons case_expr (var "never") in
  let transformed = ANF.anf_expr e in
  (* case x of 
     (x :: never)
     (let var1 = 0 in var1 :: never)
  *)
  Utilities.new_name_reset ();
  let expected =
    case x
      [ (pvar "x", binary SigCons x (var "never"));
        (pwild, binary SigCons (int 0) (var "never")) ]
  in
  Alcotest.check expr_testable "case and signal cons together" expected transformed

let test_case_is_lifted_out_of_let () = 
  (* let my_fun = fun x -> let y = (case false of 0 | 1) in y :: x *)
  Utilities.new_name_reset ();
  let my_fun =
    fun_ ["x"]
      (let_ "y"
         (case (bool false) [ (pconst (CBool false), int 0); (pwild, int 1) ])
         (binary SigCons (var "y") (var "x")))
  in
  let p : parsed program = [
    toplet "my_fun" my_fun
  ] in

  let transformed = ANF.anf p in

  (* let my_fun = fun x -> 
      let var1 = false in 
      case var1 of 
        (let y = 0 in y :: x) 
      | (let y = 1 in y :: x) *)
  Utilities.new_name_reset ();
  let expected =
    [ toplet "my_fun"
        (fun_ ["x"]
           (case (bool false)
              [ (pconst (CBool false), let_ "y" (int 0) (binary SigCons (var "y") (var "x")));
                (pwild, let_ "y" (int 1) (binary SigCons (var "y") (var "x"))) ])) ]
  in
  Alcotest.check program_testable "case is lifted out of let" expected transformed


let anf_tests = [
  "case is lifted", `Quick, test_anf_case_is_lifted_to_top;
  "signal cons becomes ref", `Quick, test_anf_signal_cons_unchanged;
  "case and signal cons together", `Quick, test_anf_case_and_signal;
  "case is lifted out of let", `Quick, test_case_is_lifted_out_of_let
]
