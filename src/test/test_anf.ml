open Rizzoc
open Rizzoc.Ast
open Rizzoc.Transformations

let program_testable = Alcotest.testable Ast.pp_program Ast.eq_program

let test_anf_case_is_lifted_to_top () = 
  Utilities.new_name_reset ();

  let id = EVar "id" in
  let y = EVar "y" in
  let case = ECase(y, [y; EApp(id, [y])]) in
  (* let x = id (case y of y; id y) *)
  let p:program = [
    TLet("x", EApp(id, [case]))
  ] in

  let transformed = ANF.anf p in

  (* let x = case y of id y; let #var1 = id y in id #var1 *)
  Utilities.new_name_reset ();
  let varName = Utilities.new_var () in
  let varValue = EVar varName in
  let expected:program = [
    TLet("x", ECase(y, [
      EApp(id, [y]);
      ELet(varName, EApp (id, [y]), EApp(id, [varValue]))
    ]))
  ] in

  Alcotest.check program_testable "case is lifted to top level" expected transformed

let test_anf_signal_cons_becomes_let () =
  Utilities.new_name_reset ();
  let x = EVar "x" in
  let y = EVar "y" in
  (* let z = x :: y *)
  let p:program = [
    TLet("z", EBinary(SigCons, x, y))
  ] in

  let transformed = ANF.anf p in

  (* let #var1 = x :: y in ref (#var1) *)
  Utilities.new_name_reset ();
  let varName = Utilities.new_var () in
  let varValue = EVar varName in
  let ref_var = EVar "ref" in
  let expected:program = [
    TLet("z", ELet(varName, EBinary(SigCons, x, y), EApp(ref_var, [varValue])))
  ] in

  Alcotest.check program_testable "signal cons becomes let" expected transformed

let anf_tests = [
  "case is lifted", `Quick, test_anf_case_is_lifted_to_top;
  "signal cons becomes ref", `Quick, test_anf_signal_cons_becomes_let;
]
