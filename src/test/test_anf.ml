open Rizzoc
open Rizzoc.Ast
open Rizzoc.Transformations

let program_testable = Alcotest.testable Ast.pp_program Ast.eq_program
let expr_testable = Alcotest.testable Ast.pp_expr Ast.eq_expr

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

  (* let x = case y of (id y) | (let #var1 = id y in id #var1) *)
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

let test_anf_case_and_signal () =
  (* let my_sig = (case x of x | 0) :: (never)*)
  Utilities.new_name_reset ();
  let x = EVar "x" in
  let case = ECase(x, [x; EConst (CInt 0)]) in
  let e = EBinary(SigCons, case, EVar "never") in
  let transformed = ANF.anf_expr e in
  (* case x of 
     (let #var1 = x :: never in let my_sig = ref #var1)
     (let #var2 = 0 in let #var3 = #var2 :: never in let my_sig = ref #var3)
  *)
  Utilities.new_name_reset ();
  let varName1 = Utilities.new_var () in
  let varName2 = Utilities.new_var () in
  let varName3 = Utilities.new_var () in
  let expected =  ECase(x, [
      ELet(varName1, EBinary(SigCons, x, EVar "never"), EApp(EVar "ref", [EVar varName1]));
      ELet(varName2, EConst (CInt 0), 
        ELet(varName3, EBinary(SigCons, EVar varName2, EVar "never"), EApp(EVar "ref", [EVar varName3])))
    ])
  in
  Alcotest.check expr_testable "case and signal cons together" expected transformed

let test_case_is_lifted_out_of_let () = 
  (* let my_fun = fun x -> let y = case false of 0 | 1 in y :: x *)
  Utilities.new_name_reset ();
  let my_fun = EFun(["x"], 
    ELet("y", ECase(EConst (CBool false), [EConst (CInt 0); EConst (CInt 1)]), 
      EBinary(SigCons, EVar "y", EVar "x"))
  ) in
  let p:program = [
    TLet("my_fun", my_fun)
  ] in

  let transformed = ANF.anf p in

  (* let my_fun = fun x -> 
      let var1 = false in 
      case var1 of 
        (let y = 0 in let var2 = y :: x in ref var2) 
      | (let y = 1 in let var3 = y :: x in ref var3) *)
  Utilities.new_name_reset ();
  let var1 = Utilities.new_var () in
  let var2 = Utilities.new_var () in
  let var3 = Utilities.new_var () in
  let expected = [ TLet("my_fun", EFun(["x"], 
    ELet(var1, EConst (CBool false), 
    ECase(EVar var1, [
      ELet("y", EConst (CInt 0), ELet(var2, EBinary(SigCons, EVar "y", EVar "x"), EApp(EVar "ref", [EVar var2])));
      ELet("y", EConst (CInt 1), ELet(var3, EBinary(SigCons, EVar "y", EVar "x"), EApp(EVar "ref", [EVar var3])))
    ]))))] in
  Alcotest.check program_testable "case is lifted out of let" expected transformed


let anf_tests = [
  "case is lifted", `Quick, test_anf_case_is_lifted_to_top;
  "signal cons becomes ref", `Quick, test_anf_signal_cons_becomes_let;
  "case and signal cons together", `Quick, test_anf_case_and_signal;
  "case is lifted out of let", `Quick, test_case_is_lifted_out_of_let
]
