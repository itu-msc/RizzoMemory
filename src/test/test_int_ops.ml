open Rizzoc
open Rizzoc.Ast
open! Rizzoc.RefCount

let parse_and_typecheck input =
  let parsed = Parser.parse_string input in
  let typed, errors = 
    let {typed_program; type_errors; _} : Rizzoc.typing_result = typecheck parsed in
    typed_program, type_errors
  in
  Alcotest.(check int) "type errors" 0 (List.length errors);
  typed

let test_typecheck_int_operators () =
  let typed =
    parse_and_typecheck
      ("let diff = 9 - 4\n"
      ^ "let product = 3 * 2\n"
      ^ "let quotient = 8 / 2\n"
      ^ "let remainder = 9 % 4\n"
      ^ "let less = 1 < 2\n"
      ^ "let more = 3 > 2\n"
      ^ "let atmost = 3 <= 4\n"
      ^ "let atleast = 4 >= 4\n")
  in
  match typed with
  | [ TopLet (_, EBinary (Sub, _, _, Ann_typed (_, diff_t)), _);
      TopLet (_, EBinary (Mul, _, _, Ann_typed (_, product_t)), _);
      TopLet (_, EBinary (Div, _, _, Ann_typed (_, quotient_t)), _);
      TopLet (_, EBinary (Mod, _, _, Ann_typed (_, remainder_t)), _);
      TopLet (_, EBinary (Lt, _, _, Ann_typed (_, less_t)), _);
      TopLet (_, EBinary (Gt, _, _, Ann_typed (_, more_t)), _);
      TopLet (_, EBinary (Leq, _, _, Ann_typed (_, atmost_t)), _);
      TopLet (_, EBinary (Geq, _, _, Ann_typed (_, atleast_t)), _) ] ->
      Alcotest.(check bool) "sub returns int" true (Ast.eq_typ diff_t TInt);
      Alcotest.(check bool) "mul returns int" true (Ast.eq_typ product_t TInt);
      Alcotest.(check bool) "div returns int" true (Ast.eq_typ quotient_t TInt);
      Alcotest.(check bool) "mod returns int" true (Ast.eq_typ remainder_t TInt);
      Alcotest.(check bool) "lt returns bool" true (Ast.eq_typ less_t TBool);
      Alcotest.(check bool) "gt returns bool" true (Ast.eq_typ more_t TBool);
      Alcotest.(check bool) "leq returns bool" true (Ast.eq_typ atmost_t TBool);
      Alcotest.(check bool) "geq returns bool" true (Ast.eq_typ atleast_t TBool)
  | _ -> Alcotest.fail "unexpected typed AST shape for int operators"

let test_rc_lowering_maps_mod_to_builtin () =
  let typed = parse_and_typecheck "let remainder = 5 % 2\n" in
  let lowered = lower_typed_program typed in
  Utilities.new_name_reset ();
  let actual = Transformations.ast_to_rc_ir Transformations.builtins lowered in
  match actual with
  | RefProg
      { functions = [];
        globals =
          [ ("remainder", FnLet (mod_var, RCall ("mod", [Const (CInt 5); Const (CInt 2)]), FnRet (Var mod_ret))) ] } ->
      Alcotest.(check string) "mod temp returned" mod_var mod_ret
  | _ -> Alcotest.fail "unexpected RC lowering shape for mod"

let test_rc_lowering_maps_greater_ops_to_builtins () =
  let typed =
    parse_and_typecheck
      ("let bigger = 5 > 2\n"
      ^ "let at_least = 5 >= 2\n")
  in
  let lowered = lower_typed_program typed in
  Utilities.new_name_reset ();
  let actual = Transformations.ast_to_rc_ir Transformations.builtins lowered in
  match actual with
  | RefProg
      { functions = [];
        globals =
          [ ("bigger", FnLet (gt_var, RCall ("gt", [Const (CInt 5); Const (CInt 2)]), FnRet (Var gt_ret)));
            ("at_least", FnLet (geq_var, RCall ("geq", [Const (CInt 5); Const (CInt 2)]), FnRet (Var geq_ret))) ] } ->
      Alcotest.(check string) "gt temp returned" gt_var gt_ret;
      Alcotest.(check string) "geq temp returned" geq_var geq_ret
  | _ -> Alcotest.fail "unexpected RC lowering shape for gt/geq"

let int_operator_tests =
  [
    "typecheck int operators", `Quick, test_typecheck_int_operators;
    "mod lowers to builtin call", `Quick, test_rc_lowering_maps_mod_to_builtin;
    "gt/geq lower to builtin calls", `Quick, test_rc_lowering_maps_greater_ops_to_builtins;
  ]
