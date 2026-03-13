open Rizzoc
open Rizzoc.Ast
open! Rizzoc.RefCount

let parse_and_typecheck input =
  let parsed = Parser.parse_string input in
  let typed, errors = typecheck parsed in
  Alcotest.(check int) "type errors" 0 (List.length errors);
  typed

let test_typecheck_not_operator () =
  let typed =
    parse_and_typecheck
      ("let flipped = !true\n"
      ^ "let named = not false\n")
  in
  match typed with
  | [ TopLet (_, EUnary (UNot, _, Ann_typed (_, flipped_t)), _);
      TopLet (_, EApp (EVar ("not", _), _, Ann_typed (_, named_t)), _) ] ->
      Alcotest.(check bool) "bang not returns bool" true (Ast.eq_typ flipped_t TBool);
      Alcotest.(check bool) "keyword not returns bool" true (Ast.eq_typ named_t TBool)
  | _ -> Alcotest.fail "unexpected typed AST shape for not operator"

let test_rc_lowering_maps_not_to_builtin () =
  let typed = parse_and_typecheck "let flipped = !true\n" in
  let lowered = lower_typed_program typed in
  Utilities.new_name_reset ();
  let actual = Transformations.ast_to_rc_ir Transformations.builtins lowered in
  match actual with
  | RefProg
      { functions = [];
        globals =
          [ ("flipped", FnLet (tmp, RCall ("not", [Const (CBool true)]), FnRet (Var ret))) ] } ->
      Alcotest.(check string) "not temp returned" tmp ret
  | _ -> Alcotest.fail "unexpected RC lowering shape for not"

let bool_operator_tests =
  [
    "typecheck not operator", `Quick, test_typecheck_not_operator;
    "not lowers to builtin call", `Quick, test_rc_lowering_maps_not_to_builtin;
  ]
