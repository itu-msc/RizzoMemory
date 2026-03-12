open Rizzoc
open Rizzoc.Ast
open Ast_test_helpers

let parse_and_typecheck input =
  let parsed = Parser.parse_string input in
  let typed, errors = typecheck parsed in
  Alcotest.(check int) "type errors" 0 (List.length errors);
  typed

let rec expr_contains_case : _ expr -> bool = function
  | ECase _ -> true
  | EConst _ | EVar _ -> false
  | ECtor (_, args, _) -> List.exists expr_contains_case args
  | ELet (_, rhs, body, _) -> expr_contains_case rhs || expr_contains_case body
  | EFun (_, body, _) -> expr_contains_case body
  | EApp (fn, args, _) -> expr_contains_case fn || List.exists expr_contains_case args
  | EUnary (_, e, _) -> expr_contains_case e
  | EBinary (_, e1, e2, _) -> expr_contains_case e1 || expr_contains_case e2
  | ETuple (e1, e2, _) -> expr_contains_case e1 || expr_contains_case e2
  | EIfe (c, t, e, _) -> expr_contains_case c || expr_contains_case t || expr_contains_case e
  | EAnno (e, _, _) -> expr_contains_case e

let rec expr_contains_call target : _ expr -> bool = function
  | EApp (EVar (name, _), _, _) when String.equal name target -> true
  | EConst _ | EVar _ -> false
  | ECtor (_, args, _) -> List.exists (expr_contains_call target) args
  | ELet (_, rhs, body, _) -> expr_contains_call target rhs || expr_contains_call target body
  | EFun (_, body, _) -> expr_contains_call target body
  | EApp (fn, args, _) -> expr_contains_call target fn || List.exists (expr_contains_call target) args
  | EUnary (_, e, _) -> expr_contains_call target e
  | EBinary (_, e1, e2, _) -> expr_contains_call target e1 || expr_contains_call target e2
  | ETuple (e1, e2, _) -> expr_contains_call target e1 || expr_contains_call target e2
  | ECase (scrutinee, branches, _) ->
      expr_contains_call target scrutinee
      || List.exists (fun (_, body, _) -> expr_contains_call target body) branches
  | EIfe (c, t, e, _) -> expr_contains_call target c || expr_contains_call target t || expr_contains_call target e
  | EAnno (e, _, _) -> expr_contains_call target e

let test_string_cons_pattern_binds_strings () =
  let typed =
    parse_and_typecheck
      "fun head_or_empty s = match s with | h :: t -> h | \"\" -> \"\"\n"
  in
  match typed with
  | [TopLet (_, EFun (_, body, _), _)] ->
      (match body with
      | ECase (_, (PStringCons (PVar (_, Ann_typed (_, head_t)), (_, Ann_typed (_, tail_t)), Ann_typed (_, pattern_t)), _, _) :: _, _) ->
          Alcotest.(check bool) "head is string" true (Ast.eq_typ head_t TString);
          Alcotest.(check bool) "tail is string" true (Ast.eq_typ tail_t TString);
          Alcotest.(check bool) "pattern is string" true (Ast.eq_typ pattern_t TString)
      | _ -> Alcotest.fail "unexpected function body for string cons pattern")
  | _ -> Alcotest.fail "unexpected typed AST shape for string cons pattern"

let test_ambiguous_string_cons_defaults_to_string () =
  let typed =
    parse_and_typecheck
      "fun describe_text s = match s with | first :: _rest -> \"Starts with \" + first\n"
  in
  match typed with
  | [TopLet (_, EFun (_, body, Ann_typed (_, fun_t)), _)] ->
      Alcotest.(check bool) "function inferred as String -> String" true (Ast.eq_typ fun_t (TFun (Cons1 (TString, []), TString)));
      (match body with
      | ECase (_, (PStringCons _, _, _) :: _, _) -> ()
      | _ -> Alcotest.fail "expected string-cons pattern in typed AST")
  | _ -> Alcotest.fail "unexpected typed AST shape for ambiguous string-cons"

let test_annotated_signal_cons_stays_signal () =
  let typed =
    parse_and_typecheck
      "fun first_signal s : Signal String -> String = match s with | first :: _rest -> first\n"
  in
  match typed with
  | [TopLet (_, EAnno (EFun (_, body, Ann_typed (_, fun_t)), _, _), _)] ->
      Alcotest.(check bool) "function stays Signal String -> String" true (Ast.eq_typ fun_t (TFun (Cons1 (TSignal TString, []), TString)));
      (match body with
      | ECase (_, (PSigCons _, _, _) :: _, _) -> ()
      | _ -> Alcotest.fail "expected signal-cons pattern in typed AST")
  | _ -> Alcotest.fail "unexpected typed AST shape for annotated signal-cons"

let test_lower_typed_program_rewrites_string_add () =
  let typed = parse_and_typecheck "let greeting = \"he\" + \"llo\"\n" in
  let lowered = lower_typed_program typed in
  let expected =
    [
      toplet "greeting" (app (var "string_concat") [str "he"; str "llo"]);
    ]
  in
  Alcotest.check program_testable "string add lowers to intrinsic" expected lowered

let test_apply_typed_transforms_eliminates_string_match () =
  let typed =
    parse_and_typecheck
      "fun split_or_empty s = match s with | h :: t -> (h, t) | \"\" -> (\"\", \"\")\n"
  in
  Utilities.new_name_reset ();
  let transformed = apply_typed_transforms typed in
  match transformed with
  | [TopLet (_, EFun (_, body, _), _)] ->
      Alcotest.(check bool) "string match removed" false (expr_contains_case body);
      Alcotest.(check bool) "uses string_is_empty" true (expr_contains_call "string_is_empty" body);
      Alcotest.(check bool) "uses string_head" true (expr_contains_call "string_head" body);
      Alcotest.(check bool) "uses string_tail" true (expr_contains_call "string_tail" body)
  | _ -> Alcotest.fail "unexpected transformed AST shape"

let test_nested_string_literal_pattern_typechecks () =
  let _typed =
    parse_and_typecheck
      "fun is_a opt = match opt with | Just(\"a\") -> true | _ -> false\n"
  in
  ()

let string_tests = [
  "string cons pattern binds strings", `Quick, test_string_cons_pattern_binds_strings;
  "ambiguous string cons defaults to string", `Quick, test_ambiguous_string_cons_defaults_to_string;
  "annotated signal cons stays signal", `Quick, test_annotated_signal_cons_stays_signal;
  "string add lowers to intrinsic", `Quick, test_lower_typed_program_rewrites_string_add;
  "string match lowers in pipeline", `Quick, test_apply_typed_transforms_eliminates_string_match;
  "nested string literal pattern typechecks", `Quick, test_nested_string_literal_pattern_typechecks;
]