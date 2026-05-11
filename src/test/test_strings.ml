open Rizzoc
open Rizzoc.Ast
open Ast_test_helpers

let parse_and_typecheck input =
  let parsed = Parser.parse_string input in
  let typed, errors = 
    let {typed_program; type_errors; _} : TypeCheck.typing_result = TypeCheck.typecheck parsed in
    typed_program, type_errors
  in
  Alcotest.(check int) "type errors" 0 (List.length errors);
  typed

let rec expr_contains_case : _ expr -> bool = function
  | ECase _ -> true
  | EConst _ | EVar _ | EError _ -> false
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
  | EConst _ | EVar _ | EError _ -> false
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

let rec expr_contains_let_binding target : _ expr -> bool = function
  | ELet ((name, _), rhs, body, _) ->
    String.equal name target || expr_contains_let_binding target rhs || expr_contains_let_binding target body
  | EConst _ | EVar _ | EError _ -> false
  | ECtor (_, args, _) -> List.exists (expr_contains_let_binding target) args
  | EFun (_, body, _) -> expr_contains_let_binding target body
  | EApp (fn, args, _) -> expr_contains_let_binding target fn || List.exists (expr_contains_let_binding target) args
  | EUnary (_, e, _) -> expr_contains_let_binding target e
  | EBinary (_, e1, e2, _) -> expr_contains_let_binding target e1 || expr_contains_let_binding target e2
  | ETuple (e1, e2, _) -> expr_contains_let_binding target e1 || expr_contains_let_binding target e2
  | ECase (scrutinee, branches, _) ->
    expr_contains_let_binding target scrutinee
    || List.exists (fun (_, body, _) -> expr_contains_let_binding target body) branches
  | EIfe (c, t, e, _) -> expr_contains_let_binding target c || expr_contains_let_binding target t || expr_contains_let_binding target e
  | EAnno (e, _, _) -> expr_contains_let_binding target e

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
      "fun is_a opt = match opt with | Some(\"a\") -> true | _ -> false\n"
  in
  ()

let test_constructor_string_cons_binds_tail () =
  let program =
    [
      toplet "consMatcher"
        (fun_ ["syn"]
           (case (var "syn")
              [
                ( pctor (name "Left") [pstringcons (pconst (CString "Hola")) (name "rest")],
                  binary Add (str "Got Hola with tail ") (var "rest") );
                (pctor (name "Left") [pvar "x"], binary Add (str "Got Left with ") (var "x"));
                (pwild, str "Got something else");
              ]));
    ]
  in
  Utilities.new_name_reset ();
  let transformed = Transformations.eliminate_patterns_tree program in
  match transformed with
    | [TopLet (_, EFun (_, body, _), _)] ->
      Alcotest.(check bool) "uses string_tail in nested constructor payload" true (expr_contains_call "string_tail" body);
      Alcotest.(check bool) "rest is bound in transformed body" true (expr_contains_let_binding "rest" body)
  | _ -> Alcotest.fail "unexpected transformed AST shape"

let test_constructor_fallback_keeps_later_subpatterns () =
  let program =
    [
      toplet "consMatcher"
        (fun_ ["syn"]
           (case (var "syn")
              [
                (pctor (name "Left") [pconst (CString "Hello")], str "Got Hello");
                (pctor (name "Left") [pvar "x"], binary Add (str "Got Left with ") (var "x"));
                (pwild, str "Got something else");
              ]));
    ]
  in
  Utilities.new_name_reset ();
  let transformed = Transformations.eliminate_patterns_tree program in
  Utilities.new_name_reset ();
  let expected =
    [
      toplet "consMatcher"
        (fun_ ["syn"]
           (let_ "scrut1" (var "syn")
              (case (var "scrut1")
                 [
                   ( pctor (name "Left") [pwild],
                     let_ "ctor_field2" (unary (UProj 0) (var "scrut1"))
                       (ife
                          (app (var "string_eq") [var "ctor_field2"; str "Hello"])
                          (str "Got Hello")
                          (let_ "x" (var "ctor_field2") (binary Add (str "Got Left with ") (var "x")))) );
                   (pwild, str "Got something else");
                 ])));
    ]
  in
  Alcotest.check program_testable
    "later constructor arm keeps its own payload pattern"
    expected transformed

let test_explicit_lifts_wrap_builtin_function_values () =
  let program =
    [
      toplet "map_l" (fun_ ["f"; "s"] (var "s"));
      toplet "show_nat" (const CUnit);
      toplet "entry" (fun_ ["x"] (app (var "map_l") [var "string_of_int"; var "show_nat"]));
    ]
  in
  let expected =
    [
      toplet "map_l" (fun_ ["f"; "s"] (var "s"));
      toplet "show_nat" (const CUnit);
      toplet "entry" (fun_ ["x"] (app (var "map_l") [app (var "string_of_int") []; var "show_nat"]));
    ]
  in
  let actual = Transformations.explicit_lift_function_values_program program in
  Alcotest.check program_testable "builtin function value becomes explicit zero-arg app" expected actual

let string_tests = [
  "string cons pattern binds strings", `Quick, test_string_cons_pattern_binds_strings;
  "ambiguous string cons defaults to string", `Quick, test_ambiguous_string_cons_defaults_to_string;
  "annotated signal cons stays signal", `Quick, test_annotated_signal_cons_stays_signal;
  "string add lowers to intrinsic", `Quick, test_lower_typed_program_rewrites_string_add;
  "string match lowers in pipeline", `Quick, test_apply_typed_transforms_eliminates_string_match;
  "nested string literal pattern typechecks", `Quick, test_nested_string_literal_pattern_typechecks;
  "constructor string-cons binds tail", `Quick, test_constructor_string_cons_binds_tail;
  "constructor fallback keeps later subpatterns", `Quick, test_constructor_fallback_keeps_later_subpatterns;
  "function values become explicit lifts", `Quick, test_explicit_lifts_wrap_builtin_function_values;
]
