open Rizzoc
open Rizzoc.Ast
open Ast_test_helpers

let parse_and_typecheck input =
  let parsed = Parser.parse_string input in
  let typed, errors = 
    let {typed_program; type_errors; _} : Rizzoc.typing_result = typecheck parsed in
    typed_program, type_errors
  in
  Alcotest.(check int) "type errors" 0 (List.length errors);
  typed

let test_console_is_string_channel () =
  let typed = parse_and_typecheck "let use_console = console\n" in
  match typed with
  | [TopLet (_, EVar ("console", Ann_typed (_, console_t)), _)] ->
      Alcotest.(check bool) "console is Chan String" true (Ast.eq_typ console_t (TChan TString))
  | _ -> Alcotest.fail "unexpected typed AST shape for console builtin"

let test_parse_int_has_expected_type () =
  let typed = parse_and_typecheck "let maybe_num = parse_int(\"42\")\n" in
  match typed with
  | [TopLet (_, EApp (EVar ("parse_int", Ann_typed (_, builtin_t)), [_], Ann_typed (_, result_t)), _)] ->
      Alcotest.(check bool) "parse_int builtin type"
        true
        (Ast.eq_typ builtin_t (TFun (Cons1 (TString, []), TApp (TName "Option", [TInt]))));
      Alcotest.(check bool) "parse_int return type"
        true
        (Ast.eq_typ result_t (TApp (TName "Option", [TInt])))
  | _ -> Alcotest.fail "unexpected typed AST shape for parse_int builtin"

let test_clock_has_expected_type () =
  let typed = parse_and_typecheck "let ticks = clock(100)\n" in
  match typed with
  | [TopLet (_, EApp (EVar ("clock", Ann_typed (_, builtin_t)), [_], Ann_typed (_, result_t)), _)] ->
      Alcotest.(check bool) "clock builtin type"
        true
        (Ast.eq_typ builtin_t (TFun (Cons1 (TInt, []), TSignal TInt)));
      Alcotest.(check bool) "clock return type"
        true
        (Ast.eq_typ result_t (TSignal TInt))
  | _ -> Alcotest.fail "unexpected typed AST shape for clock builtin"

let test_new_builtins_have_expected_types () =
  let typed =
    parse_and_typecheck
      ("let mod_result = mod 7 3\n"
      ^ "let abs_result = abs(4 - 9)\n"
      ^ "let min_result = min 4 9\n"
      ^ "let max_result = max 4 9\n"
      ^ "let clamp_result = clamp 12 0 10\n"
      ^ "let contains_result = string_contains \"functional reactive programming\" \"reactive\"\n"
      ^ "let starts_result = string_starts_with \"hello\" \"he\"\n"
      ^ "let ends_result = string_ends_with \"hello\" \"lo\"\n")
  in
  match typed with
  | [ TopLet (_, EApp (EVar ("mod", Ann_typed (_, mod_t)), [_; _], Ann_typed (_, mod_result_t)), _);
      TopLet (_, EApp (EVar ("abs", Ann_typed (_, abs_t)), [_], Ann_typed (_, abs_result_t)), _);
      TopLet (_, EApp (EVar ("min", Ann_typed (_, min_t)), [_; _], Ann_typed (_, min_result_t)), _);
      TopLet (_, EApp (EVar ("max", Ann_typed (_, max_t)), [_; _], Ann_typed (_, max_result_t)), _);
      TopLet (_, EApp (EVar ("clamp", Ann_typed (_, clamp_t)), [_; _; _], Ann_typed (_, clamp_result_t)), _);
      TopLet (_, EApp (EVar ("string_contains", Ann_typed (_, contains_t)), [_; _], Ann_typed (_, contains_result_t)), _);
      TopLet (_, EApp (EVar ("string_starts_with", Ann_typed (_, starts_t)), [_; _], Ann_typed (_, starts_result_t)), _);
      TopLet (_, EApp (EVar ("string_ends_with", Ann_typed (_, ends_t)), [_; _], Ann_typed (_, ends_result_t)), _) ] ->
      Alcotest.(check bool) "mod builtin type" true (Ast.eq_typ mod_t (TFun (Cons1 (TInt, [TInt]), TInt)));
      Alcotest.(check bool) "mod result type" true (Ast.eq_typ mod_result_t TInt);
      Alcotest.(check bool) "abs builtin type" true (Ast.eq_typ abs_t (TFun (Cons1 (TInt, []), TInt)));
      Alcotest.(check bool) "abs result type" true (Ast.eq_typ abs_result_t TInt);
      Alcotest.(check bool) "min builtin type" true (Ast.eq_typ min_t (TFun (Cons1 (TInt, [TInt]), TInt)));
      Alcotest.(check bool) "min result type" true (Ast.eq_typ min_result_t TInt);
      Alcotest.(check bool) "max builtin type" true (Ast.eq_typ max_t (TFun (Cons1 (TInt, [TInt]), TInt)));
      Alcotest.(check bool) "max result type" true (Ast.eq_typ max_result_t TInt);
      Alcotest.(check bool) "clamp builtin type" true (Ast.eq_typ clamp_t (TFun (Cons1 (TInt, [TInt; TInt]), TInt)));
      Alcotest.(check bool) "clamp result type" true (Ast.eq_typ clamp_result_t TInt);
      Alcotest.(check bool) "string_contains builtin type" true (Ast.eq_typ contains_t (TFun (Cons1 (TString, [TString]), TBool)));
      Alcotest.(check bool) "string_contains result type" true (Ast.eq_typ contains_result_t TBool);
      Alcotest.(check bool) "string_starts_with builtin type" true (Ast.eq_typ starts_t (TFun (Cons1 (TString, [TString]), TBool)));
      Alcotest.(check bool) "string_starts_with result type" true (Ast.eq_typ starts_result_t TBool);
      Alcotest.(check bool) "string_ends_with builtin type" true (Ast.eq_typ ends_t (TFun (Cons1 (TString, [TString]), TBool)));
      Alcotest.(check bool) "string_ends_with result type" true (Ast.eq_typ ends_result_t TBool)
  | _ -> Alcotest.fail "unexpected typed AST shape for new builtins"

let test_list_constructors_and_projection_builtins_have_expected_types () =
  let typed =
    parse_and_typecheck
      ("let empty : List Int = []\n"
      ^ "let nums = 1 :: []\n"
      ^ "let first = list_head nums\n"
      ^ "let rest = list_tail nums\n")
  in
  let open Ast.Factory in
  match typed with
  | [ TopLet (_, EAnno (_, TApp (TName "List", [TInt]), _), _);
      TopLet (_, EBinary (SigCons, _, _, Ann_typed (_, nums_t)), _);
      TopLet (_, EApp (EVar ("list_head", Ann_typed (_, head_builtin_t)), [_], Ann_typed (_, first_t)), _);
      TopLet (_, EApp (EVar ("list_tail", Ann_typed (_, tail_builtin_t)), [_], Ann_typed (_, rest_t)), _) ] ->
      Alcotest.(check bool) "cons infers list type" true (Ast.eq_typ nums_t (typ_list TInt));
      Alcotest.(check bool) "list_head builtin type" true (Ast.eq_typ head_builtin_t (TFun (Cons1 (typ_list TInt, []), TInt)));
      Alcotest.(check bool) "list_head result type" true (Ast.eq_typ first_t TInt);
      Alcotest.(check bool) "list_tail builtin type" true (Ast.eq_typ tail_builtin_t (TFun (Cons1 (typ_list TInt, []), typ_list TInt)));
      Alcotest.(check bool) "list_tail result type" true (Ast.eq_typ rest_t (typ_list TInt))
  | _ -> Alcotest.fail "unexpected typed AST shape for list constructors and projection builtins"

let test_list_supporting_builtins_have_expected_types () =
  let typed =
    parse_and_typecheck
      ("let empty : List Int = []\n"
      ^ "let is_empty = list_is_empty empty\n"
      ^ "let count = list_length [1, 2, 3]\n"
      ^ "let words = string_split \"a,b\" \",\"\n")
  in
  let open Ast.Factory in
  match typed with
  | [ TopLet (_, _, _);
      TopLet (_, EApp (EVar ("list_is_empty", Ann_typed (_, empty_builtin_t)), [_], Ann_typed (_, empty_result_t)), _);
      TopLet (_, EApp (EVar ("list_length", Ann_typed (_, length_builtin_t)), [_], Ann_typed (_, length_result_t)), _);
      TopLet (_, EApp (EVar ("string_split", Ann_typed (_, split_builtin_t)), [_; _], Ann_typed (_, split_result_t)), _) ] ->
      Alcotest.(check bool) "list_is_empty builtin type" true (Ast.eq_typ empty_builtin_t (typ_fun1 (typ_list TInt) TBool));
      Alcotest.(check bool) "list_is_empty result type" true (Ast.eq_typ empty_result_t TBool);
      Alcotest.(check bool) "list_length builtin type" true (Ast.eq_typ length_builtin_t (typ_fun1 (typ_list TInt) TInt));
      Alcotest.(check bool) "list_length result type" true (Ast.eq_typ length_result_t TInt);
      Alcotest.(check bool) "string_split builtin type" true (Ast.eq_typ split_builtin_t (typ_fun [TString; TString] (typ_list TString)));
      Alcotest.(check bool) "string_split result type" true (Ast.eq_typ split_result_t (typ_list TString))
  | _ -> Alcotest.fail "unexpected typed AST shape for list support builtins"

let test_lower_typed_program_rewrites_list_cons_to_constructor () =
  let typed = parse_and_typecheck "let nums = 1 :: []\n" in
  let lowered = lower_typed_program typed in
  let expected = [toplet "nums" (ctor "Cons" [int 1; ctor "Nil" []])] in
  Alcotest.check program_testable "list cons lowers to constructors" expected lowered

let builtin_tests = [
  "console is a string channel", `Quick, test_console_is_string_channel;
  "parse_int returns option int", `Quick, test_parse_int_has_expected_type;
  "clock returns signal int", `Quick, test_clock_has_expected_type;
  "new builtins have expected types", `Quick, test_new_builtins_have_expected_types;
  "list constructors and builtins have expected types", `Quick, test_list_constructors_and_projection_builtins_have_expected_types;
  "list support builtins have expected types", `Quick, test_list_supporting_builtins_have_expected_types;
  "list cons lowers to constructors", `Quick, test_lower_typed_program_rewrites_list_cons_to_constructor;
]
