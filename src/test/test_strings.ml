open Rizzoc
open Rizzoc.Ast
open Ast_test_helpers

let parse_and_typecheck input =
  let parsed = Parser.parse_string input in
  let typed, errors = typecheck parsed in
  Alcotest.(check int) "type errors" 0 (List.length errors);
  typed

let test_string_cons_pattern_binds_strings () =
  let typed =
    parse_and_typecheck
      "fun head_or_empty s = match s with | h :: t -> h | \"\" -> \"\"\n"
  in
  match typed with
  | [TopLet (_, EFun (_, body, _), _)] ->
      (match body with
      | ECase (_, (PSigCons (PVar (_, Ann_typed (_, head_t)), (_, Ann_typed (_, tail_t)), Ann_typed (_, pattern_t)), _, _) :: _, _) ->
          Alcotest.(check bool) "head is string" true (Ast.eq_typ head_t TString);
          Alcotest.(check bool) "tail is string" true (Ast.eq_typ tail_t TString);
          Alcotest.(check bool) "pattern is string" true (Ast.eq_typ pattern_t TString)
      | _ -> Alcotest.fail "unexpected function body for string cons pattern")
  | _ -> Alcotest.fail "unexpected typed AST shape for string cons pattern"

let test_lower_typed_program_rewrites_string_add () =
  let typed = parse_and_typecheck "let greeting = \"he\" + \"llo\"\n" in
  let lowered = lower_typed_program typed in
  let expected =
    [
      toplet "greeting" (app (var "string_concat") [str "he"; str "llo"]);
    ]
  in
  Alcotest.check program_testable "string add lowers to intrinsic" expected lowered

let test_lower_typed_program_eliminates_string_match () =
  Utilities.new_name_reset ();
  let typed =
    parse_and_typecheck
      "fun head_or_empty s = match s with | h :: t -> h | \"\" -> \"\"\n"
  in
  let lowered = lower_typed_program typed in
  Utilities.new_name_reset ();
  let head_tmp = Utilities.new_name "string_head" in
  let expected =
    [
      toplet "head_or_empty"
        (fun_ ["s"]
           (ife
              (app (var "string_is_empty") [var "s"])
              (ife
                 (app (var "string_eq") [var "s"; str ""])
                 (str "")
                 (app (var "match_fail") [str "Non-exhaustive string match"]))
              (let_ head_tmp
                 (app (var "string_head") [var "s"])
                 (let_ "h"
                    (var head_tmp)
                    (let_ "t" (app (var "string_tail") [var "s"]) (var "h"))))))
    ]
  in
  Alcotest.check program_testable "string match lowers to string intrinsics" expected lowered

let string_tests = [
  "string cons pattern binds strings", `Quick, test_string_cons_pattern_binds_strings;
  "string add lowers to intrinsic", `Quick, test_lower_typed_program_rewrites_string_add;
  "string match lowers to intrinsics", `Quick, test_lower_typed_program_eliminates_string_match;
]
