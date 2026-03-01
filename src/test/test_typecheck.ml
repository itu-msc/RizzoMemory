open Rizzoc

let contains_substring ~(text : string) ~(substring : string) : bool =
  let text_len = String.length text in
  let sub_len = String.length substring in
  let rec go index =
    if index + sub_len > text_len then
      false
    else if String.sub text index sub_len = substring then
      true
    else
      go (index + 1)
  in
  if sub_len = 0 then true else go 0

let test_typecheck_threads_global_env () =
  let parsed = Parser.parse_string "let x = 1\nlet y = x\n" in
  match Typecheck.typecheck parsed with
  | Ok typed_program ->
      Alcotest.(check int) "typed declaration count" 2 (List.length typed_program)
  | Error (Typecheck.Typing_error msg) ->
      Alcotest.failf "expected successful typecheck, got error: %s" msg

let test_typecheck_accumulates_top_level_errors () =
  let parsed = Parser.parse_string "let bad1 = missing\nlet bad2 = 1 == true\n" in
  match Typecheck.typecheck parsed with
  | Ok _ -> Alcotest.fail "expected typechecking to fail"
  | Error (Typecheck.Typing_error msg) ->
      Alcotest.(check bool)
        "includes unbound variable error"
        true
        (contains_substring ~text:msg ~substring:"Unbound variable: missing");
      Alcotest.(check bool)
        "includes check mismatch error"
        true
        (contains_substring ~text:msg ~substring:"Type mismatch")

let test_typed_program_pretty_prints_types () =
  let parsed = Parser.parse_string "let x = 1\nlet y = x\n" in
  match Typecheck.typecheck parsed with
  | Error (Typecheck.Typing_error msg) ->
      Alcotest.failf "expected successful typecheck, got error: %s" msg
  | Ok typed_program ->
      let rendered = Format.asprintf "%a" Ast.pp_typed_program typed_program in
      Alcotest.(check bool)
        "typed program includes int annotation"
        true
        (contains_substring ~text:rendered ~substring:": int")

let test_typecheck_functions_and_applications () =
  let parsed = Parser.parse_string "fun id x = x\nlet y = id 1\n" in
  match Typecheck.typecheck parsed with
  | Ok typed_program ->
      Alcotest.(check int) "typed declaration count" 2 (List.length typed_program)
  | Error (Typecheck.Typing_error msg) ->
      Alcotest.failf "expected successful typecheck, got error: %s" msg

let test_typecheck_reports_function_arity_mismatch () =
  let parsed = Parser.parse_string "fun pair x y = x\nlet z = pair 1\n" in
  match Typecheck.typecheck parsed with
  | Ok _ -> Alcotest.fail "expected typechecking to fail on arity mismatch"
  | Error (Typecheck.Typing_error _) -> ()

let test_typecheck_reports_function_argument_type_mismatch () =
  let parsed = Parser.parse_string "fun is_zero x = x == 0\nlet y = is_zero true\n" in
  match Typecheck.typecheck parsed with
  | Ok _ -> Alcotest.fail "expected typechecking to fail on argument type mismatch"
  | Error (Typecheck.Typing_error _) -> ()

let tests =
  [
    "threads global environment across declarations", `Quick, test_typecheck_threads_global_env;
    "accumulates errors while continuing top-level flow", `Quick, test_typecheck_accumulates_top_level_errors;
    "typed program pretty prints types", `Quick, test_typed_program_pretty_prints_types;
    "typechecks functions and applications", `Quick, test_typecheck_functions_and_applications;
    "reports function arity mismatch", `Quick, test_typecheck_reports_function_arity_mismatch;
    "reports function argument mismatch", `Quick, test_typecheck_reports_function_argument_type_mismatch;
  ]
