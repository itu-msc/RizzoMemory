open Rizzoc.Ast
open Ast_test_helpers

let test_parser_program () =
  let input =
    "let x = 1\n"
    ^ "fun id y = y\n"
    ^ "let pair = (x, 2)\n"
    ^ "let xs = x :: y :: z\n"
    ^ "let m = match xs with | h :: t -> h | _ -> y\n"
    ^ "let greeting = \"he\" + \"llo\"\n"
    ^ "let app = f x y\n\n"
    ^ "let local = let z = 1 in z\n"
  in
  let parsed = Rizzoc.Parser.parse_string input in
  let expected : parsed program =
    [ toplet "x" (int 1);
      toplet "id" (fun_ ["y"] (var "y"));
      toplet "pair" (tuple (var "x") (int 2));
      (* x :: (y :: z) *)
      toplet "xs" (binary SigCons (var "x") (binary SigCons (var "y") (var "z")));
      toplet "m" (case (var "xs") [ (psigcons (pvar "h") (name "t"), var "h"); (pwild, var "y") ]);
      toplet "greeting" (binary Add (str "he") (str "llo"));
      toplet "app" (app (var "f") [var "x"; var "y"]);
      toplet "local" (let_ "z" (int 1) (var "z"));
    ]
  in
  Alcotest.check program_testable "parser builds AST" expected parsed

let test_top_level_let_many_locals () =
  let input =
    "let pipeline = let a = 1 in let b = a in let c = (b, a) in let d = c :: xs_tail in d\n"
  in
  let parsed = Rizzoc.Parser.parse_string input in
  let expected : parsed program =
    [
      toplet "pipeline"
        (let_ "a" (int 1)
           (let_ "b" (var "a")
              (let_ "c" (tuple (var "b") (var "a"))
                 (let_ "d" (binary SigCons (var "c") (var "xs_tail")) (var "d")))));
    ]
  in
  Alcotest.check program_testable "top-level let with many locals" expected parsed

let test_arbitrary_length_tuple () =
  let input =
    "let t = (1, 2, 3, 4)\n"
  in
  let parsed = Rizzoc.Parser.parse_string input in
  let expected : parsed program =
    [
      toplet "t" (tuple (int 1) (tuple (int 2) (tuple (int 3) (int 4))));
    ]
  in
  Alcotest.check program_testable "arbitrary length tuple parses" expected parsed

let test_int_operator_precedence () =
  let input =
    "let calc = 10 - 3 * 2 / 1\n"
    ^ "let less = 1 + 2 < 4\n"
    ^ "let more = 5 > 4\n"
    ^ "let atleast = 5 >= 4\n"
    ^ "let atmost = 5 <= 6\n"
  in
  let parsed = Rizzoc.Parser.parse_string input in
  let expected : parsed program =
    [
      toplet "calc" (binary Sub (int 10) (binary Div (binary Mul (int 3) (int 2)) (int 1)));
      toplet "less" (binary Lt (binary Add (int 1) (int 2)) (int 4));
      toplet "more" (binary Gt (int 5) (int 4));
      toplet "atleast" (binary Geq (int 5) (int 4));
      toplet "atmost" (binary Leq (int 5) (int 6));
    ]
  in
  Alcotest.check program_testable "int operators parse with precedence" expected parsed

let test_not_operator_syntax () =
  let input =
    "let a = !ready\n"
    ^ "let b = not false\n"
  in
  let parsed = Rizzoc.Parser.parse_string input in
  let expected : parsed program =
    [
      toplet "a" (unary UNot (var "ready"));
      toplet "b" (app (var "not") [const (CBool false)]);
    ]
  in
  Alcotest.check program_testable "not parses as unary operator" expected parsed

let test_effectful_decorator_marks_function () =
  let input =
    "@effectful fun output_log x = x\n"
  in
  let parsed = Rizzoc.Parser.parse_string input in
  let expected : parsed program =
    [ toplet "output_log" (fun_ ["x"] (var "x")) ]
  in
  Alcotest.check program_testable "effectful decorated fun parses" expected parsed;
  Alcotest.(check bool) "effectful name registered" true (Rizzoc.Effectful.is_effectful "output_log")

let test_constructor_application_with_parenthesized_let () =
  let input =
    "let x = Just (let y = y in y :: never)\n"
  in
  let parsed = Rizzoc.Parser.parse_string input in
  let expected : parsed program =
    [
      toplet "x"
        (ctor "Just"
           [let_ "y" (var "y") (binary SigCons (var "y") (const CNever))]);
    ]
  in
  Alcotest.check program_testable "constructor application with parenthesized let" expected parsed

let parser_tests =
  [
    "parser accepts syntax", `Quick, test_parser_program;
    "top-level let with many locals", `Quick, test_top_level_let_many_locals;
    "arbitrary length tuple", `Quick, test_arbitrary_length_tuple;
    "int operators precedence", `Quick, test_int_operator_precedence;
    "not operator syntax", `Quick, test_not_operator_syntax;
    "effectful decorator", `Quick, test_effectful_decorator_marks_function;
    "constructor application with parenthesized let", `Quick, test_constructor_application_with_parenthesized_let;
  ]
