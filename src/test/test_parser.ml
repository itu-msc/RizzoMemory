open Rizzoc.Ast
open Ast_test_helpers

let test_parser_program () =
  let input =
    "let x = 1\n"
    ^ "fun id y = y\n"
    ^ "let pair = (x, 2)\n"
    ^ "let xs = x :: y :: z\n"
    ^ "let m = match xs with | h :: t -> h | _ -> y\n"
    ^ "let app = f x y\n\n"
    ^ "let local = let z = 1 in z\n"
  in
  let parsed = Rizzoc.Parser.parse_string input in
  let expected : parsed program =
    [ tlet "x" (int 1);
      tlet "id" (fun_ ["y"] (var "y"));
      tlet "pair" (tuple (var "x") (int 2));
      tlet "xs" (binary SigCons (var "x") (binary SigCons (var "y") (var "z")));
      tlet "m" (case (var "xs") [ (psigcons (pvar "h") (pvar "t"), var "h"); (pwild, var "y") ]);
      tlet "app" (app (var "f") [var "x"; var "y"]);
      tlet "local" (let_ "z" (int 1) (var "z"));
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
      tlet "pipeline"
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
      tlet "t" (tuple (int 1) (tuple (int 2) (tuple (int 3) (int 4))));
    ]
  in
  Alcotest.check program_testable "arbitrary length tuple parses" expected parsed

let parser_tests =
  [
    "parser accepts syntax", `Quick, test_parser_program;
    "top-level let with many locals", `Quick, test_top_level_let_many_locals;
    "arbitrary length tuple", `Quick, test_arbitrary_length_tuple;
  ]
