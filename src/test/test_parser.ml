open Rizzoc
open Rizzoc.Ast

let program_testable = Alcotest.testable Ast.pp_program Ast.eq_program

let test_parser_program () =
  let input =
    "let x = 1\n"
    ^ "fun id y = y\n"
    ^ "let pair = (x, 2)\n"
    ^ "let xs = x :: y :: z\n"
    ^ "let m = match xs with x | y\n"
    ^ "let app = f x y\n\n"
    ^ "let local = let z = 1 in z\n"
  in
  let parsed = Rizzoc.Parser.parse_string input in
  let expected: program =
    [ TLet ("x", EConst (CInt 1));
      TLet ("id", EFun (["y"], EVar "y"));
      TLet ("pair", ETuple (EVar "x", EConst (CInt 2)));
      TLet ("xs", EBinary (SigCons, EVar "x", EBinary (SigCons, EVar "y", EVar "z")));
      TLet ("m", ECase (EVar "xs", [EVar "x"; EVar "y"]));
      TLet ("app", EApp (EVar "f", [EVar "x"; EVar "y"]));
      TLet ("local", ELet ("z", EConst (CInt 1), EVar "z"));
    ]
  in
  Alcotest.check program_testable "parser builds AST" expected parsed

let test_top_level_let_many_locals () =
  let input =
    "let pipeline = let a = 1 in let b = a in let c = (b, a) in let d = c :: tail in d\n"
  in
  let parsed = Rizzoc.Parser.parse_string input in
  let expected: program =
    [
      TLet
        ( "pipeline",
          ELet
            ( "a",
              EConst (CInt 1),
              ELet
                ( "b",
                  EVar "a",
                  ELet
                    ( "c",
                      ETuple (EVar "b", EVar "a"),
                      ELet
                        ( "d",
                          EBinary (SigCons, EVar "c", EVar "tail"),
                          EVar "d" ) ) ) ) );
    ]
  in
  Alcotest.check program_testable "top-level let with many locals" expected parsed

let parser_tests =
  [
    "parser accepts syntax", `Quick, test_parser_program;
    "top-level let with many locals", `Quick, test_top_level_let_many_locals;
  ]
