
let () =
  Alcotest.run "Rizzo Location Tests" [
    "location tracking", Test_rizzo.location_tests;
    "BUILTIN TESTS", Test_builtins.builtin_tests;
    "PARSER/LEXER TESTS", Test_parser.parser_tests;
    "INT OPERATOR TESTS", Test_int_ops.int_operator_tests;
    "BOOL OPERATOR TESTS", Test_bool_ops.bool_operator_tests;
    "STRING TESTS", Test_strings.string_tests;
    "LANGUAGE SERVICE TESTS", Test_language_service.tests;
    "ANF TESTS", Test_anf.anf_tests;
    "LIFT TESTS", Test_lift.lift_tests;
    "LAMBDA TESTS", Test_lambda.lambda_tests;
    "COPY PROP TESTS", Test_copr.tests_copy_propagation;
    "DEAD LET TESTS", Test_dead_let.tests_dead_let;
    "SIMPLE PATTERN TESTS", Test_simple_patterns.simple_pattern_tests;
    "RESET REUSE TESTS", Test_reset_reuse.reset_reuse_tests;
  ]
