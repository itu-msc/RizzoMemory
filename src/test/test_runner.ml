
let () =
  Alcotest.run "Rizzo Location Tests" [
    "location tracking", Test_rizzo.location_tests;
    "PARSER/LEXER TESTS", Test_parser.parser_tests;
    "LANGUAGE SERVICE TESTS", Test_language_service.tests;
    "ANF TESTS", Test_anf.anf_tests;
    "LIFT TESTS", Test_lift.lift_tests;
    "COPY PROP TESTS", Test_copr.tests_copy_propagation;
  ]

