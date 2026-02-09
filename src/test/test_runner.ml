
let () =
  Alcotest.run "Rizzo Location Tests" [
    "location tracking", Test_rizzo.location_tests;
    "ANF TESTS", Test_anf.anf_tests;
  ]

