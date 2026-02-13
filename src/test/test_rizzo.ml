open Rizzoc

let test_location_creation () =
  let pos1 = { Lexing.pos_fname = "test.rz"; 
               pos_lnum = 5; 
               pos_bol = 40; 
               pos_cnum = 45 } in
  let pos2 = { pos1 with pos_cnum = 48 } in
  let loc = Location.mk pos1 pos2 in
  let loc_str = Location.to_string loc in
  Alcotest.(check bool) "location string not empty" true (String.length loc_str > 0);
  Alcotest.(check bool) "contains filename" true (String.contains loc_str ':')

let test_lexer_error_has_location () =
  let input = "   @invalid" in
  let lexbuf = Lexing.from_string input in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with 
    Lexing.pos_fname = "test_lexer.rz";
    pos_lnum = 3;
    pos_bol = 0;
  };
  
  (try
     let _ = Lexer.read lexbuf in
     Alcotest.fail "expected lexer error"
   with
   | Lexer.Error (loc, msg) ->
       Alcotest.(check string) "lexer message" "Unexpected character: '@'" msg;
       Alcotest.(check int) "line" 3 loc.Location.start_pos.Lexing.pos_lnum
   | _ -> Alcotest.fail "unexpected exception")

let test_show_error_context () =
  let test_file = "test_location_temp.rz" in
  let oc = open_out test_file in
  Printf.fprintf oc "line 1\n";
  Printf.fprintf oc "line 2 with error\n";
  Printf.fprintf oc "line 3\n";
  close_out oc;
  
  let pos_start = { 
    Lexing.pos_fname = test_file;
    pos_lnum = 2;
    pos_bol = 7;
    pos_cnum = 16;
  } in
  let pos_end = { pos_start with pos_cnum = 21 } in
  let loc = Location.mk pos_start pos_end in
  
  (* Just verify it doesn't crash - actual output is visual *)
  Location.show_error_context loc "This is a test error message";
  Sys.remove test_file;
  Alcotest.(check pass) "error context displayed" () ()

let test_warning () =
  let pos = { 
    Lexing.pos_fname = "test.rz";
    pos_lnum = 10;
    pos_bol = 100;
    pos_cnum = 105;
  } in
  let loc = Location.mk pos pos in
  (* Just verify it doesn't crash *)
  Location.report_warning loc "This is a test warning";
  Alcotest.(check pass) "warning reported" () ()

let location_tests = [
  "location creation", `Quick, test_location_creation;
  "lexer error has location", `Quick, test_lexer_error_has_location;
  "error context display", `Quick, test_show_error_context;
  "warning reporting", `Quick, test_warning;
]


