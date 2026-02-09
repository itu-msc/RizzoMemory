{
open Parser

exception Error of string

(* Helper to create error with location from lexbuf *)
let error lexbuf msg =
  let loc = Location.mk lexbuf.Lexing.lex_start_p lexbuf.Lexing.lex_curr_p in
  Location.show_error_context loc msg;
  raise (Error msg)
}

rule read = parse
  | [' ' '\t' '\r' '\n'] { read lexbuf }
  | eof { EOF }
  | _ as c { 
      error lexbuf (Printf.sprintf "Unexpected character: %C" c)
    }
