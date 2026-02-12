{
open Parser

exception Error of string

(* Helper to create error with location from lexbuf *)
let error lexbuf msg =
  let loc = Location.mk lexbuf.Lexing.lex_start_p lexbuf.Lexing.lex_curr_p in
  Location.show_error_context loc msg;
  raise (Error msg)
}

let digit = ['0'-'9']
let int = digit+
let id_start = ['a'-'z' 'A'-'Z' '_']
let id_rest = ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']
let id = id_start id_rest*

rule read = parse
  | [' ' '\t' '\r' '\n'] { read lexbuf }
  | "()" { UNIT }
  | "::" { CONS }
  | "=" { EQ }
  | "," { COMMA }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "|" { BAR }
  | int as i { INT (int_of_string i) }
  | id as x {
      match x with
      | "let" -> LET
      | "fun" -> FUN
      | "match" -> MATCH
      | "with" -> WITH
      | "in" -> IN
      | "true" -> TRUE
      | "false" -> FALSE
      | _ -> ID x
    }
  | eof { EOF }
  | _ as c { 
      error lexbuf (Printf.sprintf "Unexpected character: %C" c)
    }
