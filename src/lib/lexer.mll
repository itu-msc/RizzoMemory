{
open Parser

exception Error of Location.t * string

(* Helper to create error with location from lexbuf *)
let error lexbuf msg =
  let loc = Location.mk lexbuf.Lexing.lex_start_p lexbuf.Lexing.lex_curr_p in
  raise (Error (loc, msg))

let parse_unicode_escape s =
  if String.length s = 0 || String.length s > 6 then
    invalid_arg "unicode escape must have 1-6 hex digits";
  let code = int_of_string ("0x" ^ s) in
  if code < 0 || code > 0x10FFFF then
    invalid_arg "unicode codepoint out of range";
  let b = Buffer.create 4 in
  if code <= 0x7F then
    Buffer.add_char b (Char.chr code)
  else if code <= 0x7FF then begin
    Buffer.add_char b (Char.chr (0xC0 lor (code lsr 6)));
    Buffer.add_char b (Char.chr (0x80 lor (code land 0x3F)))
  end else if code <= 0xFFFF then begin
    Buffer.add_char b (Char.chr (0xE0 lor (code lsr 12)));
    Buffer.add_char b (Char.chr (0x80 lor ((code lsr 6) land 0x3F)));
    Buffer.add_char b (Char.chr (0x80 lor (code land 0x3F)))
  end else begin
    Buffer.add_char b (Char.chr (0xF0 lor (code lsr 18)));
    Buffer.add_char b (Char.chr (0x80 lor ((code lsr 12) land 0x3F)));
    Buffer.add_char b (Char.chr (0x80 lor ((code lsr 6) land 0x3F)));
    Buffer.add_char b (Char.chr (0x80 lor (code land 0x3F)))
  end;
  Buffer.contents b
}

let digit = ['0'-'9']
let int = digit+
let lower = ['a'-'z' '_']
let upper = ['A'-'Z']
let id_rest = ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']
let id = lower id_rest*
let type_id = upper id_rest*
let type_var = '\'' lower id_rest*
let hex = ['0'-'9' 'a'-'f' 'A'-'F']

rule read = parse
  | [' ' '\t' '\r' ] { read lexbuf }
  | '\n' { Lexing.new_line lexbuf; read lexbuf}
  | "//" { line_comment lexbuf; read lexbuf }
  | "/*" { block_comment lexbuf; read lexbuf }
  | "()" { UNIT }
  | "|>" { PIPE_GT }
  | "->" { ARROW }
  | "::" { CONS }
  | ":" { COLON }
  | "*" { STAR }
  | "=" { EQ }
  | "==" { EQEQ }
  | "," { COMMA }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "|" { BAR }
  | "_" { UNDERSCORE }
  | type_var as x { TYPEVAR x }
  | type_id as x { TYPE_ID x }
  | int as i { INT (int_of_string i) }
  | '"' { STRING (read_string (Buffer.create 32) lexbuf) }
  | id as x {
      match x with
      | "let" -> LET
      | "fun" -> FUN
      | "match" -> MATCH
      | "with" -> WITH
      | "in" -> IN
      | "true" -> TRUE
      | "false" -> FALSE
      | "never" -> NEVER
      | "wait" -> WAIT
      | "tail" -> TAIL
      | "sync" -> SYNC
      | "watch" -> WATCH
      | "laterapp" -> LATERAPP
      | "delay" -> DELAY
      | "ostar" -> OSTAR
      | "if" -> IF
      | "then" -> THEN
      | "else" -> ELSE
      | _ -> ID x
    }
  | eof { EOF }
  | _ as c { 
      error lexbuf (Printf.sprintf "Unexpected character: %C" c)
    }

and line_comment = parse
  | '\n' { () }
  | eof { () }
  | _ { line_comment lexbuf }

and block_comment = parse
  | "*/" { () }
  | eof { error lexbuf "Unterminated block comment" }
  | _ { block_comment lexbuf }

and read_string buf = parse
  | '"' { Buffer.contents buf }
  | "\\\\" { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | "\\\"" { Buffer.add_char buf '"'; read_string buf lexbuf }
  | "\\n" { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | "\\r" { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | "\\t" { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | "\\u{" (hex+ as h) "}" {
      (try Buffer.add_string buf (parse_unicode_escape h)
       with Invalid_argument msg -> error lexbuf ("Invalid unicode escape: " ^ msg));
      read_string buf lexbuf
    }
  | "\\" _ { error lexbuf "Invalid string escape" }
  | '\n' { error lexbuf "String literals cannot span multiple lines" }
  | eof { error lexbuf "Unterminated string literal" }
  | [^ '"' '\\' '\n']+ as s { Buffer.add_string buf s; read_string buf lexbuf }
