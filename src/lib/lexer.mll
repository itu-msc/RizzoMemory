{
open Parser

exception Error of string
}

rule read = parse
  | [' ' '\t' '\r' '\n'] { read lexbuf }
  | eof { EOF }
  | _ as c { raise (Error (Printf.sprintf "Unexpected character: %C" c)) }
