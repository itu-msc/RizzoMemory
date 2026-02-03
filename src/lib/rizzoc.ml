
module Ast = struct 
  type program = Ast.program
end

module RefCount = struct
include Refcount
end

let parse_string (s : string) =
  let lexbuf = Lexing.from_string s in
  Parser.main Lexer.read lexbuf
