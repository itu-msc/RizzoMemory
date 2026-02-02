
module Ast = struct 
  type ast = Ast.ast
end

let parse_string (s : string) : Ast.ast =
  let lexbuf = Lexing.from_string s in
  Parser.main Lexer.read lexbuf
