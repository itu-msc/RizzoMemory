
%{
open! Ast

(* When you expand the parser, you can use location tracking helpers:
   - $startpos : starting position of the symbol
   - $endpos   : ending position of the symbol
   - $startpos(x) : starting position of symbol x
   - $endpos(x)   : ending position of symbol x
   
   Example helper function:
   let mkloc start_pos end_pos value = value
   
   Example usage in rules:
   expr:
     | x=ID { mkloc $startpos $endpos (EVar x) }
     | e1=expr PLUS e2=expr 
       { mkloc $startpos $endpos (EBinary (Add, e1, e2)) }
*)
%}

%token EOF

%start <Ast.program> main

%%

main:
	| EOF { [] }