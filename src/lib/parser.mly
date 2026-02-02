
%{
open Ast
%}

%token EOF

%start <Ast.ast> main

%%

main:
	| EOF { Empty }