
%{
open! Ast
%}

%token EOF

%start <Ast.program> main

%%

main:
	| EOF { [] }