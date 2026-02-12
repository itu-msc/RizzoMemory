
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

%token LET FUN MATCH WITH IN
%token EQ CONS COMMA LPAREN RPAREN BAR
%token <string> ID
%token <int> INT
%token TRUE FALSE UNIT
%token EOF

%start <Ast.program> main

%right CONS

%%

main:
	| decls=top_exprs EOF { decls }

top_exprs:
  | { [] }
  | d=top_expr ds=top_exprs { d :: ds }

top_expr:
  | LET name=ID EQ body=expr
      { TLet (name, body) }
  | FUN name=ID params=nonempty_id_list EQ body=expr
      { TLet (name, EFun (params, body)) }

nonempty_id_list:
  | x=ID { [x] }
  | x=ID xs=nonempty_id_list { x :: xs }

expr:
  | LET x=ID EQ e1=expr IN e2=expr
      { ELet (x, e1, e2) }
  | MATCH scrutinee=expr WITH cases=case_exprs
      { ECase (scrutinee, cases) }
  | e=cons_expr
      { e }

case_exprs:
  | e=expr
      { [e] }
  | e=expr BAR rest=case_exprs
      { e :: rest }

cons_expr:
  | left=app_expr CONS right=cons_expr
      { EBinary (SigCons, left, right) }
  | e=app_expr
      { e }

app_expr:
  | head=atom tail=app_args
      {
        match tail with
        | [] -> head
        | args -> EApp (head, args)
      }

app_args:
  | { [] }
  | a=atom rest=app_args { a :: rest }

atom:
  | x=ID { EVar x }
  | i=INT { EConst (CInt i) }
  | TRUE { EConst (CBool true) }
  | FALSE { EConst (CBool false) }
  | UNIT { EConst CUnit }
  | LPAREN e=expr COMMA f=expr RPAREN { ETuple (e, f) }
  | LPAREN e=expr RPAREN { e }