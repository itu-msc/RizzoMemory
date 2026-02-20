
%{
open! Ast

let check_unique_params params =
  let rec go seen = function
    | [] -> ()
    | x :: xs ->
      if List.mem x seen then
        failwith ("Duplicate parameter name: " ^ x)
      else
        go (x :: seen) xs
  in
  go [] params

let mkloc start_pos end_pos = Ann_parsed (Location.mk start_pos end_pos)

let rec tuple_expr_of_list start_pos end_pos = function
  | [] | [_] -> failwith "Tuple must contain at least 2 elements"
  | [a; b] -> ETuple (a, b, mkloc start_pos end_pos)
  | a :: rest -> ETuple (a, tuple_expr_of_list start_pos end_pos rest, mkloc start_pos end_pos)

let rec tuple_pattern_of_list start_pos end_pos = function
  | [] | [_] -> failwith "Tuple pattern must contain at least 2 elements"
  | [a; b] -> PTuple (a, b, mkloc start_pos end_pos)
  | a :: rest -> PTuple (a, tuple_pattern_of_list start_pos end_pos rest, mkloc start_pos end_pos)

(* When you expand the parser, you can use location tracking helpers:
   - $startpos : starting position of the symbol
   - $endpos   : ending position of the symbol
   - $startpos(x) : starting position of symbol x
   - $endpos(x)   : ending position of symbol x
   
   Example helper function:
   let mkloc start_pos end_pos = Ann_parsed (Location.mk start_pos end_pos) 
   
   Example usage in rules:
   expr:
     | x=ID { (EVar (x, mkloc $startpos $endpos)) }
     | e1=expr PLUS e2=expr
       { EBinary (Add, e1, e2, mkloc $startpos $endpos) }
*)
%}

%token LET FUN MATCH WITH IN
%token EQ CONS COMMA LPAREN RPAREN BAR
%token IF THEN ELSE
%token PIPE_GT ARROW COLON STAR UNDERSCORE EQEQ
%token NEVER WAIT WATCH TAIL SYNC LATERAPP OSTAR DELAY
%token <string> ID
%token <string> TYPE_ID
%token <string> TYPEVAR
%token <int> INT
%token <string> STRING
%token TRUE FALSE UNIT
%token EOF

%start <Ast.parsed Ast.program> main

%nonassoc BELOW_BAR
%nonassoc BAR

%%

main:
	| decls=top_exprs EOF { decls }

top_exprs:
  | { [] }
  | d=top_expr ds=top_exprs { d :: ds }

top_expr:
  | LET name=ID EQ body=expr
    { TLet (name, body, mkloc $startpos $endpos) }
  | FUN name=ID params=nonempty_id_list EQ body=expr
    {
      check_unique_params (List.map fst params);
      TLet (name, EFun (params, body, mkloc $startpos(params) $endpos(body)), mkloc $startpos $endpos)
    }

nonempty_id_list:
  | x=ID { [(x, mkloc $startpos $endpos)] }
  | x=ID xs=nonempty_id_list { (x, mkloc $startpos $endpos) :: xs }

expr:
  | LET x=ID EQ e1=expr IN e2=expr
    { ELet ((x, mkloc $startpos(x) $endpos(x)), e1, e2, mkloc $startpos $endpos) }
  | IF e1=expr THEN e2=expr ELSE e3=expr
    { EIfe (e1, e2, e3, mkloc $startpos $endpos) }
  | MATCH scrutinee=expr WITH leading=opt_leading_bar first=match_case rest=match_case_tail
    { let _ = leading in ECase (scrutinee, first :: rest, mkloc $startpos $endpos) }
  | FUN params=nonempty_id_list ARROW body=expr
    { check_unique_params (List.map fst params); EFun (params, body, mkloc $startpos $endpos) }
  | e=op_expr
      { e }

opt_leading_bar:
  | { () }
  | BAR { () }

match_case:
  | p=pattern ARROW e=expr
  { (p, e, mkloc $startpos $endpos) }

match_case_tail:
  | { [] } %prec BELOW_BAR
  | BAR c=match_case rest=match_case_tail
      { c :: rest }

op_expr:
  | left=op_expr PIPE_GT right=cons_expr
    { EApp (right, [left], mkloc $startpos $endpos) }
  | left=op_expr EQEQ right=cons_expr
    { EBinary (Eq, left, right, mkloc $startpos $endpos) }
  | e=cons_expr
      { e }

cons_expr:
  | left=app_expr CONS right=cons_expr
    { EBinary (SigCons, left, right, mkloc $startpos $endpos) }
  | e=app_expr
      { e }

app_expr:
  | WAIT e1=atom { EUnary(UWait, e1, mkloc $startpos $endpos) }
  | TAIL e1=atom { EUnary(UTail, e1, mkloc $startpos $endpos) }
  | SYNC e1=atom e2=atom { EBinary(BSync, e1, e2, mkloc $startpos $endpos) }
  | WATCH e1=atom { EUnary(UWatch, e1, mkloc $startpos $endpos) }
  | LATERAPP e1=atom e2=atom { EBinary(BLaterApp, e1, e2, mkloc $startpos $endpos) }
  | DELAY e1=atom { EUnary(UDelay, e1, mkloc $startpos $endpos) }
  | OSTAR e1=atom e2=atom { EBinary(BOStar, e1, e2, mkloc $startpos $endpos) }
  | head=atom tail=app_args
      {
        match tail with
        | [] -> head
        | args -> EApp (head, args, mkloc $startpos $endpos)
      }

app_args:
  | { [] }
  | a=atom rest=app_args { a :: rest }

atom:
  | x=ID { EVar (x, mkloc $startpos $endpos) }
  | i=INT { EConst (CInt i, mkloc $startpos $endpos) }
  | s=STRING { EConst (CString s, mkloc $startpos $endpos) }
  | TRUE { EConst (CBool true, mkloc $startpos $endpos) }
  | FALSE { EConst (CBool false, mkloc $startpos $endpos) }
  | UNIT { EConst (CUnit, mkloc $startpos $endpos) }
  | NEVER { EConst (CNever, mkloc $startpos $endpos) }
  | LPAREN es=tuple_expr_list RPAREN { tuple_expr_of_list $startpos $endpos es }
  | LPAREN e=expr COLON ann=type_expr RPAREN { let _ = ann in e }
  | LPAREN e=expr RPAREN { e }

tuple_expr_list:
  | e1=expr COMMA e2=expr rest=tuple_expr_list_tail
      { e1 :: e2 :: rest }

tuple_expr_list_tail:
  | { [] }
  | COMMA e=expr rest=tuple_expr_list_tail
      { e :: rest }

pattern:
  | p=pattern_atom CONS rest=ID
      { PSigCons (p, (rest, mkloc $startpos(rest) $endpos(rest)), mkloc $startpos $endpos) }
  | p=pattern_atom
      { p }

pattern_atom:
  | UNDERSCORE { PWildcard }
  | x=ID { PVar (x, mkloc $startpos $endpos) }
  | i=INT { PConst (CInt i, mkloc $startpos $endpos) }
  | s=STRING { PConst (CString s, mkloc $startpos $endpos) }
  | TRUE { PConst (CBool true, mkloc $startpos $endpos) }
  | FALSE { PConst (CBool false, mkloc $startpos $endpos) }
  | UNIT { PConst (CUnit, mkloc $startpos $endpos) }
  | LPAREN ps=tuple_pattern_list RPAREN { tuple_pattern_of_list $startpos $endpos ps }
  | LPAREN p=pattern RPAREN { p }

tuple_pattern_list:
  | p1=pattern COMMA p2=pattern rest=tuple_pattern_list_tail
      { p1 :: p2 :: rest }

tuple_pattern_list_tail:
  | { [] }
  | COMMA p=pattern rest=tuple_pattern_list_tail
      { p :: rest }

type_expr:
  | ft=fun_type { let _ = ft in () }

fun_type:
  | pt=prod_type { let _ = pt in () }
  | pt=prod_type ARROW ft=fun_type { let _ = (pt, ft) in () }

prod_type:
  | at=app_type { let _ = at in () }
  | at=app_type STAR pt=prod_type { let _ = (at, pt) in () }

app_type:
  | ta=type_atom { let _ = ta in () }
  | at=app_type ta=type_atom { let _ = (at, ta) in () }

type_atom:
  | tid=TYPE_ID { let _ = tid in () }
  | tv=TYPEVAR { let _ = tv in () }
  | LPAREN te=type_expr RPAREN { let _ = te in () }
