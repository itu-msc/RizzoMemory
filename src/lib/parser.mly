
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

let rec tuple_expr_of_list = function
  | [] | [_] -> failwith "Tuple must contain at least 2 elements"
  | [a; b] -> ETuple (a, b)
  | a :: rest -> ETuple (a, tuple_expr_of_list rest)

let rec tuple_pattern_of_list = function
  | [] | [_] -> failwith "Tuple pattern must contain at least 2 elements"
  | [a; b] -> PTuple (a, b)
  | a :: rest -> PTuple (a, tuple_pattern_of_list rest)

let with_expr_loc start_pos end_pos expr =
  let loc = Location.mk start_pos end_pos in
  Ast.register_expr_location expr loc;
  expr

let with_top_loc start_pos end_pos top =
  let loc = Location.mk start_pos end_pos in
  Ast.register_top_expr_location top loc;
  top

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

%start <Ast.program> main

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
    { with_top_loc $startpos $endpos (TLet (name, body)) }
  | FUN name=ID params=nonempty_id_list EQ body=expr
    { with_top_loc $startpos $endpos (TLet (name, with_expr_loc $startpos(body) $endpos(body) (EFun (params, body)))) }

nonempty_id_list:
  | x=ID { [x] }
  | x=ID xs=nonempty_id_list { x :: xs }

expr:
  | LET x=ID EQ e1=expr IN e2=expr
    { with_expr_loc $startpos $endpos (ELet (x, e1, e2)) }
  | IF e1=expr THEN e2=expr ELSE e3=expr
    { with_expr_loc $startpos $endpos (EIfe (e1, e2, e3)) }
  | MATCH scrutinee=expr WITH leading=opt_leading_bar first=match_case rest=match_case_tail
    { let _ = leading in with_expr_loc $startpos $endpos (ECase (scrutinee, first :: rest)) }
  | FUN params=nonempty_id_list ARROW body=expr
    { check_unique_params params; with_expr_loc $startpos $endpos (EFun (params, body)) }
  | e=op_expr
      { e }

opt_leading_bar:
  | { () }
  | BAR { () }

match_case:
  | p=pattern ARROW e=expr
      { (p, e) }

match_case_tail:
  | { [] } %prec BELOW_BAR
  | BAR c=match_case rest=match_case_tail
      { c :: rest }

op_expr:
  | left=op_expr PIPE_GT right=cons_expr
    { with_expr_loc $startpos $endpos (EApp (right, [left])) }
  | left=op_expr EQEQ right=cons_expr
    { with_expr_loc $startpos $endpos (EBinary (Eq, left, right)) }
  | e=cons_expr
      { e }

cons_expr:
  | left=app_expr CONS right=cons_expr
    { with_expr_loc $startpos $endpos (EBinary (SigCons, left, right)) }
  | e=app_expr
      { e }

app_expr:
  | WAIT e1=atom { with_expr_loc $startpos $endpos (EUnary(UWait, e1)) }
  | TAIL e1=atom { with_expr_loc $startpos $endpos (EUnary(UTail, e1)) }
  | SYNC e1=atom e2=atom { with_expr_loc $startpos $endpos (EBinary(BSync, e1, e2)) }
  | WATCH e1=atom { with_expr_loc $startpos $endpos (EUnary(UWatch, e1)) }
  | LATERAPP e1=atom e2=atom { with_expr_loc $startpos $endpos (EBinary(BLaterApp, e1, e2)) }
  | DELAY e1=atom { with_expr_loc $startpos $endpos (EUnary(UDelay, e1)) }
  | OSTAR e1=atom e2=atom { with_expr_loc $startpos $endpos (EBinary(BOStar, e1, e2)) }
  | head=atom tail=app_args
      {
        match tail with
        | [] -> head
        | args -> with_expr_loc $startpos $endpos (EApp (head, args))
      }

app_args:
  | { [] }
  | a=atom rest=app_args { a :: rest }

atom:
  | x=ID { with_expr_loc $startpos $endpos (EVar x) }
  | i=INT { with_expr_loc $startpos $endpos (EConst (CInt i)) }
  | s=STRING { with_expr_loc $startpos $endpos (EConst (CString s)) }
  | TRUE { with_expr_loc $startpos $endpos (EConst (CBool true)) }
  | FALSE { with_expr_loc $startpos $endpos (EConst (CBool false)) }
  | UNIT { with_expr_loc $startpos $endpos (EConst CUnit) }
  | NEVER { with_expr_loc $startpos $endpos (EConst CNever) }
  | LPAREN es=tuple_expr_list RPAREN { with_expr_loc $startpos $endpos (tuple_expr_of_list es) }
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
  | p=pattern_atom CONS rest=pattern
      { PSigCons (p, rest) }
  | p=pattern_atom
      { p }

pattern_atom:
  | UNDERSCORE { PWildcard }
  | x=ID { PVar x }
  | i=INT { PConst (CInt i) }
  | s=STRING { PConst (CString s) }
  | TRUE { PConst (CBool true) }
  | FALSE { PConst (CBool false) }
  | UNIT { PConst CUnit }
  | LPAREN ps=tuple_pattern_list RPAREN { tuple_pattern_of_list ps }
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