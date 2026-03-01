
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
%token EFFECTFUL
%token EQ CONS COMMA LPAREN RPAREN BAR
%token IF THEN ELSE
%token PIPE_GT ARROW COLON STAR UNDERSCORE EQEQ
%token NEVER WAIT WATCH TAIL SYNC LATERAPP OSTAR DELAY
%token TYPE_SIGNAL TYPE_LATER TYPE_DELAY TYPE_SYNC
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
// %left PIPE_GT EQEQ

%%

main:
	| decls=top_exprs EOF { decls }

top_exprs:
  | { [] }
  | d=top_expr ds=top_exprs { d :: ds }

top_expr:
  | effect_dec=option(EFFECTFUL) 
    LET name=ID te_opt=option(type_annotation) EQ body=expr
    { 
      if Option.is_some effect_dec then Effectful.mark_effectful name;
      if Option.is_none te_opt 
      then TopLet (name, body, mkloc $startpos $endpos) 
      else TopLet (name, EAnno(body, Option.get te_opt, mkloc $startpos(body) $endpos(body)), mkloc $startpos $endpos)
    }
  | effect_dec=option(EFFECTFUL)
    FUN name=ID params=nonempty_id_list te_opt=option(type_annotation) EQ body=expr
    {
      check_unique_params (List.map fst params);
      if Option.is_some effect_dec then Effectful.mark_effectful name;
      match te_opt with
      | None -> TopLet (name, EFun (params, body, mkloc $startpos(params) $endpos(body)), mkloc $startpos $endpos)
      | Some te -> TopLet (name, EAnno(EFun (params, body, mkloc $startpos(params) $endpos(body)), te, mkloc $startpos(body) $endpos(body)), mkloc $startpos $endpos)
    }

nonempty_id_list:
  | x=ID { [(x, mkloc $startpos $endpos)] }
  | x=ID xs=nonempty_id_list { (x, mkloc $startpos $endpos) :: xs }

expr:
  | LET x=ID te_opt=option(type_annotation) EQ e1=expr IN e2=expr
    { let name = (x, mkloc $startpos(x) $endpos(x)) in
      match te_opt with
      | None    -> ELet (name, e1, e2, mkloc $startpos $endpos) 
      | Some te -> ELet (name, EAnno(e1, te, mkloc $startpos(e1) $endpos(e1)), e2, mkloc $startpos $endpos) 
    }
  | IF e1=expr THEN e2=expr ELSE e3=expr
    { EIfe (e1, e2, e3, mkloc $startpos $endpos) }
  | MATCH scrutinee=expr WITH leading=opt_leading_bar first=match_case rest=match_case_tail
    { let _ = leading in ECase (scrutinee, first :: rest, mkloc $startpos $endpos) }
  | FUN params=nonempty_id_list ARROW body=expr
    { check_unique_params (List.map fst params); EFun (params, body, mkloc $startpos $endpos) }
  | e=pipe_and_cmp_op
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

pipe_and_cmp_op:
  | left=pipe_and_cmp_op PIPE_GT right=cons_expr
    { (* creates laterapp (delay left) right*)
      EBinary (BLaterApp, EUnary (UDelay, left, mkloc $startpos(left) $endpos(left)), right, mkloc $startpos $endpos) }
  | left=pipe_and_cmp_op EQEQ right=cons_expr
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
  | head=atom args=atom+ { EApp (head, args, mkloc $startpos $endpos) }
  | e = atom { e }

atom:
  | name=TYPE_ID { ECtor ((name, mkloc $startpos(name) $endpos(name)), [], mkloc $startpos $endpos) }
  | name=TYPE_ID LPAREN fields=separated_nonempty_list(COMMA, expr) RPAREN
    { ECtor ((name, mkloc $startpos(name) $endpos(name)), fields, mkloc $startpos $endpos) }
  | x=ID { EVar (x, mkloc $startpos $endpos) }
  | i=INT { EConst (CInt i, mkloc $startpos $endpos) }
  | s=STRING { EConst (CString s, mkloc $startpos $endpos) }
  | TRUE { EConst (CBool true, mkloc $startpos $endpos) }
  | FALSE { EConst (CBool false, mkloc $startpos $endpos) }
  | UNIT { EConst (CUnit, mkloc $startpos $endpos) }
  | NEVER { EConst (CNever, mkloc $startpos $endpos) }
  | LPAREN es=tuple_expr_list RPAREN { tuple_expr_of_list $startpos $endpos es }
  | LPAREN e=expr COLON ann=type_expr RPAREN { EAnno (e, ann, mkloc $startpos $endpos) }
  | LPAREN e=expr RPAREN { e }

tuple_expr_list:
  | e1=expr COMMA e2=expr rest=tuple_expr_list_tail
      { e1 :: e2 :: rest }

tuple_expr_list_tail:
  | { [] }
  | COMMA e=expr rest=tuple_expr_list_tail
      { e :: rest }

pattern:
  | name=TYPE_ID LPAREN ps=ctor_pattern_args RPAREN
    { PCtor ((name, mkloc $startpos(name) $endpos(name)), ps, mkloc $startpos $endpos) }
  | name=TYPE_ID
    { PCtor ((name, mkloc $startpos(name) $endpos(name)), [], mkloc $startpos $endpos) }
  | p=pattern_atom CONS rest=ID
      { PSigCons (p, (rest, mkloc $startpos(rest) $endpos(rest)), mkloc $startpos $endpos) }
  | p=pattern_atom
      { p }

ctor_pattern_args:
  | p=pattern rest=comma_separated_patterns
    { p :: rest }


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
  | p1=pattern COMMA p2=pattern rest=comma_separated_patterns
      { p1 :: p2 :: rest }

comma_separated_patterns:
  | { [] }
  | COMMA p=pattern rest=comma_separated_patterns
      { p :: rest }

type_annotation:
  | COLON te=type_expr { te }

type_expr:
  | ft=fun_type { ft }

fun_type:
  | ts=separated_nonempty_list(ARROW, prod_type)
    { match ts with
      | [] -> failwith "impossible - nonempty list"
      | [prod] -> prod
      | _ -> 
        let length = List.length ts in
        let param_types = List.take (length - 1) ts in
        let ret_type = List.nth ts (length - 1) in
        TFun (Cons1 (List.hd param_types, List.tl param_types), ret_type) } 
  // | pt=prod_type { pt }

prod_type:
  | at=app_type STAR pt=prod_type { TTuple (at, pt) }
  | at=app_type { at }

app_type:
  | TYPE_SIGNAL ta=type_atom { TSignal ta }
  | TYPE_LATER ta=type_atom { TLater ta }
  | TYPE_DELAY ta=type_atom { TDelay ta }
  | TYPE_SYNC ta1=type_atom ta2=type_atom { TSync (ta1, ta2) }
  | ta=type_atom { ta }
  (* For now just keep it simple - we could certainly add a 'TApp of typ * typ' later  *)
  // | at=app_type ta=type_atoma { failwith "type application ..." }

type_atom:
  | tid=TYPE_ID { 
    match tid with 
    | "Int" -> TInt
    | "String" -> TString
    | "Bool" -> TBool
    | "Unit" -> TUnit
    | _ -> TName tid 
    }
  | tv=TYPEVAR { TParam tv }
  | LPAREN te=type_expr RPAREN { te }
