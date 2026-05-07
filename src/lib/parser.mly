
%{
open! Ast

(* let check_unique_params params =
  let rec go seen = function
    | [] -> ()
    | x :: xs ->
      if List.mem x seen then
        failwith ("Duplicate parameter name: " ^ x)
      else
        go (x :: seen) xs
  in
  go [] params *)

let mkloc start_pos end_pos = Ann_parsed (Location.mk start_pos end_pos)

let binary_ann left right =
  let left_loc = get_location (expr_get_ann left) in
  let right_loc = get_location (expr_get_ann right) in
  mkloc left_loc.start_pos right_loc.end_pos

let nil_expr start_pos end_pos =
  ECtor (("Nil", mkloc start_pos end_pos), [], mkloc start_pos end_pos)

let cons_expr start_pos end_pos head tail =
  ECtor (("Cons", mkloc start_pos end_pos), [head; tail], mkloc start_pos end_pos)

let rec list_expr_of_list start_pos end_pos = function
  | [] -> nil_expr start_pos end_pos
  | head :: tail -> cons_expr start_pos end_pos head (list_expr_of_list start_pos end_pos tail)

let nil_pattern start_pos end_pos =
  PCtor (("Nil", mkloc start_pos end_pos), [], mkloc start_pos end_pos)

let cons_pattern start_pos end_pos head tail =
  PCtor (("Cons", mkloc start_pos end_pos), [head; tail], mkloc start_pos end_pos)

let rec list_pattern_of_list start_pos end_pos = function
  | [] -> nil_pattern start_pos end_pos
  | head :: tail -> cons_pattern start_pos end_pos head (list_pattern_of_list start_pos end_pos tail)

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

%token LET FUN MATCH WITH IN TYPE
%token EFFECTFUL
%token EQ CONS COMMA LPAREN RPAREN LBRACKET RBRACKET BAR
%token IF THEN ELSE
%token PIPE_GT ARROW COLON STAR SLASH PERCENT UNDERSCORE EQEQ PLUS MINUS LT GT LEQ GEQ BANG
%token NEVER WAIT WATCH TAIL SYNC LATERAPP OSTAR DELAY //NOT
%token TYPE_SIGNAL TYPE_LATER TYPE_DELAY
%token <string> ID
%token <string> TYPE_ID
%token <string> TYPEVAR
%token <int> INT
%token <string> STRING
%token TRUE FALSE UNIT
%token EOF

%start <Ast.parsed Ast.program> main

%nonassoc BELOW_BAR
%nonassoc BELOW_CTOR_ARGS
%nonassoc LPAREN
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
      let top_name = (name, mkloc $startpos(name) $endpos(name)) in
      match te_opt with
      | None -> TopLet (top_name, body, mkloc $symbolstartpos $endpos(body)) 
      | Some (te, anno_loc) ->
        TopLet (top_name, EAnno(body, te, anno_loc), mkloc $symbolstartpos $endpos(body))
    }
  | effect_dec=option(EFFECTFUL)
    FUN name=ID params=pattern_atom+ te_opt=option(type_annotation) EQ body=expr
    {
      (* check_unique_params (List.map fst params); *)
      if Option.is_some effect_dec then Effectful.mark_effectful name;
      let top_name = (name, mkloc $startpos(name) $endpos(name)) in
      match te_opt with
      | None -> TopLet (top_name, EFun (params, body, mkloc $startpos(params) $endpos(body)), mkloc $symbolstartpos $endpos(body))
      | Some (te, anno_loc) -> 
        let fun_loc = mkloc $startpos(params) $endpos(body) in
        TopLet (top_name, EAnno(EFun (params, body, fun_loc), te, anno_loc), mkloc $symbolstartpos $endpos(body))
    }
  | TYPE type_name=TYPE_ID type_params=type_param_names EQ BAR? type_ctors=separated_nonempty_list(BAR, type_ctor)
    { let type_name = (type_name, mkloc $startpos(type_name) $endpos(type_name)) in
      TopTypeDef (type_name, type_params, type_ctors, mkloc $startpos $endpos) }

(* Separate rule because these need to be converted to names, idk - that's how I encoded it in AST *)
type_param_names:
  | { [] }
  | tv=TYPEVAR type_vars=type_param_names { (tv, mkloc $startpos(tv) $endpos(tv)) :: type_vars }

type_ctor:
  | ctor_name=TYPE_ID 
    { let loc = mkloc $startpos(ctor_name) $endpos(ctor_name) in
      (ctor_name, loc), [], loc }
  | ctor_name=TYPE_ID LPAREN ctor_args=separated_nonempty_list(COMMA, type_expr) RPAREN
    { let ctor_name = (ctor_name, mkloc $startpos(ctor_name) $endpos(ctor_name)) in
      (ctor_name, ctor_args, mkloc $startpos $endpos) }

expr:
  | LET x=ID te_opt=option(type_annotation) EQ e1=expr IN e2=expr
    { let name = (x, mkloc $startpos(x) $endpos(x)) in
      match te_opt with
      | None    -> ELet (name, e1, e2, mkloc $startpos $endpos) 
      | Some (te, anno_loc) -> ELet (name, EAnno(e1, te, anno_loc), e2, mkloc $startpos $endpos) 
    }
  | IF e1=expr THEN e2=expr ELSE e3=expr
    { EIfe (e1, e2, e3, mkloc $startpos $endpos) }
  | MATCH scrutinee=expr WITH leading=opt_leading_bar first=match_case rest=match_case_tail
    { let _ = leading in ECase (scrutinee, first :: rest, mkloc $startpos $endpos) }
  | FUN params=pattern_atom+ ARROW body=expr
    { (* check_unique_params (List.map fst params); *) EFun (params, body, mkloc $startpos $endpos) }
  | e=pipe_expr
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

pipe_expr:
  | left=pipe_expr PIPE_GT right=eq_expr
    { (* creates laterapp (delay left) right*)
      EBinary (BLaterApp, EUnary (UDelay, left, mkloc $startpos(left) $endpos(left)), right, mkloc $startpos $endpos) }
  | e=eq_expr
      { e }

eq_expr:
  | left=compare_expr EQEQ right=compare_expr
    { EBinary (Eq, left, right, binary_ann left right) }
  | e=compare_expr
    { e }

compare_expr:
  | left=add_expr LT right=add_expr
    { EBinary (Lt, left, right, binary_ann left right) }
  | left=add_expr LEQ right=add_expr
    { EBinary (Leq, left, right, binary_ann left right) }
  | left=add_expr GT right=add_expr
    { EBinary (Gt, left, right, binary_ann left right) }
  | left=add_expr GEQ right=add_expr
    { EBinary (Geq, left, right, binary_ann left right) }
  | e=add_expr
    { e }

add_expr:
  | left=add_expr PLUS right=mul_expr
    { EBinary (Add, left, right, binary_ann left right) }
  | left=add_expr MINUS right=mul_expr
    { EBinary (Sub, left, right, binary_ann left right) }
  | e=mul_expr
    { e }

mul_expr:
  | left=mul_expr STAR right=cons_expr
    { EBinary (Mul, left, right, binary_ann left right) }
  | left=mul_expr SLASH right=cons_expr
    { EBinary (Div, left, right, binary_ann left right) }
  | left=mul_expr PERCENT right=cons_expr
    { EBinary (Mod, left, right, binary_ann left right) }
  | e=cons_expr
    { e }

cons_expr:
  | left=app_expr CONS right=cons_expr
    { EBinary (SigCons, left, right, mkloc $startpos $endpos) }
  | e=app_expr
      { e }

app_expr:
  // | NOT e1=atom { EUnary(UNot, e1, mkloc $startpos $endpos) }
  | BANG e1=atom { EUnary(UNot, e1, mkloc $startpos $endpos) }
  | WAIT e1=atom { EUnary(UWait, e1, mkloc $startpos $endpos) }
  | TAIL e1=atom { EUnary(UTail, e1, mkloc $startpos $endpos) }
  | SYNC e1=atom e2=atom { EBinary(BSync, e1, e2, mkloc $startpos $endpos) }
  | WATCH e1=atom { EUnary(UWatch, e1, mkloc $startpos $endpos) }
  | LATERAPP e1=atom e2=atom { EBinary(BLaterApp, e1, e2, mkloc $startpos $endpos) }
  | DELAY e1=atom { EUnary(UDelay, e1, mkloc $startpos $endpos) }
  | OSTAR e1=atom e2=atom { EBinary(BOStar, e1, e2, mkloc $startpos $endpos) }
  | head=app_head args=atom+ { EApp (head, args, mkloc $startpos $endpos) }
  | e = atom { e }

app_head:
  | x=ID { EVar (x, mkloc $startpos $endpos) }
  | LPAREN e=expr COLON ann=type_expr RPAREN { EAnno (e, ann, mkloc $startpos $endpos) }
  | LPAREN e=expr RPAREN { e }

atom:
  | name=TYPE_ID fields=ctor_fields_opt
    {
      ECtor
        ( (name, mkloc $startpos(name) $endpos(name)),
          fields,
          mkloc $startpos $endpos )
    }
  | x=ID { EVar (x, mkloc $startpos $endpos) }
  | i=INT { EConst (CInt i, mkloc $startpos $endpos) }
  | s=STRING { EConst (CString s, mkloc $startpos $endpos) }
  | TRUE { EConst (CBool true, mkloc $startpos $endpos) }
  | FALSE { EConst (CBool false, mkloc $startpos $endpos) }
  | UNIT { EConst (CUnit, mkloc $startpos $endpos) }
  | NEVER { EConst (CNever, mkloc $startpos $endpos) }
  | LBRACKET RBRACKET { nil_expr $startpos $endpos }
  | LBRACKET es=separated_nonempty_list(COMMA, expr) RBRACKET { list_expr_of_list $startpos $endpos es }
  | LPAREN e1=expr COMMA es=separated_nonempty_list(COMMA, expr) RPAREN { ETuple(e1, List.hd es, List.tl es, mkloc $startpos $endpos) }
  | LPAREN e=expr COLON ann=type_expr RPAREN { EAnno (e, ann, mkloc $startpos $endpos) }
  | LPAREN e=expr RPAREN { e }

ctor_fields_opt:
  | LPAREN fields=separated_nonempty_list(COMMA, expr) RPAREN
    { fields }
  | %prec BELOW_CTOR_ARGS
    { [] }

pattern:
  | name=TYPE_ID ps=option(delimited(LPAREN, separated_list(COMMA, pattern), RPAREN))
    { PCtor ((name, mkloc $startpos(name) $endpos(name)), Option.value ~default:[] ps, mkloc $startpos $endpos) }
  | p=pattern_atom CONS rest=ID
      { PSigCons (p, (rest, mkloc $startpos(rest) $endpos(rest)), mkloc $startpos $endpos) }
  | p=pattern_atom
      { p }

pattern_atom:
  | UNDERSCORE { PWildcard (mkloc $startpos $endpos) }
  | x=ID { PVar (x, mkloc $startpos $endpos) }
  | i=INT { PConst (CInt i, mkloc $startpos $endpos) }
  | s=STRING { PConst (CString s, mkloc $startpos $endpos) }
  | TRUE { PConst (CBool true, mkloc $startpos $endpos) }
  | FALSE { PConst (CBool false, mkloc $startpos $endpos) }
  | UNIT { PConst (CUnit, mkloc $startpos $endpos) }
  | LBRACKET RBRACKET { list_pattern_of_list $startpos $endpos [] }
  | LBRACKET ps=separated_nonempty_list(COMMA, pattern) RBRACKET { list_pattern_of_list $startpos $endpos ps }
  | LPAREN p=pattern COMMA ps=separated_nonempty_list(COMMA, pattern) RPAREN 
    { PTuple (p, List.hd ps, List.tl ps, mkloc $startpos $endpos) }
  | LPAREN p=pattern RPAREN { p }

type_annotation:
  | COLON te=type_expr { (te, mkloc $startpos(te) $endpos(te)) }

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
  | at=app_type STAR pts=separated_nonempty_list(STAR, app_type) { TTuple (at, List.hd pts, List.tl pts) }
  | at=app_type { at }

app_type:
  // Parse type application in one production so Menhir does not need to guess
  // whether another type_atom extends the current application or starts a new one.
  | head=type_apply_head LPAREN ta=type_expr COMMA args=separated_nonempty_list(COMMA, type_expr) RPAREN
    { match args with
      | [] -> head
      | _ -> TApp (head, ta :: args) }
  | head=type_apply_head ta=type_atom { TApp (head, [ta]) }
  | head=type_apply_head { head }

type_apply_head:
  | TYPE_SIGNAL ta=type_atom { TSignal ta }
  | TYPE_LATER ta=type_atom { TLater ta }
  | TYPE_DELAY ta=type_atom { TDelay ta }
  | ta=type_atom { ta }

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
