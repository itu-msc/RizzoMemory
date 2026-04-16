open Rizzoc

let test_location_creation () =
  let pos1 = { Lexing.pos_fname = "test.rz"; 
               pos_lnum = 5; 
               pos_bol = 40; 
               pos_cnum = 45 } in
  let pos2 = { pos1 with pos_cnum = 48 } in
  let loc = Location.mk pos1 pos2 in
  let loc_str = Location.to_string loc in
  Alcotest.(check bool) "location string not empty" true (String.length loc_str > 0);
  Alcotest.(check bool) "contains filename" true (String.contains loc_str ':')

let test_lexer_error_has_location () =
  let input = "   @invalid" in
  let lexbuf = Lexing.from_string input in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with 
    Lexing.pos_fname = "test_lexer.rz";
    pos_lnum = 3;
    pos_bol = 0;
  };
  
  (try
     let _ = Lexer.read lexbuf in
     Alcotest.fail "expected lexer error"
   with
   | Lexer.Error (loc, msg) ->
       Alcotest.(check string) "lexer message" "Unexpected character: '@'" msg;
       Alcotest.(check int) "line" 3 loc.Location.start_pos.Lexing.pos_lnum
   | _ -> Alcotest.fail "unexpected exception")

let test_string_literal_location_spans_full_literal () =
  let input = "let greeting = \"Hello World!\"\n" in
  match Parser.parse_string_with_filename ~filename:"test_string_location.rizz" input with
  | [Ast.TopLet (_, Ast.EConst (Ast.CString "Hello World!", ann), _)] ->
      let loc = Ast.get_location ann in
      Alcotest.(check int)
        "string literal starts at opening quote"
        15
        (loc.Location.start_pos.Lexing.pos_cnum - loc.Location.start_pos.Lexing.pos_bol);
      Alcotest.(check int)
        "string literal ends after closing quote"
        29
        (loc.Location.end_pos.Lexing.pos_cnum - loc.Location.end_pos.Lexing.pos_bol)
  | _ -> Alcotest.fail "expected top-level string literal binding"

let test_show_error_context () =
  let test_file = "test_location_temp.rz" in
  let oc = open_out test_file in
  Printf.fprintf oc "line 1\n";
  Printf.fprintf oc "line 2 with error\n";
  Printf.fprintf oc "line 3\n";
  close_out oc;
  
  let pos_start = { 
    Lexing.pos_fname = test_file;
    pos_lnum = 2;
    pos_bol = 7;
    pos_cnum = 16;
  } in
  let pos_end = { pos_start with pos_cnum = 21 } in
  let loc = Location.mk pos_start pos_end in
  
  (* Just verify it doesn't crash - actual output is visual *)
  Location.show_error_context loc "This is a test error message";
  Sys.remove test_file;
  Alcotest.(check pass) "error context displayed" () ()

let test_warning () =
  let pos = { 
    Lexing.pos_fname = "test.rz";
    pos_lnum = 10;
    pos_bol = 100;
    pos_cnum = 105;
  } in
  let loc = Location.mk pos pos in
  (* Just verify it doesn't crash *)
  Location.report_warning loc "This is a test warning";
  Alcotest.(check pass) "warning reported" () ()

let rec typ_has_tvar = function
  | Ast.TVar _ -> true
  | Ast.TError | Ast.TUnit | Ast.TInt | Ast.TString | Ast.TBool | Ast.TName _ | Ast.TParam _ -> false
  | Ast.TSignal t | Ast.TLater t | Ast.TDelay t | Ast.TOption t | Ast.TList t | Ast.TChan t -> typ_has_tvar t
  | Ast.TTuple (t1, t2) | Ast.TSync (t1, t2) -> typ_has_tvar t1 || typ_has_tvar t2
  | Ast.TFun (Ast.Cons1 (front, rest), ret) -> typ_has_tvar front || List.exists typ_has_tvar rest || typ_has_tvar ret

let ann_has_tvar : type s. s Ast.ann -> bool = function
  | Ast.Ann_typed (_, typ) -> typ_has_tvar typ
  | Ast.Ann_parsed _ | Ast.Ann_bound _ -> false

let rec pattern_has_tvar : type s. s Ast.pattern -> bool = function
  | Ast.PWildcard ann | Ast.PConst (_, ann) | Ast.PVar (_, ann) -> ann_has_tvar ann
  | Ast.PTuple (p1, p2, ann) -> ann_has_tvar ann || pattern_has_tvar p1 || pattern_has_tvar p2
  | Ast.PSigCons (p1, (_, ann2), ann) | Ast.PStringCons (p1, (_, ann2), ann) ->
      ann_has_tvar ann || ann_has_tvar ann2 || pattern_has_tvar p1
  | Ast.PCtor ((_, ctor_ann), args, ann) ->
      ann_has_tvar ctor_ann || ann_has_tvar ann || List.exists pattern_has_tvar args

let rec expr_has_tvar : type s. s Ast.expr -> bool = function
  | Ast.EConst (_, ann) -> ann_has_tvar ann
  | Ast.EVar (_, ann) -> ann_has_tvar ann
  | Ast.ECtor ((_, ctor_ann), args, ann) ->
      ann_has_tvar ctor_ann || ann_has_tvar ann || List.exists expr_has_tvar args
  | Ast.ELet ((_, name_ann), e1, e2, ann) ->
      ann_has_tvar name_ann || ann_has_tvar ann || expr_has_tvar e1 || expr_has_tvar e2
  | Ast.EFun (params, body, ann) ->
      ann_has_tvar ann || List.exists (fun (_, param_ann) -> ann_has_tvar param_ann) params || expr_has_tvar body
  | Ast.EApp (fn, args, ann) ->
      ann_has_tvar ann || expr_has_tvar fn || List.exists expr_has_tvar args
  | Ast.EUnary (_, expr, ann) ->
      ann_has_tvar ann || expr_has_tvar expr
  | Ast.EBinary (_, e1, e2, ann) ->
      ann_has_tvar ann || expr_has_tvar e1 || expr_has_tvar e2
  | Ast.ETuple (e1, e2, ann) ->
      ann_has_tvar ann || expr_has_tvar e1 || expr_has_tvar e2
  | Ast.ECase (scrutinee, branches, ann) ->
      ann_has_tvar ann
      || expr_has_tvar scrutinee
      || List.exists (fun (pattern, branch, branch_ann) ->
           ann_has_tvar branch_ann || pattern_has_tvar pattern || expr_has_tvar branch) branches
  | Ast.EIfe (c, t, e, ann) ->
      ann_has_tvar ann || expr_has_tvar c || expr_has_tvar t || expr_has_tvar e
  | Ast.EAnno (expr, typ, ann) ->
      ann_has_tvar ann || typ_has_tvar typ || expr_has_tvar expr

let program_has_tvar (program : _ Ast.program) : bool =
  List.exists (function
    | Ast.TopLet ((_, name_ann), expr, ann) ->
        ann_has_tvar name_ann || ann_has_tvar ann || expr_has_tvar expr) program

let parse_and_typecheck input =
  let parsed = Parser.parse_string input in
  let typed_program, errors = Rizzoc.typecheck parsed in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  typed_program

let test_typecheck_normalizes_inner_annotations () =
  let program =
    Parser.parse_string
      "fun entry p =\n\
      \  let swap_nested_left = fun q -> snd (fst q) in\n\
      \  swap_nested_left\n"
  in
  let typed_program, errors = Rizzoc.typecheck program in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  Alcotest.(check bool) "typed program contains no unresolved weak vars" false (program_has_tvar typed_program)

let test_projection_partial_app_survives_refcount () =
  let program =
    Parser.parse_string
      "fun use f p =\n\
      \  f p\n\
      fun entry x =\n\
      \  use snd (1, 2)\n"
  in
  let typed_program, errors = Rizzoc.typecheck program in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  let lowered_program = Rizzoc.apply_typed_transforms typed_program in
  let _ = Rizzoc.ref_count lowered_program in
  Alcotest.(check pass) "projection partial application reaches RC" () ()

let test_annotated_list_map_typechecks () =
  let typed_program =
    parse_and_typecheck
      "fun list_map f lst : ('a -> 'b) -> List 'a -> List 'b =\n\
      \  match lst with\n\
      \  | [] -> []\n\
      \  | x :: xs -> (f x) :: (list_map f xs)\n"
  in
  Alcotest.(check bool) "typed program contains no unresolved weak vars" false (program_has_tvar typed_program)

let location_tests = [
  "location creation", `Quick, test_location_creation;
  "lexer error has location", `Quick, test_lexer_error_has_location;
  "string literal location spans full literal", `Quick, test_string_literal_location_spans_full_literal;
  "error context display", `Quick, test_show_error_context;
  "warning reporting", `Quick, test_warning;
  "typecheck normalizes inner annotations", `Quick, test_typecheck_normalizes_inner_annotations;
  "projection partial application survives refcount", `Quick, test_projection_partial_app_survives_refcount;
  "annotated list_map typechecks", `Quick, test_annotated_list_map_typechecks;
]


