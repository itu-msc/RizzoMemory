open Rizzoc

let contains_substring ~(text : string) ~(substring : string) : bool =
  let text_len = String.length text in
  let sub_len = String.length substring in
  let rec go index =
    if index + sub_len > text_len then
      false
    else if String.sub text index sub_len = substring then
      true
    else
      go (index + 1)
  in
  if sub_len = 0 then true else go 0

let leading_whitespace_width (line : string) : int =
  let rec go index =
    if index < String.length line then
      match line.[index] with
      | ' ' | '\t' -> go (index + 1)
      | _ -> index
    else
      index
  in
  go 0

let drop_leading_whitespace (line : string) (count : int) : string =
  let rec go index remaining =
    if index >= String.length line || remaining = 0 then
      String.sub line index (String.length line - index)
    else
      match line.[index] with
      | ' ' | '\t' -> go (index + 1) (remaining - 1)
      | _ -> String.sub line index (String.length line - index)
  in
  go 0 count

let dedent_text_for_test (text : string) : string =
  let lines = String.split_on_char '\n' text |> Array.of_list in
  let start = ref 0 in
  let stop = ref (Array.length lines - 1) in
  while !start <= !stop && String.trim lines.(!start) = "" do
    incr start
  done;
  while !stop >= !start && String.trim lines.(!stop) = "" do
    decr stop
  done;
  if !start > !stop then
    ""
  else
    let indent = ref max_int in
    for index = !start to !stop do
      let line = lines.(index) in
      if String.trim line <> "" then
        indent := min !indent (leading_whitespace_width line)
    done;
    let indent = if !indent = max_int then 0 else !indent in
    let dedented_lines =
      List.init (!stop - !start + 1) (fun offset ->
        let line = lines.(!start + offset) in
        if String.trim line = "" then "" else drop_leading_whitespace line indent)
    in
    String.concat "\n" dedented_lines

let test_dedent_code_block_trims_common_indentation () =
  let text =
    "                match mysync with\n"
    ^ "                | Left(xs') -> interleave_l(str_concat, xs', buzz_signal)\n"
    ^ "                | Right(ys') -> interleave_l_right(str_concat, fizz_signal,\n"
    ^ "                                                   ys')\n"
    ^ "                | Both(xs', ys') -> interleave(str_concat, xs', ys')\n"
  in
  let dedented = dedent_text_for_test text in
  Alcotest.(check string)
    "dedents shared indentation"
    ("match mysync with\n"
     ^ "| Left(xs') -> interleave_l(str_concat, xs', buzz_signal)\n"
     ^ "| Right(ys') -> interleave_l_right(str_concat, fizz_signal,\n"
     ^ "                                   ys')\n"
     ^ "| Both(xs', ys') -> interleave(str_concat, xs', ys')")
    dedented

let test_expression_info_block_dedents_pretty_printed_expression () =
  let expr =
    Ast_test_helpers.case
      (Ast_test_helpers.var "mysync")
      [
        Ast_test_helpers.pctor (Ast_test_helpers.name "Left") [ Ast_test_helpers.pvar "xs'" ],
        Ast_test_helpers.app
          (Ast_test_helpers.var "interleave_l")
          [
            Ast_test_helpers.var "str_concat";
            Ast_test_helpers.var "xs'";
            Ast_test_helpers.var "buzz_signal";
          ];
        Ast_test_helpers.pctor (Ast_test_helpers.name "Right") [ Ast_test_helpers.pvar "ys'" ],
        Ast_test_helpers.app
          (Ast_test_helpers.var "interleave_l_right")
          [
            Ast_test_helpers.var "str_concat";
            Ast_test_helpers.var "fizz_signal";
            Ast_test_helpers.var "ys'";
          ];
        Ast_test_helpers.pctor (Ast_test_helpers.name "Both") [ Ast_test_helpers.pvar "xs'"; Ast_test_helpers.pvar "ys'" ],
        Ast_test_helpers.app
          (Ast_test_helpers.var "interleave")
          [
            Ast_test_helpers.var "str_concat";
            Ast_test_helpers.var "xs'";
            Ast_test_helpers.var "ys'";
          ];
      ]
  in
  let expected =
    "\n\nExpr:\n```rizz\n"
    ^ dedent_text_for_test (Format.asprintf "%a" Ast.pp_expr expr)
    ^ "\n```"
  in
  Alcotest.(check string)
    "expression block uses dedented pretty-printed code"
    expected
    (Language_service.expression_info_block expr)

let test_valid_document_has_no_diagnostics () =
  let text = "let x = 1\nfun id y = y\n" in
  let result = Language_service.analyze_document ~uri:"file:///test.rizz" ~filename:None ~text in
  Alcotest.(check int) "diagnostic count" 0 (List.length result.Language_service.diagnostics)

let test_document_can_use_implicit_signal_prelude () =
  let text =
    "fun entry x : Int -> Int =\n"
    ^ "  head (map (fun y -> y) (x :: never))\n"
  in
  let result = Language_service.analyze_document ~uri:"file:///test.rizz" ~filename:None ~text in
  Alcotest.(check int) "signal prelude diagnostic count" 0 (List.length result.Language_service.diagnostics)

let test_stdlib_document_excludes_itself_from_implicit_prelude () =
  match Source_units.default_source_units () with
  | [] -> Alcotest.fail "expected at least one stdlib source"
  | Source_units.Source_text { filename; text } :: _ ->
      let result = Language_service.analyze_document ~uri:"" ~filename:(Some filename) ~text in
      Alcotest.(check int) "stdlib file diagnostic count" 0 (List.length result.Language_service.diagnostics)
  | Source_units.Source_file _ :: _ -> Alcotest.fail "expected stdlib source text units"

let test_invalid_document_reports_diagnostic () =
  let text = "let x = @\n" in
  let result = Language_service.analyze_document ~uri:"file:///test.rizz" ~filename:None ~text in
  match result.Language_service.diagnostics with
  | [] -> Alcotest.fail "expected at least one diagnostic"
  | first :: _ ->
      Alcotest.(check bool) "message not empty" true (String.length first.Language_service.message > 0);
      Alcotest.(check int) "line is zero-based" 0 first.Language_service.range.start_pos.line

let test_missing_paren_reports_friendly_message () =
  let text =
    "fun main x =\n"
    ^ "  match x with\n"
    ^ "  | 4 -> (x\n"
    ^ "  | 6 -> x\n"
  in
  let result = Language_service.analyze_document ~uri:"file:///test.rizz" ~filename:None ~text in
  match result.Language_service.diagnostics with
  | [] -> Alcotest.fail "expected parse diagnostic"
  | first :: _ ->
      Alcotest.(check bool)
        "does not expose menhir internals"
        false
        (contains_substring ~text:first.Language_service.message ~substring:"MenhirBasics.Error");
      Alcotest.(check bool)
        "mentions missing close paren"
        true
        (contains_substring ~text:first.Language_service.message ~substring:"missing ')'" );
      Alcotest.(check int)
        "range points to opening delimiter line"
        2
        first.Language_service.range.start_pos.line

let test_malformed_match_branch_reports_hint () =
  let text =
    "fun main x =\n"
    ^ "  match x with\n"
    ^ "  | 1 x\n"
  in
  let result = Language_service.analyze_document ~uri:"file:///test.rizz" ~filename:None ~text in
  match result.Language_service.diagnostics with
  | [] -> Alcotest.fail "expected parse diagnostic"
  | first :: _ ->
      Alcotest.(check bool)
        "mentions expected match branch shape"
        true
        (contains_substring ~text:first.Language_service.message ~substring:"expected '| <pattern> -> <expr>'")

let test_missing_bracket_reports_hint () =
  let text = "let x = [1\n" in
  let result = Language_service.analyze_document ~uri:"file:///test.rizz" ~filename:None ~text in
  match result.Language_service.diagnostics with
  
  | [] -> Alcotest.fail "expected lexer diagnostic"
  | first :: _ ->
    let message = first.Language_service.message in
    let has_bracket_hint =
      contains_substring ~text:message ~substring:"Syntax error: missing ']'"
    in
    if not has_bracket_hint then
      Alcotest.failf "bracket hint not found in diagnostic message: %s" message

let has_semantic_token ~(tokens : Language_service.semantic_token list) ~(line : int) ~(character : int) ~(kind : Language_service.semantic_token_kind) ~(declaration : bool) : bool =
  List.exists
    (fun (token : Language_service.semantic_token) ->
      let token_range = Language_service.semantic_token_range token in
      token_range.start_pos.line = line
      && token_range.start_pos.character = character
      && Language_service.semantic_token_kind token = kind
      && Language_service.semantic_token_is_declaration token = declaration)
    tokens

let has_semantic_token_span
    ~(tokens : Language_service.semantic_token list)
    ~(line : int)
    ~(start_character : int)
    ~(end_character : int)
    ~(kind : Language_service.semantic_token_kind)
    ~(declaration : bool) : bool =
  List.exists
    (fun (token : Language_service.semantic_token) ->
      let token_range = Language_service.semantic_token_range token in
      token_range.start_pos.line = line
      && token_range.start_pos.character = start_character
      && token_range.end_pos.line = line
      && token_range.end_pos.character = end_character
      && Language_service.semantic_token_kind token = kind
      && Language_service.semantic_token_is_declaration token = declaration)
    tokens

let completion_labels ~(text : string) ~(position : Language_service.position) : string list =
  let completion =
    Language_service.completions_at_position
      ~uri:"file:///test.rizz"
      ~filename:None
      ~text
      ~position
  in
  List.map (fun (item : Language_service.completion_item) -> item.label) completion.items

let completion_item
    ~(text : string)
    ~(position : Language_service.position)
    ~(name : string) : Language_service.completion_item option =
  let completion =
    Language_service.completions_at_position
      ~uri:"file:///test.rizz"
      ~filename:None
      ~text
      ~position
  in
  List.find_opt (fun (item : Language_service.completion_item) -> String.equal item.label name) completion.items

let completion_has_label ~(labels : string list) ~(name : string) : bool =
  List.exists (fun label -> String.equal label name) labels

let starts_with ~(prefix : string) (s : string) : bool =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix

let test_completions_include_local_scope_and_top_level () =
  let text =
    "fun addOne x =\n"
    ^ "  let y = x in\n"
    ^ "  y\n"
  in
  let labels =
    completion_labels
      ~text
      ~position:{ Language_service.line = 2; character = 2 }
  in
  Alcotest.(check bool)
    "includes function parameter"
    true
    (completion_has_label ~labels ~name:"x");
  Alcotest.(check bool)
    "includes local let binding"
    true
    (completion_has_label ~labels ~name:"y");
  Alcotest.(check bool)
    "includes top-level function"
    true
    (completion_has_label ~labels ~name:"addOne")

let test_completions_respect_case_branch_scope () =
  let text =
    "fun pick s =\n"
    ^ "  match s with\n"
    ^ "  | Left (x) -> x\n"
    ^ "  | Right (y) -> y\n"
  in
  let labels =
    completion_labels
      ~text
      ~position:{ Language_service.line = 2; character = 16 }
  in
  Alcotest.(check bool)
    "includes first-branch binding"
    true
    (completion_has_label ~labels ~name:"x");
  Alcotest.(check bool)
    "does not include second-branch binding"
    false
    (completion_has_label ~labels ~name:"y")

let test_completions_include_builtins_and_constructors () =
  let text =
    "fun main x =\n"
    ^ "  x\n"
  in
  let labels =
    completion_labels
      ~text
      ~position:{ Language_service.line = 1; character = 2 }
  in
  Alcotest.(check bool)
    "includes builtin function"
    true
    (completion_has_label ~labels ~name:"add");
  Alcotest.(check bool)
    "includes clock builtin"
    true
    (completion_has_label ~labels ~name:"clock");
  Alcotest.(check bool)
    "does not include internal string intrinsic"
    false
    (completion_has_label ~labels ~name:"string_concat");
  Alcotest.(check bool)
    "includes constructor"
    true
    (completion_has_label ~labels ~name:"Some")

let test_completions_filter_by_prefix () =
  let text = "fun main x = st\n" in
  let labels =
    completion_labels
      ~text
      ~position:{ Language_service.line = 0; character = 15 }
  in
  Alcotest.(check bool)
    "contains matching builtin"
    true
    (completion_has_label ~labels ~name:"start_event_loop");
  Alcotest.(check bool)
    "all completions share prefix"
    true
    (List.for_all (starts_with ~prefix:"st") labels)

let test_semantic_tokens_mvp () =
  let text =
    "fun id x = x\n"
    ^ "let y = id\n"
  in
  let tokens = Language_service.semantic_tokens ~uri:"file:///test.rizz" ~filename:None ~text in
  Alcotest.(check bool)
    "function declaration token"
    true
    (has_semantic_token
       ~tokens
       ~line:0
       ~character:4
       ~kind:Language_service.SemanticFunction
       ~declaration:false);
  Alcotest.(check bool)
    "variable declaration token"
    true
     (has_semantic_token
       ~tokens
       ~line:1
       ~character:4
       ~kind:Language_service.SemanticVariable
       ~declaration:false);
  Alcotest.(check bool)
    "function reference token"
    true
    (has_semantic_token
       ~tokens
       ~line:1
       ~character:8
       ~kind:Language_service.SemanticFunction
       ~declaration:false)

let test_semantic_tokens_include_local_let_declaration () =
  let text =
    "fun main x =\n"
    ^ "  let y = x in\n"
    ^ "  y\n"
  in
  let tokens = Language_service.semantic_tokens ~uri:"file:///test.rizz" ~filename:None ~text in
  Alcotest.(check bool)
    "local let binding declaration token"
    true
    (has_semantic_token
       ~tokens
       ~line:1
       ~character:6
       ~kind:Language_service.SemanticVariable
       ~declaration:false)

let test_semantic_tokens_include_builtin_operators () =
  let text =
    "fun main x =\n"
    ^ "  let mysync = sync 2 4 in\n"
    ^ "  let y = wait mysync in\n"
    ^ "  let z = watch mysync in\n"
    ^ "  delay y\n"
  in
  let tokens = Language_service.semantic_tokens ~uri:"file:///test.rizz" ~filename:None ~text in
  Alcotest.(check bool)
    "sync token as function"
    true
    (has_semantic_token
       ~tokens
       ~line:1
       ~character:15
       ~kind:Language_service.SemanticFunction
       ~declaration:false);
  Alcotest.(check bool)
    "wait token as function"
    true
    (has_semantic_token
       ~tokens
       ~line:2
       ~character:10
       ~kind:Language_service.SemanticFunction
       ~declaration:false);
  Alcotest.(check bool)
    "watch token as function"
    true
    (has_semantic_token
       ~tokens
       ~line:3
       ~character:10
       ~kind:Language_service.SemanticFunction
       ~declaration:false);
  Alcotest.(check bool)
    "delay token as function"
    true
    (has_semantic_token
       ~tokens
       ~line:4
       ~character:2
       ~kind:Language_service.SemanticFunction
       ~declaration:false)

let test_semantic_tokens_include_builtin_function_references () =
  let text =
    "fun main s =\n"
    ^ "  console_out_signal s\n"
  in
  let tokens = Language_service.semantic_tokens ~uri:"file:///test.rizz" ~filename:None ~text in
  Alcotest.(check bool)
    "builtin function reference token"
    true
    (has_semantic_token
       ~tokens
       ~line:1
       ~character:2
       ~kind:Language_service.SemanticFunction
       ~declaration:false)

let test_pipe_left_operand_keeps_function_reference_metadata () =
  let text =
    "fun update_greeting x = x\n"
    ^ "fun entry later_x : Later Int -> Later Int =\n"
    ^ "  update_greeting |> later_x\n"
  in
  let tokens = Language_service.semantic_tokens ~uri:"file:///test.rizz" ~filename:None ~text in
  Alcotest.(check bool)
    "pipe left operand token spans the full function name"
    true
    (has_semantic_token_span
       ~tokens
       ~line:2
       ~start_character:2
       ~end_character:17
       ~kind:Language_service.SemanticFunction
       ~declaration:false);
  match Language_service.definition_at_position
          ~uri:"file:///test.rizz"
          ~filename:None
          ~text
          ~position:{ Language_service.line = 2; character = 2 }
  with
  | None -> Alcotest.fail "expected definition for pipe left operand"
  | Some defn ->
      Alcotest.(check string) "pipe left operand resolves to update_greeting" "update_greeting" defn.Language_service.name;
      Alcotest.(check int) "pipe left operand definition line" 0 defn.Language_service.range.start_pos.line

let test_semantic_tokens_include_function_parameters () =
  let text = "fun consf x = x :: never\n" in
  let tokens = Language_service.semantic_tokens ~uri:"file:///test.rizz" ~filename:None ~text in
  Alcotest.(check bool)
    "function parameter token"
    true
    (has_semantic_token
       ~tokens
       ~line:0
       ~character:10
       ~kind:Language_service.SemanticVariable
       ~declaration:false)

let test_semantic_tokens_include_function_parameter_patterns () =
  let text = "fun unwrap (Some(x)) = x\n" in
  let tokens = Language_service.semantic_tokens ~uri:"file:///test.rizz" ~filename:None ~text in
  Alcotest.(check bool)
    "constructor token in function parameter pattern"
    true
    (has_semantic_token
       ~tokens
       ~line:0
       ~character:12
       ~kind:Language_service.SemanticType
       ~declaration:false);
  Alcotest.(check bool)
    "pattern binding token in function parameter pattern"
    true
    (has_semantic_token
       ~tokens
       ~line:0
       ~character:17
       ~kind:Language_service.SemanticVariable
       ~declaration:false)

let test_semantic_tokens_after_line_comment () =
  let text =
    "let x = 1 // keep this comment\n"
    ^ "let y = x\n"
  in
  let tokens = Language_service.semantic_tokens ~uri:"file:///test.rizz" ~filename:None ~text in
  Alcotest.(check bool)
     "second let declaration keeps correct line after line comment"
    true
     (has_semantic_token
       ~tokens
       ~line:1
       ~character:4
       ~kind:Language_service.SemanticVariable
       ~declaration:false);
    Alcotest.(check bool)
     "reference after line comment keeps correct line"
     true
     (has_semantic_token
       ~tokens
       ~line:1
       ~character:8
       ~kind:Language_service.SemanticVariable
       ~declaration:false)

let test_semantic_tokens_after_block_comment () =
  let text =
    "let x = 1 /* first line\n"
    ^ "second line */\n"
    ^ "let y = x\n"
  in
  let tokens = Language_service.semantic_tokens ~uri:"file:///test.rizz" ~filename:None ~text in
  Alcotest.(check bool)
     "second let declaration keeps correct line after block comment"
    true
     (has_semantic_token
       ~tokens
       ~line:2
       ~character:4
       ~kind:Language_service.SemanticVariable
       ~declaration:false);
    Alcotest.(check bool)
     "reference after block comment keeps correct line"
     true
     (has_semantic_token
        ~tokens
        ~line:2
        ~character:8
        ~kind:Language_service.SemanticVariable
        ~declaration:false)

let test_typed_top_level_function_symbols_and_tokens () =
  let text =
    "fun first_signal s : Signal String -> String = s\n"
    ^ "let alias = first_signal\n"
  in
  let symbols = Language_service.document_symbols ~uri:"file:///test.rizz" ~filename:None ~text in
  (match symbols with
   | first :: _ ->
       Alcotest.(check string) "first symbol name" "first_signal" first.Language_service.name;
       Alcotest.(check int) "typed function symbol kind" 12
         (match first.Language_service.kind with
          | Language_service.Function -> 12
          | Language_service.Variable -> 13)
   | [] -> Alcotest.fail "expected typed function symbol");
  let tokens = Language_service.semantic_tokens ~uri:"file:///test.rizz" ~filename:None ~text in
  Alcotest.(check bool)
    "typed function declaration token"
    true
    (has_semantic_token
       ~tokens
       ~line:0
       ~character:4
       ~kind:Language_service.SemanticFunction
       ~declaration:false);
  Alcotest.(check bool)
    "typed function reference token"
    true
    (has_semantic_token
       ~tokens
       ~line:1
       ~character:12
       ~kind:Language_service.SemanticFunction
       ~declaration:false)

let test_hover_and_completion_for_typed_top_level_function () =
  let text =
    "fun first_signal s : Signal String -> String = s\n"
    ^ "fun use x = x\n"
  in
  (match Language_service.hover_at_position
          ~uri:"file:///test.rizz"
          ~filename:None
          ~text
          ~position:{ Language_service.line = 0; character = 4 }
   with
   | None -> Alcotest.fail "expected hover for typed top-level function"
   | Some hover ->
       Alcotest.(check bool)
         "hover mentions top-level function"
         true
         (contains_substring
            ~text:hover.Language_service.contents
            ~substring:"top-level function: first_signal"));
  match completion_item
          ~text
          ~position:{ Language_service.line = 1; character = 12 }
          ~name:"first_signal"
  with
  | None -> Alcotest.fail "expected annotated top-level function completion"
  | Some item -> Alcotest.(check int) "completion kind is function" 3 item.Language_service.kind

let test_hover_on_top_type_definition_uses_definition_text () =
  let text =
    "type MyOption 'a =\n"
    ^ "    | MNothing\n"
    ^ "    | MJust('a)\n"
    ^ "\n"
    ^ "fun some x = MJust (x)\n"
  in
  match Language_service.hover_at_position
          ~uri:"file:///test.rizz"
          ~filename:None
          ~text
          ~position:{ Language_service.line = 0; character = 0 }
  with
  | None -> Alcotest.fail "expected hover for type definition"
  | Some hover ->
      Alcotest.(check bool)
        "hover shows type definition text"
        true
        (contains_substring ~text:hover.Language_service.contents ~substring:"type MyOption 'a =");
      Alcotest.(check bool)
        "hover uses a syntax-highlighted code block"
        true
        (contains_substring ~text:hover.Language_service.contents ~substring:"```rizz");
      Alcotest.(check bool)
        "hover includes constructors"
        true
        (contains_substring ~text:hover.Language_service.contents ~substring:"MNothing");
      Alcotest.(check bool)
        "hover includes constructor args"
        true
        (contains_substring ~text:hover.Language_service.contents ~substring:"MJust('a)");
      Alcotest.(check bool)
        "hover omits expression block"
        false
        (contains_substring ~text:hover.Language_service.contents ~substring:"Expr:");
      Alcotest.(check int)
        "hover range starts at type keyword"
        0
        hover.Language_service.range.start_pos.character

let test_hover_on_function_parameter_uses_name_range () =
  let text = "fun pair first second = first\n" in
  match Language_service.hover_at_position
          ~uri:"file:///test.rizz"
          ~filename:None
          ~text
          ~position:{ Language_service.line = 0; character = 10 }
  with
  | None -> Alcotest.fail "expected hover for function parameter"
  | Some hover ->
      Alcotest.(check bool)
        "hover mentions parameter"
        true
        (contains_substring ~text:hover.Language_service.contents ~substring:"parameter first");
      Alcotest.(check int)
        "hover range starts at parameter name"
        9
        hover.Language_service.range.start_pos.character;
      Alcotest.(check int)
        "hover range ends after parameter name"
        14
        hover.Language_service.range.end_pos.character;
      Alcotest.(check bool)
        "hover omits sibling parameter text"
        false
        (contains_substring ~text:hover.Language_service.contents ~substring:"second")

let test_hover_on_function_parameter_pattern_uses_name_range () =
  let text = "fun unwrap (Some(x)) = x\n" in
  match Language_service.hover_at_position
          ~uri:"file:///test.rizz"
          ~filename:None
          ~text
          ~position:{ Language_service.line = 0; character = 17 }
  with
  | None -> Alcotest.fail "expected hover for function parameter pattern binding"
  | Some hover ->
      Alcotest.(check bool)
        "hover mentions pattern binding"
        true
        (contains_substring ~text:hover.Language_service.contents ~substring:"pattern binding x");
      Alcotest.(check int)
        "hover range starts at pattern name"
        17
        hover.Language_service.range.start_pos.character;
      Alcotest.(check int)
        "hover range ends after pattern name"
        18
        hover.Language_service.range.end_pos.character

let test_hover_inside_string_literal_uses_full_range () =
  let text = "let greeting = \"Hello World!\"\n" in
  match Language_service.hover_at_position
          ~uri:"file:///test.rizz"
          ~filename:None
          ~text
          ~position:{ Language_service.line = 0; character = 20 }
  with
  | None -> Alcotest.fail "expected hover inside string literal"
  | Some hover ->
      Alcotest.(check bool)
        "hover reports string constant"
        true
        (contains_substring ~text:hover.Language_service.contents ~substring:"constant Hello World!");
      Alcotest.(check int)
        "hover range starts at opening quote"
        15
        hover.Language_service.range.start_pos.character;
      Alcotest.(check bool)
        "hover range extends beyond interior character"
        true
        (hover.Language_service.range.end_pos.character > 20)

let test_hover_on_local_let_binding_uses_name_range () =
  let text =
    "fun entry p =\n"
    ^ "  let t = ((\"Hello world!\", 42), 1337) in\n"
    ^ "  snd (fst t)\n"
  in
  match Language_service.hover_at_position
          ~uri:"file:///test.rizz"
          ~filename:None
          ~text
          ~position:{ Language_service.line = 1; character = 6 }
  with
  | None -> Alcotest.fail "expected hover for local let binding"
  | Some hover ->
      Alcotest.(check bool)
        "hover mentions let binding"
        true
        (contains_substring ~text:hover.Language_service.contents ~substring:"let-binding t");
      Alcotest.(check int)
        "hover range starts at binding name"
        6
        hover.Language_service.range.start_pos.character;
      Alcotest.(check int)
        "hover range ends after binding name"
        7
        hover.Language_service.range.end_pos.character;
      Alcotest.(check bool)
        "hover omits enclosing let expression"
        false
        (contains_substring ~text:hover.Language_service.contents ~substring:"Expr:");
      Alcotest.(check bool)
        "type section starts on its own paragraph"
        true
        (contains_substring ~text:hover.Language_service.contents ~substring:"\n\nType:\n```")

let test_hover_on_match_pattern_binding_uses_name_range () =
  let text =
    "fun entry p =\n"
    ^ "  match p with\n"
    ^ "  | (x, y) -> x\n"
  in
  match Language_service.hover_at_position
          ~uri:"file:///test.rizz"
          ~filename:None
          ~text
          ~position:{ Language_service.line = 2; character = 5 }
  with
  | None -> Alcotest.fail "expected hover for match pattern binding"
  | Some hover ->
      Alcotest.(check bool)
        "hover mentions pattern binding"
        true
        (contains_substring ~text:hover.Language_service.contents ~substring:"pattern binding x");
      Alcotest.(check int)
        "hover range starts at pattern name"
        5
        hover.Language_service.range.start_pos.character;
      Alcotest.(check int)
        "hover range ends after pattern name"
        6
        hover.Language_service.range.end_pos.character;
      Alcotest.(check bool)
        "hover omits enclosing match expression"
        false
        (contains_substring ~text:hover.Language_service.contents ~substring:"match expression");
      Alcotest.(check bool)
        "hover omits enclosing expression block"
        false
        (contains_substring ~text:hover.Language_service.contents ~substring:"Expr:")

let test_hover_on_wildcard_pattern_uses_pattern_range_and_type () =
  let text =
    "fun entry p : Int * Bool -> Int =\n"
    ^ "  match p with\n"
    ^ "  | (_, y) -> 1\n"
  in
  match Language_service.hover_at_position
          ~uri:"file:///test.rizz"
          ~filename:None
          ~text
          ~position:{ Language_service.line = 2; character = 5 }
  with
  | None -> Alcotest.fail "expected hover for wildcard pattern"
  | Some hover ->
      Alcotest.(check bool)
        "hover mentions wildcard pattern"
        true
        (contains_substring ~text:hover.Language_service.contents ~substring:"wildcard pattern");
      Alcotest.(check int)
        "hover range starts at underscore"
        5
        hover.Language_service.range.start_pos.character;
      Alcotest.(check int)
        "hover range ends after underscore"
        6
        hover.Language_service.range.end_pos.character;
      Alcotest.(check bool)
        "hover shows inferred wildcard type"
        true
        (contains_substring ~text:hover.Language_service.contents ~substring:"Type:\n```rizz\nInt\n```");
      Alcotest.(check bool)
        "hover omits enclosing match expression"
        false
        (contains_substring ~text:hover.Language_service.contents ~substring:"match expression")

let test_hover_on_function_use_shows_usage_and_definition_types () =
  let text =
    "fun id x = x\n"
    ^ "let y = id 1\n"
  in
  match Language_service.hover_at_position
          ~uri:"file:///test.rizz"
          ~filename:None
          ~text
          ~position:{ Language_service.line = 1; character = 8 }
  with
  | None -> Alcotest.fail "expected hover for function use"
  | Some hover ->
      Alcotest.(check bool)
        "hover shows usage type"
        true
        (contains_substring ~text:hover.Language_service.contents ~substring:"Type:\n```rizz");
      Alcotest.(check bool)
        "hover shows definition type"
        true
        (contains_substring
           ~text:hover.Language_service.contents
           ~substring:"Definition type:\n```rizz")

let string_of_range (range : Language_service.range) : string =
  Printf.sprintf
    "%d:%d-%d:%d"
    range.start_pos.line
    range.start_pos.character
    range.end_pos.line
    range.end_pos.character

let test_rename_top_level_function_updates_declaration_and_use () =
  let text = "fun id x = x\nlet y = id 1\n" in
  match Language_service.rename_at_position
          ~uri:"file:///test.rizz"
          ~filename:None
          ~text
          ~position:{ Language_service.line = 1; character = 8 }
  with
  | None -> Alcotest.fail "expected rename info"
  | Some rename ->
      Alcotest.(check string) "selected range" "1:8-1:10" (string_of_range rename.Language_service.range);
      Alcotest.(check (list string))
        "rename edits"
        ["0:4-0:6"; "1:8-1:10"]
        (List.map string_of_range rename.Language_service.edits)

let test_rename_local_binding_respects_shadowing () =
  let text = "let x = 1\nlet y = let x = x in x\nlet z = x\n" in
  match Language_service.rename_at_position
          ~uri:"file:///test.rizz"
          ~filename:None
          ~text
          ~position:{ Language_service.line = 1; character = 21 }
  with
  | None -> Alcotest.fail "expected rename info"
  | Some rename ->
      Alcotest.(check (list string))
        "rename edits"
        ["1:12-1:13"; "1:21-1:22"]
        (List.map string_of_range rename.Language_service.edits)

let test_rename_constructor_updates_declaration_expression_and_pattern () =
  let text =
    "type Option = None | Some(Int)\n"
    ^ "fun make x = Some(x)\n"
    ^ "fun read x = match x with | Some(y) -> y | None -> 0\n"
  in
  match Language_service.rename_at_position
          ~uri:"file:///test.rizz"
          ~filename:None
          ~text
          ~position:{ Language_service.line = 1; character = 13 }
  with
  | None -> Alcotest.fail "expected rename info"
  | Some rename ->
      Alcotest.(check (list string))
        "rename edits"
        ["0:21-0:25"; "1:13-1:17"; "2:28-2:32"]
        (List.map string_of_range rename.Language_service.edits)

let test_rename_function_parameter_pattern_updates_body_and_pattern () =
  let text = "fun unwrap (Some(x)) = x\n" in
  match Language_service.rename_at_position
          ~uri:"file:///test.rizz"
          ~filename:None
          ~text
          ~position:{ Language_service.line = 0; character = 23 }
  with
  | None -> Alcotest.fail "expected rename info for function parameter pattern"
  | Some rename ->
      Alcotest.(check string) "selected range" "0:23-0:24" (string_of_range rename.Language_service.range);
      Alcotest.(check (list string))
        "rename edits"
        ["0:17-0:18"; "0:23-0:24"]
        (List.map string_of_range rename.Language_service.edits)

let test_completion_in_function_parameter_pattern_sees_binding () =
  let text = "fun unwrap (Some(x)) = x\n" in
  let labels =
    completion_labels
      ~text
      ~position:{ Language_service.line = 0; character = 23 }
  in
  Alcotest.(check bool)
    "completions include pattern binding in function body"
    true
    (completion_has_label ~labels ~name:"x")

let test_rename_rejects_external_stdlib_symbol () =
  let text = "fun entry x : Int -> Int =\n  map (fun y -> y) x\n" in
  match Language_service.rename_at_position
          ~uri:"file:///test.rizz"
          ~filename:None
          ~text
          ~position:{ Language_service.line = 1; character = 2 }
  with
  | None -> ()
  | Some _ -> Alcotest.fail "expected stdlib rename to be rejected"

let tests = [
  "dedent code block trims common indentation", `Quick, test_dedent_code_block_trims_common_indentation;
  "expression info block dedents pretty-printed expression", `Quick, test_expression_info_block_dedents_pretty_printed_expression;
  "valid document diagnostics", `Quick, test_valid_document_has_no_diagnostics;
  "implicit signal prelude diagnostics", `Quick, test_document_can_use_implicit_signal_prelude;
  "stdlib document excludes self from implicit prelude", `Quick, test_stdlib_document_excludes_itself_from_implicit_prelude;
  "invalid document diagnostics", `Quick, test_invalid_document_reports_diagnostic;
  "missing paren gives friendly diagnostic", `Quick, test_missing_paren_reports_friendly_message;
  "malformed match branch gives hint", `Quick, test_malformed_match_branch_reports_hint;
  "missing bracket gives hint", `Quick, test_missing_bracket_reports_hint;
  "semantic tokens MVP", `Quick, test_semantic_tokens_mvp;
  "semantic tokens local let declaration", `Quick, test_semantic_tokens_include_local_let_declaration;
  "semantic tokens builtin operators", `Quick, test_semantic_tokens_include_builtin_operators;
  "semantic tokens builtin function references", `Quick, test_semantic_tokens_include_builtin_function_references;
  "pipe left operand keeps function metadata", `Quick, test_pipe_left_operand_keeps_function_reference_metadata;
   "semantic tokens function parameters", `Quick, test_semantic_tokens_include_function_parameters;
    "semantic tokens function parameter patterns", `Quick, test_semantic_tokens_include_function_parameter_patterns;
   "semantic tokens after line comment", `Quick, test_semantic_tokens_after_line_comment;
   "semantic tokens after block comment", `Quick, test_semantic_tokens_after_block_comment;
   "typed top-level function symbols and tokens", `Quick, test_typed_top_level_function_symbols_and_tokens;
   "completion local and top-level scope", `Quick, test_completions_include_local_scope_and_top_level;
   "completion case branch scope", `Quick, test_completions_respect_case_branch_scope;
   "completion includes builtins and constructors", `Quick, test_completions_include_builtins_and_constructors;
   "completion prefix filtering", `Quick, test_completions_filter_by_prefix;
    "completion function parameter patterns", `Quick, test_completion_in_function_parameter_pattern_sees_binding;
   "hover and completion for typed top-level function", `Quick, test_hover_and_completion_for_typed_top_level_function;
  "hover on type definition uses definition text", `Quick, test_hover_on_top_type_definition_uses_definition_text;
  "hover on function parameter uses name range", `Quick, test_hover_on_function_parameter_uses_name_range;
    "hover on function parameter pattern uses name range", `Quick, test_hover_on_function_parameter_pattern_uses_name_range;
   "hover inside string literal uses full range", `Quick, test_hover_inside_string_literal_uses_full_range;
   "hover on local let binding uses name range", `Quick, test_hover_on_local_let_binding_uses_name_range;
   "hover on match pattern binding uses name range", `Quick, test_hover_on_match_pattern_binding_uses_name_range;
   "hover on wildcard pattern uses range and type", `Quick, test_hover_on_wildcard_pattern_uses_pattern_range_and_type;
   "hover on function use shows usage and definition types", `Quick, test_hover_on_function_use_shows_usage_and_definition_types;
   "document symbols", `Quick,
    (fun () ->
      let text = "let x = 1\nfun id y = y\nlet y = x\n" in
      let symbols : Language_service.document_symbol list =
        Language_service.document_symbols ~uri:"file:///test.rizz" ~filename:None ~text
      in
      let names = List.map (fun (s : Language_service.document_symbol) -> s.name) symbols in
      Alcotest.(check (list string)) "symbol names" ["x"; "id"; "y"] names;
      match symbols with
      | first :: _ ->
          Alcotest.(check int) "first symbol line" 0 first.range.start_pos.line
      | [] -> Alcotest.fail "expected symbols");
  "top-level definition lookup", `Quick,
    (fun () ->
      let text = "let x = 1\nfun id y = y\nlet y = x\n" in
      match Language_service.definition_at_position
              ~uri:"file:///test.rizz"
              ~filename:None
              ~text
              ~position:{ Language_service.line = 2; character = 8 }
      with
      | None -> Alcotest.fail "expected definition"
      | Some defn ->
          Alcotest.(check string) "name" "x" defn.Language_service.name;
          Alcotest.(check int) "def line" 0 defn.Language_service.range.start_pos.line);
  "stdlib definition lookup returns stdlib filename", `Quick,
    (fun () ->
      let text = "fun entry x : Int -> Int =\n  map (fun y -> y) x\n" in
      match Language_service.definition_at_position
              ~uri:"file:///test.rizz"
              ~filename:None
              ~text
              ~position:{ Language_service.line = 1; character = 2 }
      with
      | None -> Alcotest.fail "expected stdlib definition"
      | Some defn ->
          Alcotest.(check string) "name" "map" defn.Language_service.name;
          Alcotest.(check bool)
            "stdlib filename"
            true
            (contains_substring ~text:defn.Language_service.filename ~substring:"signal.rizz");
          Alcotest.(check int) "stdlib line" 11 defn.Language_service.range.start_pos.line);
  "hover returns node info", `Quick,
    (fun () ->
      let text = "let x = 1\nlet y = x\n" in
      match Language_service.hover_at_position
              ~uri:"file:///test.rizz"
              ~filename:None
              ~text
              ~position:{ Language_service.line = 1; character = 8 }
      with
      | None -> Alcotest.fail "expected hover"
      | Some hover ->
          Alcotest.(check bool) "hover has text" true (String.length hover.Language_service.contents > 0));
  "hover returns top-level name info", `Quick,
    (fun () ->
      let text = "let x = 1\n" in
      match Language_service.hover_at_position
              ~uri:"file:///test.rizz"
              ~filename:None
              ~text
              ~position:{ Language_service.line = 0; character = 4 }
      with
      | None -> Alcotest.fail "expected top-level hover"
      | Some hover ->
          Alcotest.(check bool)
            "hover mentions top-level binding"
            true
            (contains_substring ~text:hover.Language_service.contents ~substring:"top-level binding: x"));
  "rename top-level function", `Quick, test_rename_top_level_function_updates_declaration_and_use;
  "rename local binding shadowing", `Quick, test_rename_local_binding_respects_shadowing;
  "rename constructor occurrences", `Quick, test_rename_constructor_updates_declaration_expression_and_pattern;
  "rename function parameter pattern", `Quick, test_rename_function_parameter_pattern_updates_body_and_pattern;
  "rename rejects stdlib symbol", `Quick, test_rename_rejects_external_stdlib_symbol;
]
