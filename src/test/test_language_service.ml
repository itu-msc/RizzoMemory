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

let test_valid_document_has_no_diagnostics () =
  let text = "let x = 1\nfun id y = y\n" in
  let result = Language_service.analyze_document ~uri:"file:///test.rizz" ~filename:None ~text in
  Alcotest.(check int) "diagnostic count" 0 (List.length result.Language_service.diagnostics)

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

let test_unexpected_bracket_reports_hint () =
  let text = "let x = [1\n" in
  let result = Language_service.analyze_document ~uri:"file:///test.rizz" ~filename:None ~text in
  match result.Language_service.diagnostics with
  | [] -> Alcotest.fail "expected lexer diagnostic"
  | first :: _ ->
      let message = first.Language_service.message in
      let has_bracket_hint =
        contains_substring ~text:message ~substring:"bracket"
        || contains_substring ~text:message ~substring:"Unexpected character: ["
      in
      if not has_bracket_hint then
        Alcotest.failf "mentions bracket unsupported (message was: %s)" message

let has_semantic_token ~(tokens : Language_service.semantic_token list) ~(line : int) ~(character : int) ~(kind : Language_service.semantic_token_kind) ~(declaration : bool) : bool =
  List.exists
    (fun (token : Language_service.semantic_token) ->
      let token_range = Language_service.semantic_token_range token in
      token_range.start_pos.line = line
      && token_range.start_pos.character = character
      && Language_service.semantic_token_kind token = kind
      && Language_service.semantic_token_is_declaration token = declaration)
    tokens

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

let test_semantic_tokens_include_function_parameters () =
  let text = "fun const x = x :: never\n" in
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

let tests = [
  "valid document diagnostics", `Quick, test_valid_document_has_no_diagnostics;
  "invalid document diagnostics", `Quick, test_invalid_document_reports_diagnostic;
  "missing paren gives friendly diagnostic", `Quick, test_missing_paren_reports_friendly_message;
  "malformed match branch gives hint", `Quick, test_malformed_match_branch_reports_hint;
  "unexpected bracket gives hint", `Quick, test_unexpected_bracket_reports_hint;
  "semantic tokens MVP", `Quick, test_semantic_tokens_mvp;
  "semantic tokens local let declaration", `Quick, test_semantic_tokens_include_local_let_declaration;
  "semantic tokens builtin operators", `Quick, test_semantic_tokens_include_builtin_operators;
  "semantic tokens function parameters", `Quick, test_semantic_tokens_include_function_parameters;
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
]
