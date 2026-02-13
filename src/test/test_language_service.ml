open Rizzoc

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

let tests = [
  "valid document diagnostics", `Quick, test_valid_document_has_no_diagnostics;
  "invalid document diagnostics", `Quick, test_invalid_document_reports_diagnostic;
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
