open Yojson.Safe.Util

module LS = Rizzoc.Language_service

let ( let* ) x f = match x with Some value -> f value | None -> None

let starts_with ~prefix s =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

let hex_value = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> 10 + Char.code c - Char.code 'a'
  | 'A' .. 'F' as c -> 10 + Char.code c - Char.code 'A'
  | _ -> -1

let percent_decode (s : string) : string =
  let len = String.length s in
  let buf = Buffer.create len in
  let rec go index =
    if index >= len then
      ()
    else
      match s.[index] with
      | '%' when index + 2 < len ->
          let h1 = hex_value s.[index + 1] in
          let h2 = hex_value s.[index + 2] in
          if h1 >= 0 && h2 >= 0 then (
            Buffer.add_char buf (Char.chr ((h1 lsl 4) lor h2));
            go (index + 3))
          else (
            Buffer.add_char buf s.[index];
            go (index + 1))
      | c ->
          Buffer.add_char buf c;
          go (index + 1)
  in
  go 0;
  Buffer.contents buf

let path_of_uri (uri : string) : string =
  if starts_with ~prefix:"file://" uri then
    let raw = String.sub uri 7 (String.length uri - 7) |> percent_decode in
    if String.length raw >= 3 && raw.[0] = '/' && raw.[2] = ':' then
      String.sub raw 1 (String.length raw - 1)
    else
      raw
  else
    uri

let read_message () : Yojson.Safe.t option =
  let rec read_headers content_length =
    match input_line stdin with
    | line ->
        let trimmed = String.trim line in
        if trimmed = "" then
          content_length
        else if starts_with ~prefix:"Content-Length:" trimmed then
          let value =
            String.trim (String.sub trimmed (String.length "Content-Length:") (String.length trimmed - String.length "Content-Length:"))
          in
          read_headers (Some (int_of_string value))
        else
          read_headers content_length
    | exception End_of_file -> None
  in
  match read_headers None with
  | None -> None
  | Some length ->
      let payload = really_input_string stdin length in
      Some (Yojson.Safe.from_string payload)

let send_json (json : Yojson.Safe.t) : unit =
  let payload = Yojson.Safe.to_string json in
  output_string stdout (Printf.sprintf "Content-Length: %d\r\n\r\n" (String.length payload));
  output_string stdout payload;
  flush stdout

let json_of_position (position : LS.position) : Yojson.Safe.t =
  `Assoc [
    ("line", `Int position.line);
    ("character", `Int position.character)
  ]

let json_of_range (range : LS.range) : Yojson.Safe.t =
  `Assoc [
    ("start", json_of_position range.start_pos);
    ("end", json_of_position range.end_pos)
  ]

let json_of_severity = function
  | LS.Error -> `Int 1
  | LS.Warning -> `Int 2
  | LS.Information -> `Int 3

let json_of_diagnostic (diagnostic : LS.diagnostic) : Yojson.Safe.t =
  `Assoc [
    ("range", json_of_range diagnostic.range);
    ("severity", json_of_severity diagnostic.severity);
    ("source", `String diagnostic.source);
    ("message", `String diagnostic.message)
  ]

let json_of_symbol_kind = function
  | LS.Function -> `Int 12
  | LS.Variable -> `Int 13

let json_of_document_symbol (symbol : LS.document_symbol) : Yojson.Safe.t =
  `Assoc [
    ("name", `String symbol.name);
    ("kind", json_of_symbol_kind symbol.kind);
    ("range", json_of_range symbol.range);
    ("selectionRange", json_of_range symbol.selection_range)
  ]

let json_of_location ~uri (range : LS.range) : Yojson.Safe.t =
  `Assoc [
    ("uri", `String uri);
    ("range", json_of_range range)
  ]

let json_of_hover (hover : LS.hover_info) : Yojson.Safe.t =
  `Assoc [
    ("contents", `Assoc [
      ("kind", `String "plaintext");
      ("value", `String hover.contents)
    ]);
    ("range", json_of_range hover.range)
  ]

let semantic_token_type_index = function
  | LS.SemanticFunction -> 0
  | LS.SemanticVariable -> 1
  | LS.SemanticType -> 2

let semantic_token_modifier_mask (token : LS.semantic_token) : int =
  if LS.semantic_token_is_declaration token then 1 else 0

let semantic_token_length (token : LS.semantic_token) : int =
  let token_range = LS.semantic_token_range token in
  max 1 (token_range.end_pos.character - token_range.start_pos.character)

let compare_semantic_token (left : LS.semantic_token) (right : LS.semantic_token) : int =
  let left_range = LS.semantic_token_range left in
  let right_range = LS.semantic_token_range right in
  let by_line = compare left_range.start_pos.line right_range.start_pos.line in
  if by_line <> 0 then by_line
  else compare left_range.start_pos.character right_range.start_pos.character

let json_of_semantic_tokens (tokens : LS.semantic_token list) : Yojson.Safe.t =
  let sorted = List.sort compare_semantic_token tokens in
  let rec encode prev_line prev_char acc = function
    | [] -> List.rev acc
    | token :: rest ->
        let token_range = LS.semantic_token_range token in
        let line = token_range.start_pos.line in
        let start_char = token_range.start_pos.character in
        let delta_line = line - prev_line in
        let delta_start = if delta_line = 0 then start_char - prev_char else start_char in
        let encoded =
          [
            `Int delta_line;
            `Int delta_start;
            `Int (semantic_token_length token);
            `Int (semantic_token_type_index (LS.semantic_token_kind token));
            `Int (semantic_token_modifier_mask token);
          ]
        in
        encode line start_char (List.rev_append encoded acc) rest
  in
  `Assoc [
    ("data", `List (encode 0 0 [] sorted))
  ]

let response ~id ~result =
  send_json (`Assoc [
    ("jsonrpc", `String "2.0");
    ("id", id);
    ("result", result)
  ])

let error_response ~id ~code ~message =
  send_json (`Assoc [
    ("jsonrpc", `String "2.0");
    ("id", id);
    ("error", `Assoc [
      ("code", `Int code);
      ("message", `String message)
    ])
  ])

let publish_diagnostics ~uri diagnostics =
  send_json (`Assoc [
    ("jsonrpc", `String "2.0");
    ("method", `String "textDocument/publishDiagnostics");
    ("params", `Assoc [
      ("uri", `String uri);
      ("diagnostics", `List (List.map json_of_diagnostic diagnostics))
    ])
  ])

type document_state = {
  text: string;
}

let documents : (string, document_state) Hashtbl.t = Hashtbl.create 16

let text_for_uri uri =
  match Hashtbl.find_opt documents uri with
  | Some state -> Some state.text
  | None -> None

let analyze_and_publish uri text =
  let filename = path_of_uri uri in
  let result = LS.analyze_document ~uri ~filename:(Some filename) ~text in
  publish_diagnostics ~uri result.diagnostics

let handle_did_open params =
  let* text_document = params |> member "textDocument" |> to_option (fun x -> x) in
  let* uri = text_document |> member "uri" |> to_option to_string in
  let* text = text_document |> member "text" |> to_option to_string in
  Hashtbl.replace documents uri { text };
  analyze_and_publish uri text;
  Some ()

let handle_did_change params =
  let* text_document = params |> member "textDocument" |> to_option (fun x -> x) in
  let* uri = text_document |> member "uri" |> to_option to_string in
  let* changes = params |> member "contentChanges" |> to_option to_list in
  let* first_change = match changes with first :: _ -> Some first | [] -> None in
  let* text = first_change |> member "text" |> to_option to_string in
  Hashtbl.replace documents uri { text };
  analyze_and_publish uri text;
  Some ()

let handle_did_save params =
  let* text_document = params |> member "textDocument" |> to_option (fun x -> x) in
  let* uri = text_document |> member "uri" |> to_option to_string in
  let text =
    match params |> member "text" |> to_option to_string with
    | Some t -> t
    | None ->
        (match Hashtbl.find_opt documents uri with
         | Some state -> state.text
         | None -> "")
  in
  Hashtbl.replace documents uri { text };
  analyze_and_publish uri text;
  Some ()

let handle_did_close params =
  let* text_document = params |> member "textDocument" |> to_option (fun x -> x) in
  let* uri = text_document |> member "uri" |> to_option to_string in
  Hashtbl.remove documents uri;
  publish_diagnostics ~uri [];
  Some ()

let process_request ~method_name ~id ~params =
  match method_name with
  | "initialize" ->
      let _ = params in
      response
        ~id
        ~result:(`Assoc [
          ("capabilities", `Assoc [
            ("textDocumentSync", `Int 1);
            ("documentSymbolProvider", `Bool true);
            ("definitionProvider", `Bool true);
            ("hoverProvider", `Bool true);
            ("semanticTokensProvider", `Assoc [
              ("legend", `Assoc [
                ("tokenTypes", `List [`String "function"; `String "variable"; `String "type"]);
                ("tokenModifiers", `List [`String "declaration"])
              ]);
              ("full", `Bool true)
            ])
          ]);
          ("serverInfo", `Assoc [
            ("name", `String "rizzolsp");
            ("version", `String "0.1.0")
          ])
        ])
  | "textDocument/documentSymbol" ->
      let result =
        let* text_document = params |> member "textDocument" |> to_option (fun x -> x) in
        let* uri = text_document |> member "uri" |> to_option to_string in
        let* text = text_for_uri uri in
        let filename = path_of_uri uri in
        let symbols = LS.document_symbols ~uri ~filename:(Some filename) ~text in
        Some (`List (List.map json_of_document_symbol symbols))
      in
      response ~id ~result:(match result with Some value -> value | None -> `List [])
  | "textDocument/definition" ->
      let result =
        let* text_document = params |> member "textDocument" |> to_option (fun x -> x) in
        let* uri = text_document |> member "uri" |> to_option to_string in
        let* text = text_for_uri uri in
        let* position_json = params |> member "position" |> to_option (fun x -> x) in
        let* line = position_json |> member "line" |> to_option to_int in
        let* character = position_json |> member "character" |> to_option to_int in
        let filename = path_of_uri uri in
        let definition =
          LS.definition_at_position
            ~uri
            ~filename:(Some filename)
            ~text
            ~position:{ LS.line; character }
        in
        Some (
          match definition with
          | Some defn -> json_of_location ~uri defn.range
          | None -> `Null)
      in
      response ~id ~result:(match result with Some value -> value | None -> `Null)
  | "textDocument/hover" ->
      let result =
        let* text_document = params |> member "textDocument" |> to_option (fun x -> x) in
        let* uri = text_document |> member "uri" |> to_option to_string in
        let* text = text_for_uri uri in
        let* position_json = params |> member "position" |> to_option (fun x -> x) in
        let* line = position_json |> member "line" |> to_option to_int in
        let* character = position_json |> member "character" |> to_option to_int in
        let filename = path_of_uri uri in
        let hover =
          LS.hover_at_position
            ~uri
            ~filename:(Some filename)
            ~text
            ~position:{ LS.line; character }
        in
        Some (match hover with Some h -> json_of_hover h | None -> `Null)
      in
      response ~id ~result:(match result with Some value -> value | None -> `Null)
  | "textDocument/semanticTokens/full" ->
      let result =
        let* text_document = params |> member "textDocument" |> to_option (fun x -> x) in
        let* uri = text_document |> member "uri" |> to_option to_string in
        let* text = text_for_uri uri in
        let filename = path_of_uri uri in
        let tokens = LS.semantic_tokens ~uri ~filename:(Some filename) ~text in
        Some (json_of_semantic_tokens tokens)
      in
      response ~id ~result:(match result with Some value -> value | None -> `Assoc [("data", `List [])])
  | "shutdown" -> response ~id ~result:`Null
  | _ -> error_response ~id ~code:(-32601) ~message:("Method not found: " ^ method_name)

let process_notification ~method_name ~params =
  match method_name with
  | "initialized" -> ()
  | "textDocument/didOpen" -> ignore (handle_did_open params)
  | "textDocument/didChange" -> ignore (handle_did_change params)
  | "textDocument/didSave" -> ignore (handle_did_save params)
  | "textDocument/didClose" -> ignore (handle_did_close params)
  | _ -> ()

let rec loop shutdown_received =
  match read_message () with
  | None -> ()
  | Some message ->
      let method_name = message |> member "method" |> to_option to_string in
      let id = message |> member "id" |> to_option (fun x -> x) in
      let params = message |> member "params" in
      begin
        match method_name, id with
        | Some "exit", None ->
            if shutdown_received then
              exit 0
            else
              exit 1
        | Some method_name, Some id ->
            process_request ~method_name ~id ~params;
            loop (shutdown_received || method_name = "shutdown")
        | Some method_name, None ->
            process_notification ~method_name ~params;
            loop shutdown_received
        | None, Some id ->
            error_response ~id ~code:(-32600) ~message:"Invalid Request";
            loop shutdown_received
        | None, None -> loop shutdown_received
      end

let () =
  set_binary_mode_in stdin true;
  set_binary_mode_out stdout true;
  loop false
