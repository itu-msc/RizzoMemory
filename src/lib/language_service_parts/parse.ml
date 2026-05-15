open Types
open Document

let closing_delimiter = function
  | '(' -> ')'
  | '[' -> ']'
  | '{' -> '}'
  | c -> c

let range_for_single_char ~(line : int) ~(character : int) : range =
  {
    start_pos = { line = max 0 line; character = max 0 character };
    end_pos = { line = max 0 line; character = max 0 (character + 1) };
  }

let has_unmatched_open_delimiter_before ~(text : string) ~(upto_offset : int) : (char * int * int) option =
  let limit = min (String.length text) (max 0 upto_offset) in
  let rec go index line column in_string escaped stack =
    if index >= limit then
      stack
    else
      let c = text.[index] in
      if c = '\n' then
        go (index + 1) (line + 1) 0 in_string false stack
      else if in_string then
        if escaped then
          go (index + 1) line (column + 1) true false stack
        else if c = '\\' then
          go (index + 1) line (column + 1) true true stack
        else if c = '"' then
          go (index + 1) line (column + 1) false false stack
        else
          go (index + 1) line (column + 1) true false stack
      else
        match c with
        | '"' -> go (index + 1) line (column + 1) true false stack
        | '(' | '[' | '{' -> go (index + 1) line (column + 1) false false ((c, line, column) :: stack)
        | ')' | ']' | '}' ->
            let next_stack =
              match stack with
              | (open_c, _, _) :: rest when closing_delimiter open_c = c -> rest
              | _ -> stack
            in
            go (index + 1) line (column + 1) false false next_stack
        | _ -> go (index + 1) line (column + 1) false false stack
  in
  match go 0 0 0 false false [] with
  | (open_c, line, column) :: _ -> Some (open_c, line, column)
  | [] -> None

let previous_nonempty_line (lines : string array) (line_no : int) : string option =
  let rec go idx =
    if idx < 0 then
      None
    else
      let candidate = String.trim lines.(idx) in
      if candidate = "" then go (idx - 1) else Some candidate
  in
  go (line_no - 1)

let match_syntax_hint ~(text : string) ~(line_no : int) : string option =
  let lines = text |> String.split_on_char '\n' |> Array.of_list in
  let current_trimmed = line_at lines line_no |> Option.map String.trim in
  let previous_trimmed = previous_nonempty_line lines line_no in
  let is_malformed_branch = function
    | Some line -> starts_with ~prefix:"|" line && not (contains_substring ~text:line ~substring:"->")
    | None -> false
  in
  let missing_with_after_match = function
    | Some line -> starts_with ~prefix:"match " line && not (contains_substring ~text:line ~substring:" with")
    | None -> false
  in
  if is_malformed_branch current_trimmed || is_malformed_branch previous_trimmed then
    Some "Syntax error in match branch: expected '| <pattern> -> <expr>'."
  else if missing_with_after_match current_trimmed || missing_with_after_match previous_trimmed then
    Some "Syntax error in match expression: expected 'match <expr> with | <pattern> -> <expr>'."
  else
    None

let friendly_lexer_message (msg : string) : string =
  let prefix = "Unexpected character: " in
  if starts_with ~prefix msg && String.length msg > String.length prefix then
    let payload =
      String.sub msg (String.length prefix) (String.length msg - String.length prefix)
      |> String.trim
    in
    let unexpected_char =
      if String.length payload = 1 then
        Some payload.[0]
      else if String.length payload >= 3 && payload.[0] = '\'' && payload.[2] = '\'' then
        Some payload.[1]
      else
        None
    in
    match unexpected_char with
    | Some ('{' | '}') ->
        "Unexpected brace. '{' and '}' are not part of Rizz syntax."
    | _ -> msg
  else
    msg

let parse_error_details ~(text : string) (loc : Location.t) : (range option * string) =
  let offset = loc.start_pos.Lexing.pos_cnum in
  let line_no = max 0 (loc.start_pos.Lexing.pos_lnum - 1) in
  match has_unmatched_open_delimiter_before ~text ~upto_offset:offset with
  | Some (open_c, line, column) ->
      let close_c = closing_delimiter open_c in
      ( Some (range_for_single_char ~line ~character:column),
        Printf.sprintf
          "Syntax error: missing '%c' to match '%c' at line %d, character %d."
          close_c
          open_c
          (line + 1)
          (column + 1) )
  | None ->
      (match match_syntax_hint ~text ~line_no with
       | Some hint -> (None, hint)
       | None -> (None, "Syntax error: unexpected token."))

let diagnostic_of_type_error ((loc, msg) : Location.t * string) : diagnostic =
  {
    range = range_of_location loc;
    severity = Error;
    message = msg;
    source = "rizzoc";
  }

let parse_with_filename ~(filename : string) (text : string) : (parsed_typed_result, diagnostic) result =
  try
    let parsed = Source_units.parse_document_with_default_prelude ~exclude_paths:[filename] ~filename text in
    let validation_errors = Source_units.validate_program parsed in
    if validation_errors <> [] then
      Ok
        {
          typed_program = [];
          type_definitions = Type_env.empty_env.typedefinitions;
          diagnostics = List.map diagnostic_of_type_error validation_errors;
        }
    else
      let { typed_program; type_definitions; type_errors } : Typecheck.typing_result = Typecheck.typecheck parsed in
      let diagnostics = List.map diagnostic_of_type_error type_errors in
      Ok { typed_program; type_definitions; diagnostics }
  with
  | Lexer.Error (loc, msg) ->
      Error
        {
          range = range_of_location loc;
          severity = Error;
          message = friendly_lexer_message msg;
          source = "rizzoc";
        }
  | Source_units.Parser.Error (loc, msg) ->
      if String.equal loc.Location.start_pos.Lexing.pos_fname filename then
        let range_override, friendly_msg = parse_error_details ~text loc in
        let range = match range_override with Some r -> r | None -> range_of_location loc in
        Error { range; severity = Error; message = friendly_msg; source = "rizzoc" }
      else
        Error { range = range_of_location loc; severity = Error; message = msg; source = "rizzoc" }
  | exn ->
      let loc = Location.mk Lexing.dummy_pos Lexing.dummy_pos in
      let msg = Printf.sprintf "Parse error: %s" (Printexc.to_string exn) in
      Error { range = range_of_location loc; severity = Error; message = msg; source = "rizzoc" }
