open Types

let position_of_lexpos (p : Lexing.position) : position =
  {
    line = max 0 (p.pos_lnum - 1);
    character = max 0 (p.pos_cnum - p.pos_bol);
  }

let range_of_location (loc : Location.t) : range =
  {
    start_pos = position_of_lexpos loc.start_pos;
    end_pos = position_of_lexpos loc.end_pos;
  }

let range_of_ann (ann : _ Ast.ann) : range =
  range_of_location (Ast.get_location ann)

let range_of_name ((_, ann) : _ Ast.name) : range =
  range_of_ann ann

let name_text ((name, _) : _ Ast.name) : string = name

let ann_filename (ann : _ Ast.ann) : string =
  (Ast.get_location ann).Location.start_pos.Lexing.pos_fname

let ann_in_file ~(filename : string) (ann : _ Ast.ann) : bool =
  String.equal (ann_filename ann) filename

let range_contains_position (range : range) (position : position) : bool =
  let after_start =
    position.line > range.start_pos.line
    || (position.line = range.start_pos.line && position.character >= range.start_pos.character)
  in
  let before_end =
    position.line < range.end_pos.line
    || (position.line = range.end_pos.line && position.character <= range.end_pos.character)
  in
  after_start && before_end

let range_span_score (range : range) : int =
  let line_span = max 0 (range.end_pos.line - range.start_pos.line) in
  let char_span = max 0 (range.end_pos.character - range.start_pos.character) in
  (line_span * 10_000) + char_span

let empty_range : range =
  {
    start_pos = { line = 0; character = 0 };
    end_pos = { line = 0; character = 0 };
  }

let first_opt = function
  | [] -> None
  | x :: _ -> Some x

let starts_with ~(prefix : string) (s : string) : bool =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix

let is_identifier_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let is_identifier_rest_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

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

let lines_of_text (text : string) : string array =
  text |> String.split_on_char '\n' |> Array.of_list

let line_at (lines : string array) (line_no : int) : string option =
  if line_no < 0 || line_no >= Array.length lines then None else Some lines.(line_no)

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

let dedent_text (text : string) : string =
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

let file_name ~(uri : string) ~(filename : string option) : string =
  match filename with
  | Some path when String.length path > 0 -> path
  | _ -> if String.length uri = 0 then "<memory>.rizz" else uri
