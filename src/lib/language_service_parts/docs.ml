open Collections
open Types
open Document

type doc_index = string StringMap.t

type cached_doc_index = {
  fingerprint: int * int;
  docs: doc_index;
}

let doc_index_cache : (string, cached_doc_index) Hashtbl.t = Hashtbl.create 16

let doc_key ~(filename : string) ~(name : string) ~(line : int) : string =
  filename ^ "\000" ^ name ^ "\000" ^ string_of_int line

let doc_index_find ~(filename : string) ~(name : string) ~(line : int) (docs : doc_index) : string option =
  StringMap.find_opt (doc_key ~filename ~name ~line) docs

let starts_with_at ~(prefix : string) (text : string) ~(index : int) : bool =
  let prefix_len = String.length prefix in
  index >= 0
  && String.length text >= index + prefix_len
  && String.sub text index prefix_len = prefix

let is_space = function
  | ' ' | '\t' -> true
  | _ -> false

let skip_spaces (text : string) (index : int) : int =
  let rec go index =
    if index < String.length text && is_space text.[index] then go (index + 1) else index
  in
  go index

let keyword_at (text : string) ~(index : int) ~(keyword : string) : bool =
  let keyword_len = String.length keyword in
  starts_with_at ~prefix:keyword text ~index
  && String.length text > index + keyword_len
  && is_space text.[index + keyword_len]

let read_decl_name (text : string) (index : int) : string option =
  let index = skip_spaces text index in
  if index >= String.length text || not (is_identifier_char text.[index]) then
    None
  else
    let rec stop index =
      if index < String.length text && is_identifier_rest_char text.[index] then
        stop (index + 1)
      else
        index
    in
    let end_index = stop (index + 1) in
    Some (String.sub text index (end_index - index))

let top_decl_name_of_line (line : string) : string option =
  let line = String.trim line in
  let line =
    if keyword_at line ~index:0 ~keyword:"@effectful" then
      let effectful_end = String.length "@effectful" in
      String.sub line effectful_end (String.length line - effectful_end) |> String.trim
    else
      line
  in
  if keyword_at line ~index:0 ~keyword:"fun" then
    read_decl_name line (String.length "fun")
  else if keyword_at line ~index:0 ~keyword:"let" then
    read_decl_name line (String.length "let")
  else if keyword_at line ~index:0 ~keyword:"type" then
    read_decl_name line (String.length "type")
  else
    None

let doc_line_content (line : string) : string option =
  let trimmed_left = drop_leading_whitespace line (leading_whitespace_width line) in
  if starts_with ~prefix:"///" trimmed_left then
    let content = String.sub trimmed_left 3 (String.length trimmed_left - 3) in
    if String.length content > 0 && content.[0] = ' ' then
      Some (String.sub content 1 (String.length content - 1))
    else
      Some content
  else
    None

let doc_index_of_source_text ~(filename : string) (text : string) : doc_index =
  let lines = lines_of_text text in
  let rec go line_no pending docs =
    if line_no >= Array.length lines then
      docs
    else
      let line = lines.(line_no) in
      match doc_line_content line with
      | Some content -> go (line_no + 1) (Some (Option.value pending ~default:[] @ [content])) docs
      | None ->
          let docs =
            match pending, top_decl_name_of_line line with
            | Some doc_lines, Some name ->
                let doc = String.concat "\n" doc_lines in
                StringMap.add (doc_key ~filename ~name ~line:line_no) doc docs
            | _ -> docs
          in
          go (line_no + 1) None docs
  in
  go 0 None StringMap.empty

let doc_index_of_source_text_cached ~(filename : string) (text : string) : doc_index =
  let fingerprint = (String.length text, Hashtbl.hash text) in
  match Hashtbl.find_opt doc_index_cache filename with
  | Some cached when cached.fingerprint = fingerprint -> cached.docs
  | _ ->
      let docs = doc_index_of_source_text ~filename text in
      Hashtbl.replace doc_index_cache filename { fingerprint; docs };
      docs

let read_file_text_opt (filename : string) : string option =
  try
    let ic = open_in_bin filename in
    Fun.protect
      ~finally:(fun () -> close_in ic)
      (fun () ->
        let len = in_channel_length ic in
        Some (really_input_string ic len))
  with
  | Sys_error _ -> None

let doc_index_add_source_unit (docs : doc_index) (source_unit : Source_units.source_unit) : doc_index =
  let source_text =
    match source_unit with
    | Source_units.Source_text { filename; text } -> Some (filename, text)
    | Source_units.Source_file filename -> Option.map (fun text -> (filename, text)) (read_file_text_opt filename)
  in
  match source_text with
  | None -> docs
  | Some (filename, text) ->
      StringMap.union
        (fun _ _ right -> Some right)
        docs
        (doc_index_of_source_text_cached ~filename text)

let doc_index_for_document ~(filename : string) (text : string) : doc_index =
  let source_units =
    try Source_units.default_source_units ~exclude_paths:[filename] () with
    | _ -> []
  in
  source_units @ [Source_units.source_text ~filename text]
  |> List.fold_left doc_index_add_source_unit StringMap.empty

let documentation_for_name (docs : doc_index) (name : _ Ast.name) : string option =
  let range = range_of_name name in
  let filename = ann_filename (snd name) in
  doc_index_find ~filename ~name:(name_text name) ~line:range.start_pos.line docs

let doc_info_block = function
  | None -> ""
  | Some doc when String.trim doc = "" -> ""
  | Some doc -> "\n\n" ^ doc
