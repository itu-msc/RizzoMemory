open Types
open Document
open Parse
open Symbols

let analyze_document ~(uri : string) ~(filename : string option) ~(text : string) : analysis_result =
  match parse_with_filename ~filename:(file_name ~uri ~filename) text with
  | Ok { diagnostics; _ } -> { diagnostics }
  | Error diagnostic -> { diagnostics = [diagnostic] }

let document_symbols ~(uri : string) ~(filename : string option) ~(text : string) : document_symbol list =
  let active_filename = file_name ~uri ~filename in
  match parse_with_filename ~filename:active_filename text with
  | Error _ -> []
  | Ok { typed_program = program; _ } ->
      top_level_declarations ~filename:(Some active_filename) program
      |> List.map (fun (name, kind, _decl_filename, top_range, selection_range) ->
             { name; kind; range = top_range; selection_range })
