module Generated_lexer = Lexer
module Generated_parser = Parser

module Parser = struct
  include Generated_parser

  exception Error of Location.t * string

  let parse_with lexbuf =
    Effectful.reset_custom ();
    try Generated_parser.main Generated_lexer.read lexbuf with
    | Generated_lexer.Error _ as exn -> raise exn
    | exn ->
        let loc = Location.mk lexbuf.Lexing.lex_start_p lexbuf.Lexing.lex_curr_p in
        let msg = Printf.sprintf "Menhir parse error: %s" (Printexc.to_string exn) in
        raise (Error (loc, msg))

  let parse_string (text : string) =
    let lexbuf = Lexing.from_string text in
    parse_with lexbuf

  let parse_string_with_filename ~(filename : string) (text : string) =
    let lexbuf = Lexing.from_string text in
    lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
    parse_with lexbuf

  let parse_file (filename : string) =
    let ic = open_in filename in
    let lexbuf = Lexing.from_channel ic in
    lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
    let result = parse_with lexbuf in
    close_in ic;
    result
end

type source_unit =
  | Source_file of string
  | Source_text of { filename: string; text: string }

exception Validation_failed of (Location.t * string) list

let source_units_of_source_texts source_texts =
  source_texts
  |> List.map
    (fun ({ Stdlib_sources.filename; text } : Stdlib_sources.source_text) ->
      Source_text { filename; text })

let default_source_units ?executable_path ?stdlib_path ?(exclude_paths = []) () =
  source_units_of_source_texts
    (Stdlib_sources.default_preludes ?executable_path ?stdlib_path ~exclude_paths ())

let included_source_units include_paths =
  include_paths
  |> List.concat_map Stdlib_sources.source_texts_of_path
  |> source_units_of_source_texts

let source_file filename = Source_file filename
let source_text ~filename text = Source_text { filename; text }

let parse_source_unit = function
  | Source_file filename -> Parser.parse_file filename
  | Source_text { filename; text } -> Parser.parse_string_with_filename ~filename text

let parse_source_units source_units =
  List.concat_map parse_source_unit source_units

let parse_document_with_default_prelude ?executable_path ?stdlib_path ?(exclude_paths = []) ~(filename : string) (text : string) =
  parse_source_units (default_source_units ?executable_path ?stdlib_path ~exclude_paths () @ [source_text ~filename text])

let format_location_ref (loc : Location.t) =
  Printf.sprintf
    "%s:%d:%d"
    loc.Location.start_pos.Lexing.pos_fname
    loc.Location.start_pos.Lexing.pos_lnum
    (loc.Location.start_pos.Lexing.pos_cnum - loc.Location.start_pos.Lexing.pos_bol + 1)

let validate_program (program : _ Ast.program) =
  let seen = Hashtbl.create 32 in
  let errors = ref [] in
  List.iter
    (function
    | Ast.TopTypeDef _ -> ()
    | Ast.TopLet ((name, name_ann), _, _) ->
      let loc = Ast.get_location name_ann in
      match Hashtbl.find_opt seen name with
      | None -> Hashtbl.add seen name loc
      | Some first_loc ->
          let msg =
            Printf.sprintf
              "Duplicate top-level definition '%s' (previously defined at %s)"
              name
              (format_location_ref first_loc)
          in
          errors := (loc, msg) :: !errors)
    program;
  List.rev !errors

let report_validation_errors errors =
  List.iter (fun (loc, msg) -> Location.show_error_context loc msg) errors
