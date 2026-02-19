(** Source code location tracking and error reporting *)

type t = {
  start_pos: Lexing.position;
  end_pos: Lexing.position;
}

type 'a located = {
  value: 'a;
  loc: t;
}

let mk start_pos end_pos = { start_pos; end_pos }

(** String representation of a location *)
let to_string loc =
  let open Lexing in
  let start = loc.start_pos in
  let end_ = loc.end_pos in
  if start.pos_fname = "" then
    "unknown location"
  else if start.pos_lnum = end_.pos_lnum then
    Printf.sprintf "%s:%d:%d-%d"
      start.pos_fname
      start.pos_lnum
      (start.pos_cnum - start.pos_bol + 1)
      (end_.pos_cnum - end_.pos_bol + 1)
  else
    Printf.sprintf "%s:%d:%d-%d:%d"
      start.pos_fname
      start.pos_lnum
      (start.pos_cnum - start.pos_bol + 1)
      end_.pos_lnum
      (end_.pos_cnum - end_.pos_bol + 1)

(** Read a specific line from a file *)
let read_line filename line_num =
  try
    let ic = open_in filename in
    let rec find_line n =
      if n = line_num then
        let line = input_line ic in
        close_in ic;
        Some line
      else
        (ignore (input_line ic); find_line (n + 1))
    in
    find_line 1
  with _ -> None

(** Show error with source context (Bun/Rust style) *)
let show_error_context loc msg =
  let open Lexing in
  let line = loc.start_pos.pos_lnum in
  
  let col_start = loc.start_pos.pos_cnum - loc.start_pos.pos_bol in
  let col_end = loc.end_pos.pos_cnum - loc.end_pos.pos_bol in
  let filename = loc.start_pos.pos_fname in
  
  Printf.eprintf "\nError: %s\n" msg;
  Printf.eprintf "  --> %s\n" (to_string loc);
  
  (* Try to show the actual source line *)
  match read_line filename line with
  | Some source_line ->
      let line_num_str = string_of_int line in
      let padding = String.make (String.length line_num_str) ' ' in
      
      Printf.eprintf " %s |\n" padding;
      Printf.eprintf " %s | %s\n" line_num_str source_line;
      
      (* Show the error indicator *)
      let underline_len = max 1 (col_end - col_start) in
      Printf.eprintf " %s | %s%s\n"
        padding
        (String.make col_start ' ')
        (String.make underline_len '^');
      Printf.eprintf " %s |\n" padding
  | None ->
      (* Can't read file, just show the message *)
      ()

(** Report error and exit *)
let report_error loc msg =
  show_error_context loc msg;
  exit 1

(** Report warning (don't exit) *)
let report_warning loc msg =
  Printf.eprintf "\nWarning at %s:\n%s\n\n" (to_string loc) msg
