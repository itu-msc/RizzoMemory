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

(** Read a range of lines from a file (inclusive) *)
let read_lines filename start_line end_line =
  try
    let ic = open_in filename in
    let rec collect n acc =
      if n > end_line then (
        close_in ic;
        Some (List.rev acc)
      ) else
        let line = input_line ic in
        if n >= start_line then
          collect (n + 1) (line :: acc)
        else
          collect (n + 1) acc
    in
    collect 1 []
  with _ -> None

(** Show error with source context (Bun/Rust style) *)
let show_error_context loc msg =
  let open Lexing in
  let start_line = loc.start_pos.pos_lnum in
  let end_line = max start_line loc.end_pos.pos_lnum in
  let col_start = max 0 (loc.start_pos.pos_cnum - loc.start_pos.pos_bol) in
  let col_end = max 0 (loc.end_pos.pos_cnum - loc.end_pos.pos_bol) in
  let filename = loc.start_pos.pos_fname in
  
  Printf.eprintf "\nError: %s\n" msg;
  Printf.eprintf "  --> %s\n" (to_string loc);
  
  (* Try to show the actual source span *)
  match read_lines filename start_line end_line with
  | Some source_lines ->
      if source_lines <> [] then (
        let line_num_width = String.length (string_of_int end_line) in
        let padding = String.make line_num_width ' ' in

        Printf.eprintf " %s |\n" padding;

        List.iteri
          (fun i source_line ->
            let current_line = start_line + i in
            let source_len = String.length source_line in

            let indicator_start, indicator_len =
              if start_line = end_line then
                let start_col = min col_start source_len in
                let end_col = max start_col (min col_end source_len) in
                (start_col, max 1 (end_col - start_col))
              else if current_line = start_line then
                let start_col = min col_start source_len in
                (start_col, max 1 (source_len - start_col))
              else if current_line = end_line then
                let end_col = min col_end source_len in
                (0, max 1 end_col)
              else
                (0, max 1 source_len)
            in

            Printf.eprintf " %*d | %s\n" line_num_width current_line source_line;
            Printf.eprintf " %s | %s%s\n"
              padding
              (String.make indicator_start ' ')
              (String.make indicator_len '^'))
          source_lines;

        Printf.eprintf " %s |\n" padding)
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
