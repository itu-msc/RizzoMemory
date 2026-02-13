type position = {
  line: int;
  character: int;
}

type range = {
  start_pos: position;
  end_pos: position;
}

type diagnostic_severity =
  | Error
  | Warning
  | Information

type diagnostic = {
  range: range;
  severity: diagnostic_severity;
  message: string;
  source: string;
}

type symbol_kind =
  | Function
  | Variable

type document_symbol = {
  name: string;
  kind: symbol_kind;
  range: range;
  selection_range: range;
}

type definition = {
  name: string;
  range: range;
}

type hover_info = {
  range: range;
  contents: string;
}

type analysis_result = {
  diagnostics: diagnostic list;
}

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

let parse_with_filename ~(filename : string) (text : string) : (Ast.program, diagnostic) result =
  let lexbuf = Lexing.from_string text in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  Ast.clear_locations ();
  try
    Ok (Parser.main Lexer.read lexbuf)
  with
  | Lexer.Error (loc, msg) ->
      Error { range = range_of_location loc; severity = Error; message = msg; source = "rizzoc" }
  | exn ->
      let loc = Location.mk lexbuf.Lexing.lex_start_p lexbuf.Lexing.lex_curr_p in
      let msg = Printf.sprintf "Parse error: %s" (Printexc.to_string exn) in
      Error { range = range_of_location loc; severity = Error; message = msg; source = "rizzoc" }

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

let lines_of_text (text : string) : string array =
  text |> String.split_on_char '\n' |> Array.of_list

let safe_line (lines : string array) (line_no : int) : string option =
  if line_no < 0 || line_no >= Array.length lines then None else Some lines.(line_no)

let find_identifier_in_line ~(line : string) ~(name : string) ~(from_col : int) : (int * int) option =
  let line_len = String.length line in
  let name_len = String.length name in
  let rec go col =
    if col + name_len > line_len then
      None
    else if String.sub line col name_len = name then
      let left_ok = col = 0 || not (is_ident_char line.[col - 1]) in
      let right_index = col + name_len in
      let right_ok = right_index >= line_len || not (is_ident_char line.[right_index]) in
      if left_ok && right_ok then Some (col, right_index) else go (col + 1)
    else
      go (col + 1)
  in
  go (max 0 from_col)

let top_level_declarations ~(text : string) (program : Ast.program) : (string * symbol_kind * range) list =
  let lines = lines_of_text text in
  let declaration_of_top (top : Ast.top_expr) =
    match top with
    | Ast.TLet (name, rhs) ->
        let kind =
          match rhs with
          | Ast.EFun _ -> Function
          | _ -> Variable
        in
        let fallback_range =
          match Ast.location_of_top_expr top with
          | Some loc -> range_of_location loc
          | None -> { start_pos = { line = 0; character = 0 }; end_pos = { line = 0; character = max 1 (String.length name) } }
        in
        let selection_range =
          match Ast.location_of_top_expr top with
          | Some loc ->
              let top_range = range_of_location loc in
              let line_no = top_range.start_pos.line in
              (match safe_line lines line_no with
               | Some line_text ->
                   (match find_identifier_in_line ~line:line_text ~name ~from_col:top_range.start_pos.character with
                    | Some (s_col, e_col) ->
                        { start_pos = { line = line_no; character = s_col }; end_pos = { line = line_no; character = e_col } }
                    | None -> fallback_range)
               | None -> fallback_range)
          | None -> fallback_range
        in
        Some (name, kind, selection_range)
  in
  List.filter_map declaration_of_top program

let rec iter_expr (f : Ast.expr -> unit) (expr : Ast.expr) : unit =
  f expr;
  match expr with
  | Ast.EConst _ | Ast.EVar _ -> ()
  | Ast.ELet (_, e1, e2) ->
      iter_expr f e1;
      iter_expr f e2
  | Ast.EFun (_, body) -> iter_expr f body
  | Ast.EApp (fn, args) ->
      iter_expr f fn;
      List.iter (iter_expr f) args
  | Ast.EUnary (_, e) -> iter_expr f e
  | Ast.EBinary (_, e1, e2) ->
      iter_expr f e1;
      iter_expr f e2
  | Ast.ETuple (e1, e2) ->
      iter_expr f e1;
      iter_expr f e2
  | Ast.ECase (scrutinee, branches) ->
      iter_expr f scrutinee;
      List.iter (fun (_, branch) -> iter_expr f branch) branches
  | Ast.EIfe (c, t, e) ->
      iter_expr f c;
      iter_expr f t;
      iter_expr f e

let collect_expr_ranges (program : Ast.program) : (Ast.expr * range) list =
  let acc = ref [] in
  let push expr =
    match Ast.location_of_expr expr with
    | Some loc -> acc := (expr, range_of_location loc) :: !acc
    | None -> ()
  in
  let visit_top = function
    | Ast.TLet (_, rhs) -> iter_expr push rhs
  in
  List.iter visit_top program;
  !acc

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

let first_opt = function
  | [] -> None
  | x :: _ -> Some x

let hover_text_for_expr (expr : Ast.expr) : string =
  match expr with
  | Ast.EConst _ -> "constant"
  | Ast.EVar name -> "variable " ^ name
  | Ast.ELet (name, _, _) -> "let-binding " ^ name
  | Ast.EFun (params, _) -> "function(" ^ String.concat ", " params ^ ")"
  | Ast.EApp _ -> "function application"
  | Ast.EUnary _ -> "unary expression"
  | Ast.EBinary _ -> "binary expression"
  | Ast.ETuple _ -> "tuple expression"
  | Ast.ECase _ -> "match expression"
  | Ast.EIfe _ -> "if expression"

let analyze_document ~(uri : string) ~(filename : string option) ~(text : string) : analysis_result =
  let filename =
    match filename with
    | Some path when String.length path > 0 -> path
    | _ -> if String.length uri = 0 then "<memory>.rizz" else uri
  in
  match parse_with_filename ~filename text with
  | Ok _ -> { diagnostics = [] }
  | Error diagnostic -> { diagnostics = [diagnostic] }

let document_symbols ~(uri : string) ~(filename : string option) ~(text : string) : document_symbol list =
  let filename =
    match filename with
    | Some path when String.length path > 0 -> path
    | _ -> if String.length uri = 0 then "<memory>.rizz" else uri
  in
  match parse_with_filename ~filename text with
  | Error _ -> []
  | Ok program ->
      top_level_declarations ~text program
      |> List.map (fun (name, kind, range) ->
            { name; kind; range; selection_range = range })

let identifier_at_position ~(text : string) (pos : position) : string option =
  let lines = String.split_on_char '\n' text in
  if pos.line < 0 || pos.line >= List.length lines then
    None
  else
    let line = List.nth lines pos.line in
    let len = String.length line in
    if len = 0 then
      None
    else
      let char_index =
        if pos.character < 0 then 0
        else if pos.character >= len then len - 1
        else pos.character
      in
      if not (is_ident_char line.[char_index]) then
        None
      else
        let rec left i =
          if i > 0 && is_ident_char line.[i - 1] then left (i - 1) else i
        in
        let rec right i =
          if i < len && is_ident_char line.[i] then right (i + 1) else i
        in
        let start_i = left char_index in
        let end_i = right char_index in
        Some (String.sub line start_i (end_i - start_i))

let definition_at_position ~(uri : string) ~(filename : string option) ~(text : string) ~(position : position) : definition option =
  let symbols : document_symbol list = document_symbols ~uri ~filename ~text in
  match identifier_at_position ~text position with
  | None -> None
  | Some target ->
      symbols
      |> List.find_opt (fun (sym : document_symbol) -> sym.name = target)
      |> Option.map (fun (sym : document_symbol) -> { name = sym.name; range = sym.selection_range })

let hover_at_position ~(uri : string) ~(filename : string option) ~(text : string) ~(position : position) : hover_info option =
  let filename =
    match filename with
    | Some path when String.length path > 0 -> path
    | _ -> if String.length uri = 0 then "<memory>.rizz" else uri
  in
  match parse_with_filename ~filename text with
  | Error _ -> None
  | Ok program ->
      collect_expr_ranges program
      |> List.filter (fun (_, range) -> range_contains_position range position)
      |> List.sort (fun (_, r1) (_, r2) -> compare (range_span_score r1) (range_span_score r2))
      |> first_opt
      |> Option.map (fun (expr, range) -> { range; contents = hover_text_for_expr expr })
