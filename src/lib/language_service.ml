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

type semantic_token_kind =
  | SemanticFunction
  | SemanticVariable

type semantic_token = {
  range: range;
  kind: semantic_token_kind;
  declaration: bool;
}

let semantic_token_range (token : semantic_token) : range = token.range
let semantic_token_kind (token : semantic_token) : semantic_token_kind = token.kind
let semantic_token_is_declaration (token : semantic_token) : bool = token.declaration

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

let is_menhir_parse_error (exn : exn) : bool =
  match Printexc.to_string exn with
  | "Rizzoc__Parser.MenhirBasics.Error"
  | "Parser.MenhirBasics.Error"
  | "MenhirBasics.Error" -> true
  | _ -> false

let starts_with ~(prefix : string) (s : string) : bool =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix

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

let line_at (lines : string array) (line_no : int) : string option =
  if line_no < 0 || line_no >= Array.length lines then None else Some lines.(line_no)

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
    | Some ('[' | ']') ->
        "Unexpected bracket. '[' and ']' are not part of Rizz syntax; use parentheses '()' for grouping/tuples."
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

let parse_with_filename ~(filename : string) (text : string) : (Ast.program, diagnostic) result =
  let lexbuf = Lexing.from_string text in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  Ast.clear_locations ();
  try
    Ok (Parser.main Lexer.read lexbuf)
  with
  | Lexer.Error (loc, msg) ->
      Error
        {
          range = range_of_location loc;
          severity = Error;
          message = friendly_lexer_message msg;
          source = "rizzoc";
        }
  | Parser.Error ->
      let loc = Location.mk lexbuf.Lexing.lex_start_p lexbuf.Lexing.lex_curr_p in
      let range_override, msg = parse_error_details ~text loc in
      let range = match range_override with Some r -> r | None -> range_of_location loc in
      Error { range; severity = Error; message = msg; source = "rizzoc" }
  | exn when is_menhir_parse_error exn ->
      let loc = Location.mk lexbuf.Lexing.lex_start_p lexbuf.Lexing.lex_curr_p in
      let range_override, msg = parse_error_details ~text loc in
      let range = match range_override with Some r -> r | None -> range_of_location loc in
      Error { range; severity = Error; message = msg; source = "rizzoc" }
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

let semantic_kind_of_symbol_kind = function
  | Function -> SemanticFunction
  | Variable -> SemanticVariable

let compare_range_position (left : range) (right : range) : int =
  let by_start_line = compare left.start_pos.line right.start_pos.line in
  if by_start_line <> 0 then
    by_start_line
  else
    let by_start_char = compare left.start_pos.character right.start_pos.character in
    if by_start_char <> 0 then
      by_start_char
    else
      compare left.end_pos.character right.end_pos.character

let valid_single_line_range (token_range : range) : bool =
  token_range.start_pos.line = token_range.end_pos.line
  && token_range.end_pos.character > token_range.start_pos.character

let semantic_tokens ~(uri : string) ~(filename : string option) ~(text : string) : semantic_token list =
  let filename =
    match filename with
    | Some path when String.length path > 0 -> path
    | _ -> if String.length uri = 0 then "<memory>.rizz" else uri
  in
  match parse_with_filename ~filename text with
  | Error _ -> []
  | Ok program ->
      let lines = lines_of_text text in
      let declarations = top_level_declarations ~text program in
      let declaration_tokens =
        declarations
        |> List.map (fun (_name, symbol_kind, token_range) ->
               {
                 range = token_range;
                 kind = semantic_kind_of_symbol_kind symbol_kind;
                 declaration = false;
               })
      in
      let declaration_kind_by_name =
        declarations
        |> List.map (fun (name, symbol_kind, _range) -> (name, semantic_kind_of_symbol_kind symbol_kind))
      in
      let kind_for_name name =
        match List.find_opt (fun (candidate, _) -> candidate = name) declaration_kind_by_name with
        | Some (_, kind) -> kind
        | None -> SemanticVariable
      in
      let references = ref [] in
      let local_declarations = ref [] in
      let parameter_declarations = ref [] in
      let builtins = ref [] in
      let identifier_spans_in_line (line : string) : (string * int * int) list =
        let len = String.length line in
        let rec collect index acc =
          if index >= len then
            List.rev acc
          else if is_ident_char line.[index] then
            let rec scan_end j =
              if j < len && is_ident_char line.[j] then scan_end (j + 1) else j
            in
            let end_index = scan_end index in
            let name = String.sub line index (end_index - index) in
            collect end_index ((name, index, end_index) :: acc)
          else
            collect (index + 1) acc
        in
        collect 0 []
      in
      let range_for_local_let_binding ~(name : string) ~(binding_expr_range : range) : range =
        let line_no = binding_expr_range.start_pos.line in
        match safe_line lines line_no with
        | Some line_text ->
            (match find_identifier_in_line ~line:line_text ~name ~from_col:binding_expr_range.start_pos.character with
             | Some (start_col, end_col) ->
                 {
                   start_pos = { line = line_no; character = start_col };
                   end_pos = { line = line_no; character = end_col };
                 }
             | None ->
                 {
                   start_pos = { line = line_no; character = binding_expr_range.start_pos.character };
                   end_pos = { line = line_no; character = binding_expr_range.start_pos.character + String.length name };
                 })
        | None ->
            {
              start_pos = binding_expr_range.start_pos;
              end_pos = { line = binding_expr_range.start_pos.line; character = binding_expr_range.start_pos.character + String.length name };
            }
      in
      let range_for_keyword ~(name : string) ~(expr_range : range) : range option =
        let line_no = expr_range.start_pos.line in
        match safe_line lines line_no with
        | Some line_text ->
            (match find_identifier_in_line ~line:line_text ~name ~from_col:expr_range.start_pos.character with
             | Some (start_col, end_col) ->
                 Some
                   {
                     start_pos = { line = line_no; character = start_col };
                     end_pos = { line = line_no; character = end_col };
                   }
             | None -> None)
        | None -> None
      in
      let push_builtin_keyword ~(name : string) (expr : Ast.expr) =
        match Ast.location_of_expr expr with
        | Some loc ->
            (match range_for_keyword ~name ~expr_range:(range_of_location loc) with
             | Some keyword_range ->
                 builtins := {
                   range = keyword_range;
                   kind = SemanticFunction;
                   declaration = false;
                 } :: !builtins
             | None -> ())
        | None -> ()
      in
      let add_top_level_function_params (top : Ast.top_expr) : unit =
        match top with
        | Ast.TLet (function_name, Ast.EFun (params, _)) when params <> [] ->
            (match Ast.location_of_top_expr top with
             | Some loc ->
                 let top_range = range_of_location loc in
                 let line_no = top_range.start_pos.line in
                 (match safe_line lines line_no with
                  | Some line_text ->
                      let header_end =
                        match String.index_from_opt line_text top_range.start_pos.character '=' with
                        | Some idx -> idx
                        | None -> String.length line_text
                      in
                      let spans =
                        identifier_spans_in_line line_text
                        |> List.filter (fun (_, start_col, _) ->
                               start_col >= top_range.start_pos.character
                               && start_col < header_end)
                      in
                      let spans_after_function_name =
                        let rec drop_until_function_name = function
                          | [] -> []
                          | (name, _, _) :: rest when name = function_name -> rest
                          | _ :: rest -> drop_until_function_name rest
                        in
                        drop_until_function_name spans
                      in
                      let rec add_params remaining_params remaining_spans =
                        match remaining_params with
                        | [] -> ()
                        | param_name :: rest_params ->
                            let rec seek = function
                              | [] -> ()
                              | (name, start_col, end_col) :: rest_spans ->
                                  if name = param_name then (
                                    parameter_declarations :=
                                      {
                                        range =
                                          {
                                            start_pos = { line = line_no; character = start_col };
                                            end_pos = { line = line_no; character = end_col };
                                          };
                                        kind = SemanticVariable;
                                        declaration = false;
                                      }
                                      :: !parameter_declarations;
                                    add_params rest_params rest_spans)
                                  else
                                    seek rest_spans
                            in
                            seek remaining_spans
                      in
                      add_params params spans_after_function_name
                  | None -> ())
             | None -> ())
        | _ -> ()
      in
      let push_reference expr =
        match expr with
        | Ast.EVar name ->
            (match Ast.location_of_expr expr with
             | Some loc ->
                 references := {
                   range = range_of_location loc;
                   kind = kind_for_name name;
                   declaration = false;
                 } :: !references
             | None -> ())
              | Ast.ELet (name, _, _) ->
                (match Ast.location_of_expr expr with
                 | Some loc ->
                   let binding_range = range_for_local_let_binding ~name ~binding_expr_range:(range_of_location loc) in
                   local_declarations := {
                     range = binding_range;
                     kind = SemanticVariable;
                     declaration = false;
                   } :: !local_declarations
                 | None -> ())
              | Ast.EUnary (Ast.UWait, _) ->
                push_builtin_keyword ~name:"wait" expr
              | Ast.EUnary (Ast.UWatch, _) ->
                push_builtin_keyword ~name:"watch" expr
              | Ast.EUnary (Ast.UDelay, _) ->
                push_builtin_keyword ~name:"delay" expr
              | Ast.EBinary (Ast.BSync, _, _) ->
                push_builtin_keyword ~name:"sync" expr
        | _ -> ()
      in
      let visit_top = function
        | Ast.TLet (_, rhs) as top ->
            add_top_level_function_params top;
            iter_expr push_reference rhs
      in
      List.iter visit_top program;
      (declaration_tokens @ !parameter_declarations @ !local_declarations @ !references @ !builtins)
      |> List.filter (fun token -> valid_single_line_range token.range)
      |> List.sort (fun left right -> compare_range_position left.range right.range)
