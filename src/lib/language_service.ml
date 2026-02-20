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

module StringMap = Map.Make(String)

type scoped_symbol = {
  kind: semantic_token_kind;
  range: range;
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

let range_of_ann (ann : _ Ast.ann) : range =
  range_of_location (Ast.get_location ann)

let range_of_name ((_, ann) : _ Ast.name) : range =
  range_of_ann ann

let name_text ((name, _) : _ Ast.name) : string = name

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

let parse_with_filename ~(filename : string) (text : string) : (Ast.parsed Ast.program, diagnostic) result =
  let lexbuf = Lexing.from_string text in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
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

let file_name ~(uri : string) ~(filename : string option) : string =
  match filename with
  | Some path when String.length path > 0 -> path
  | _ -> if String.length uri = 0 then "<memory>.rizz" else uri

let top_level_name_range ~(lines : string array) (top_range : range) ~(name : string) : range =
  let fallback =
    {
      start_pos = top_range.start_pos;
      end_pos = { line = top_range.start_pos.line; character = top_range.start_pos.character + max 1 (String.length name) };
    }
  in
  let line_no = top_range.start_pos.line in
  match safe_line lines line_no with
  | None -> fallback
  | Some line_text ->
      (match find_identifier_in_line ~line:line_text ~name ~from_col:top_range.start_pos.character with
       | Some (s_col, e_col) ->
           {
             start_pos = { line = line_no; character = s_col };
             end_pos = { line = line_no; character = e_col };
           }
       | None -> fallback)

let top_level_declarations ~(text : string) (program : Ast.parsed Ast.program) : (string * symbol_kind * range * range) list =
  let lines = lines_of_text text in
  let declaration_of_top (top : Ast.parsed Ast.top_expr) =
    match top with
    | Ast.TLet (name, rhs, ann) ->
        let kind =
          match rhs with
          | Ast.EFun _ -> Function
          | _ -> Variable
        in
        let top_range = range_of_ann ann in
        let selection_range = top_level_name_range ~lines top_range ~name in
        Some (name, kind, top_range, selection_range)
  in
  List.filter_map declaration_of_top program

let rec iter_expr (f : Ast.parsed Ast.expr -> unit) (expr : Ast.parsed Ast.expr) : unit =
  f expr;
  match expr with
  | Ast.EConst _ | Ast.EVar _ -> ()
  | Ast.ELet (_, e1, e2, _) ->
      iter_expr f e1;
      iter_expr f e2
  | Ast.EFun (_, body, _) -> iter_expr f body
  | Ast.EApp (fn, args, _) ->
      iter_expr f fn;
      List.iter (iter_expr f) args
  | Ast.EUnary (_, e, _) -> iter_expr f e
  | Ast.EBinary (_, e1, e2, _) ->
      iter_expr f e1;
      iter_expr f e2
  | Ast.ETuple (e1, e2, _) ->
      iter_expr f e1;
      iter_expr f e2
  | Ast.ECase (scrutinee, branches, _) ->
      iter_expr f scrutinee;
      List.iter (fun (_, branch, _) -> iter_expr f branch) branches
  | Ast.EIfe (c, t, e, _) ->
      iter_expr f c;
      iter_expr f t;
      iter_expr f e

let collect_expr_ranges (program : Ast.parsed Ast.program) : (Ast.parsed Ast.expr * range) list =
  let acc = ref [] in
  let push expr =
    let expr_range = range_of_ann (Ast.expr_get_ann expr) in
    acc := (expr, expr_range) :: !acc
  in
  let visit_top = function
    | Ast.TLet (_, rhs, _) -> iter_expr push rhs
  in
  List.iter visit_top program;
  !acc

let hover_text_for_expr (expr : Ast.parsed Ast.expr) : string =
  match expr with
  | Ast.EConst (c, _) -> "constant " ^ (
    match c with
    | Ast.CUnit -> "()"
    | Ast.CNever -> "never"
    | Ast.CInt n -> string_of_int n
    | Ast.CBool b -> if b then "true" else "false"
    | Ast.CString s -> s
  )
  | Ast.EVar (name, _) -> "variable " ^ name
  | Ast.ELet ((name, _), _, _, _) -> "let-binding " ^ name
  | Ast.EFun (params, _, _) -> "function(" ^ String.concat ", " (List.map fst params) ^ ")"
  | Ast.EApp _ -> "function application"
  | Ast.EUnary _ -> "unary expression"
  | Ast.EBinary _ -> "binary expression"
  | Ast.ETuple _ -> "tuple expression"
  | Ast.ECase _ -> "match expression"
  | Ast.EIfe _ -> "if expression"

let rec pattern_bound_decls (pat : Ast.parsed Ast.pattern) : (string * range) list =
  match pat with
  | Ast.PWildcard | Ast.PConst _ -> []
  | Ast.PVar ((name, ann)) -> [ (name, range_of_ann ann) ]
  | Ast.PSigCons (p1, p2, _) ->
      pattern_bound_decls p1 @ [ (fst p2, range_of_ann (snd p2)) ]
  | Ast.PTuple (p1, p2, _) | Ast.PBoth (p1, p2, _) ->
      pattern_bound_decls p1 @ pattern_bound_decls p2
  | Ast.PLeft (p, _) | Ast.PRight (p, _) -> pattern_bound_decls p

let keyword_range_from_expr ~(name : string) (expr : Ast.parsed Ast.expr) : range option =
  let expr_range = range_of_ann (Ast.expr_get_ann expr) in
  if expr_range.start_pos.line <> expr_range.end_pos.line then
    None
  else
    Some
      {
        start_pos = expr_range.start_pos;
        end_pos =
          {
            line = expr_range.start_pos.line;
            character = expr_range.start_pos.character + String.length name;
          };
      }

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

let analyze_document ~(uri : string) ~(filename : string option) ~(text : string) : analysis_result =
  match parse_with_filename ~filename:(file_name ~uri ~filename) text with
  | Ok _ -> { diagnostics = [] }
  | Error diagnostic -> { diagnostics = [diagnostic] }

let document_symbols ~(uri : string) ~(filename : string option) ~(text : string) : document_symbol list =
  match parse_with_filename ~filename:(file_name ~uri ~filename) text with
  | Error _ -> []
  | Ok program ->
      top_level_declarations ~text program
      |> List.map (fun (name, kind, top_range, selection_range) ->
             { name; kind; range = top_range; selection_range })

let definition_at_position ~(uri : string) ~(filename : string option) ~(text : string) ~(position : position) : definition option =
  match parse_with_filename ~filename:(file_name ~uri ~filename) text with
  | Error _ -> None
  | Ok program ->
  let lines = lines_of_text text in
      let top_decls = top_level_declarations ~text program in
      let top_env =
        top_decls
        |> List.fold_left
             (fun env (name, kind, _top_range, selection_range) ->
               StringMap.add name { kind = semantic_kind_of_symbol_kind kind; range = selection_range } env)
             StringMap.empty
      in
      let rec find_in_expr (env : scoped_symbol StringMap.t) (expr : Ast.parsed Ast.expr) : definition option =
        match expr with
        | Ast.EConst _ -> None
        | Ast.EVar var_name ->
            let var_range = range_of_name var_name in
            if range_contains_position var_range position then
              let name = name_text var_name in
              (match StringMap.find_opt name env with
               | Some symbol -> Some { name; range = symbol.range }
               | None -> Some { name; range = var_range })
            else
              None
        | Ast.ELet (bound_name, e1, e2, _) ->
            let binding_range = range_of_name bound_name in
            if range_contains_position binding_range position then
              Some { name = name_text bound_name; range = binding_range }
            else
              (match find_in_expr env e1 with
               | Some _ as found -> found
               | None ->
                   let env' = StringMap.add (name_text bound_name) { kind = SemanticVariable; range = binding_range } env in
                   find_in_expr env' e2)
        | Ast.EFun (params, body, _) ->
            let declared_here =
              params
              |> List.find_map (fun param ->
                     let param_range = range_of_name param in
                     if range_contains_position param_range position then
                       Some { name = name_text param; range = param_range }
                     else
                       None)
            in
            (match declared_here with
             | Some _ as found -> found
             | None ->
                 let env' =
                   List.fold_left
                     (fun acc param ->
                       StringMap.add (name_text param) { kind = SemanticVariable; range = range_of_name param } acc)
                     env
                     params
                 in
                 find_in_expr env' body)
        | Ast.EApp (fn, args, _) ->
            (match find_in_expr env fn with
             | Some _ as found -> found
             | None -> List.find_map (find_in_expr env) args)
        | Ast.EUnary (_, e, _) -> find_in_expr env e
        | Ast.EBinary (_, e1, e2, _) ->
            (match find_in_expr env e1 with
             | Some _ as found -> found
             | None -> find_in_expr env e2)
        | Ast.ETuple (e1, e2, _) ->
            (match find_in_expr env e1 with
             | Some _ as found -> found
             | None -> find_in_expr env e2)
        | Ast.ECase (scrutinee, branches, _) ->
            (match find_in_expr env scrutinee with
             | Some _ as found -> found
             | None ->
                 List.find_map
                   (fun (pattern, branch_expr, _) ->
                     let bound = pattern_bound_decls pattern in
                     let bound_here =
                       bound
                       |> List.find_map (fun (name, binding_range) ->
                              if range_contains_position binding_range position then
                                Some { name; range = binding_range }
                              else
                                None)
                     in
                     match bound_here with
                     | Some _ as found -> found
                     | None ->
                         let env' =
                           List.fold_left
                             (fun acc (name, binding_range) ->
                               StringMap.add name { kind = SemanticVariable; range = binding_range } acc)
                             env
                             bound
                         in
                         find_in_expr env' branch_expr)
                   branches)
        | Ast.EIfe (c, t, e, _) ->
            (match find_in_expr env c with
             | Some _ as found -> found
             | None ->
                 match find_in_expr env t with
                 | Some _ as found -> found
                 | None -> find_in_expr env e)
      in
      let rec find_in_tops tops =
        match tops with
        | [] -> None
        | Ast.TLet (name, rhs, ann) :: rest ->
            let top_range = range_of_ann ann in
            let name_range = top_level_name_range ~lines top_range ~name in
            if range_contains_position name_range position then
              Some { name; range = name_range }
            else
              let env' = StringMap.add name { kind = SemanticVariable; range = name_range } top_env in
              match find_in_expr env' rhs with
              | Some _ as found -> found
              | None -> find_in_tops rest
      in
      find_in_tops program

let hover_at_position ~(uri : string) ~(filename : string option) ~(text : string) ~(position : position) : hover_info option =
  match parse_with_filename ~filename:(file_name ~uri ~filename) text with
  | Error _ -> None
  | Ok program ->
      collect_expr_ranges program
      |> List.filter (fun (_, range) -> range_contains_position range position)
      |> List.sort (fun (_, r1) (_, r2) -> compare (range_span_score r1) (range_span_score r2))
      |> first_opt
      |> Option.map (fun (expr, range) -> { range; contents = hover_text_for_expr expr })

let semantic_tokens ~(uri : string) ~(filename : string option) ~(text : string) : semantic_token list =
  match parse_with_filename ~filename:(file_name ~uri ~filename) text with
  | Error _ -> []
  | Ok program ->
      let lines = lines_of_text text in
      let top_decls = top_level_declarations ~text program in
      let tokens : semantic_token list ref = ref [] in
      let push_token ~(kind : semantic_token_kind) ~(range : range) =
        tokens := { range; kind; declaration = false } :: !tokens
      in
      let top_env =
        top_decls
        |> List.fold_left
             (fun env (name, kind, _top_range, selection_range) ->
               let semantic_kind = semantic_kind_of_symbol_kind kind in
               push_token ~kind:semantic_kind ~range:selection_range;
               StringMap.add name { kind = semantic_kind; range = selection_range } env)
             StringMap.empty
      in
      let rec walk_expr (env : scoped_symbol StringMap.t) (expr : Ast.parsed Ast.expr) : unit =
        (match expr with
         | Ast.EConst _ -> ()
         | Ast.EVar var_name ->
             let name = name_text var_name in
             let kind =
               match StringMap.find_opt name env with
               | Some symbol -> symbol.kind
               | None -> SemanticVariable
             in
             push_token ~kind ~range:(range_of_name var_name)
         | Ast.ELet (bound_name, e1, e2, _) ->
             walk_expr env e1;
             let binding_range = range_of_name bound_name in
             push_token ~kind:SemanticVariable ~range:binding_range;
             let env' = StringMap.add (name_text bound_name) { kind = SemanticVariable; range = binding_range } env in
             walk_expr env' e2
         | Ast.EFun (params, body, _) ->
             let env' =
               List.fold_left
                 (fun acc param ->
                   let param_range = range_of_name param in
                   push_token ~kind:SemanticVariable ~range:param_range;
                   StringMap.add (name_text param) { kind = SemanticVariable; range = param_range } acc)
                 env
                 params
             in
             walk_expr env' body
         | Ast.EApp (fn, args, _) ->
             walk_expr env fn;
             List.iter (walk_expr env) args
         | Ast.EUnary (Ast.UWait, e, _) ->
             (match keyword_range_from_expr ~name:"wait" expr with
              | Some keyword_range -> push_token ~kind:SemanticFunction ~range:keyword_range
              | None -> ());
             walk_expr env e
         | Ast.EUnary (Ast.UWatch, e, _) ->
             (match keyword_range_from_expr ~name:"watch" expr with
              | Some keyword_range -> push_token ~kind:SemanticFunction ~range:keyword_range
              | None -> ());
             walk_expr env e
         | Ast.EUnary (Ast.UDelay, e, _) ->
             (match keyword_range_from_expr ~name:"delay" expr with
              | Some keyword_range -> push_token ~kind:SemanticFunction ~range:keyword_range
              | None -> ());
             walk_expr env e
         | Ast.EUnary (_, e, _) -> walk_expr env e
         | Ast.EBinary (Ast.BSync, e1, e2, _) ->
             (match keyword_range_from_expr ~name:"sync" expr with
              | Some keyword_range -> push_token ~kind:SemanticFunction ~range:keyword_range
              | None -> ());
             walk_expr env e1;
             walk_expr env e2
         | Ast.EBinary (_, e1, e2, _) ->
             walk_expr env e1;
             walk_expr env e2
         | Ast.ETuple (e1, e2, _) ->
             walk_expr env e1;
             walk_expr env e2
         | Ast.ECase (scrutinee, branches, _) ->
             walk_expr env scrutinee;
             List.iter
               (fun (pat, branch_expr, _) ->
                 let bound = pattern_bound_decls pat in
                 let env' =
                   List.fold_left
                     (fun acc (name, binding_range) ->
                       push_token ~kind:SemanticVariable ~range:binding_range;
                       StringMap.add name { kind = SemanticVariable; range = binding_range } acc)
                     env
                     bound
                 in
                 walk_expr env' branch_expr)
               branches
         | Ast.EIfe (c, t, e, _) ->
             walk_expr env c;
             walk_expr env t;
             walk_expr env e)
      in
      List.iter
        (function
          | Ast.TLet (name, rhs, ann) ->
              let top_range = range_of_ann ann in
              let name_range = top_level_name_range ~lines top_range ~name in
              let kind =
                match rhs with
                | Ast.EFun _ -> SemanticFunction
                | _ -> SemanticVariable
              in
              let env' = StringMap.add name { kind; range = name_range } top_env in
              walk_expr env' rhs)
        program;
      !tokens
      |> List.filter (fun (token : semantic_token) -> valid_single_line_range token.range)
      |> List.sort (fun (left : semantic_token) (right : semantic_token) -> compare_range_position left.range right.range)