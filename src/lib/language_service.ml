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
  | SemanticType

type semantic_token = {
  range: range;
  kind: semantic_token_kind;
  declaration: bool;
}

type completion_item = {
  label: string;
  kind: int;
  detail: string option;
  documentation: string option;
}

type completion_list = {
  items: completion_item list;
  is_incomplete: bool;
}

type completion_source =
  | CompletionLocal
  | CompletionTopLevel
  | CompletionBuiltin
  | CompletionConstructor

type completion_symbol = {
  kind: semantic_token_kind;
  range: range;
  detail: string option;
  source: completion_source;
}

let semantic_token_range (token : semantic_token) : range = token.range
let semantic_token_kind (token : semantic_token) : semantic_token_kind = token.kind
let semantic_token_is_declaration (token : semantic_token) : bool = token.declaration

type analysis_result = {
  diagnostics: diagnostic list;
}

type parsed_typed_result = {
  typed_program: Ast.typed Ast.program;
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

let is_identifier_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
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

let diagnostic_of_type_error ((loc, msg) : Location.t * string) : diagnostic =
  {
    range = range_of_location loc;
    severity = Error;
    message = msg;
    source = "rizzoc";
  }

let parse_with_filename ~(filename : string) (text : string) : (parsed_typed_result, diagnostic) result =
  let lexbuf = Lexing.from_string text in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  try
    let parsed = Parser.main Lexer.read lexbuf in
    let typed_program, type_errors = Typecheck.typecheck parsed in
    let diagnostics = List.map diagnostic_of_type_error type_errors in
    Ok { typed_program; diagnostics }
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

let lines_of_text (text : string) : string array =
  text |> String.split_on_char '\n' |> Array.of_list

let file_name ~(uri : string) ~(filename : string option) : string =
  match filename with
  | Some path when String.length path > 0 -> path
  | _ -> if String.length uri = 0 then "<memory>.rizz" else uri

let rec expr_is_function : type s. s Ast.expr -> bool =
  function
  | Ast.EFun _ -> true
  | Ast.EAnno (expr, _, _) -> expr_is_function expr
  | _ -> false

let symbol_kind_of_expr : type s. s Ast.expr -> symbol_kind =
  fun expr -> if expr_is_function expr then Function else Variable

let top_level_declarations : type s.
    text:string -> s Ast.program -> (string * symbol_kind * range * range) list =
  fun ~text program ->
  let _ = text in
  let declaration_of_top : s Ast.top_expr -> (string * symbol_kind * range * range) option = fun top ->
    match top with
    | Ast.TopLet (top_name, rhs, ann) ->
        let kind = symbol_kind_of_expr rhs in
        let name = name_text top_name in
        let top_range = range_of_ann ann in
        let selection_range = range_of_name top_name in
        Some (name, kind, top_range, selection_range)
  in
  List.filter_map declaration_of_top program

    let rec iter_expr : type s. (s Ast.expr -> unit) -> s Ast.expr -> unit =
      fun f expr ->
      f expr;
      match expr with
      | Ast.EConst _ | Ast.EVar _ -> ()
      | Ast.ECtor (_, args, _) ->
        List.iter (iter_expr f) args
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
      | Ast.EAnno (e, _, _) -> iter_expr f e

    let collect_expr_ranges : type s. s Ast.program -> (s Ast.expr * range) list =
      fun program ->
        let acc = ref [] in
        let push expr =
          let expr_range = range_of_ann (Ast.expr_get_ann expr) in
          acc := (expr, expr_range) :: !acc
        in
        let visit_top = function
          | Ast.TopLet (_, rhs, _) -> iter_expr push rhs
        in
        List.iter visit_top program;
        !acc

let typ_of_ann_opt : type s. s Ast.ann -> Ast.typ option = function
  | Ast.Ann_typed (_, t) -> Some t
  | Ast.Ann_parsed _ | Ast.Ann_bound _ -> None

let string_of_typ (t : Ast.typ) : string =
  Format.asprintf "%a" Ast.pp_typ t

let detail_of_ann_opt : type s. s Ast.ann -> string option =
  fun ann -> typ_of_ann_opt ann |> Option.map string_of_typ

let detail_of_name : type s. s Ast.name -> string option =
  fun (_, ann) -> detail_of_ann_opt ann

let type_info_block_of_typ_opt (typ : Ast.typ option) : string =
  match typ with
  | None -> ""
  | Some t -> Format.asprintf "\n\nType:\n```rizz\n%a\n```" Ast.pp_typ t

let type_info_block_of_ann : type s. s Ast.ann -> string =
  fun ann -> type_info_block_of_typ_opt (typ_of_ann_opt ann)

let hover_text_for_named_symbol : type s. label:string -> s Ast.name -> string =
  fun ~label (name, ann) ->
    label ^ " " ^ name ^ type_info_block_of_ann ann

let type_info_block : type s. s Ast.expr -> string =
  fun expr ->
    type_info_block_of_typ_opt (typ_of_ann_opt (Ast.expr_get_ann expr))

let expression_info_block : type s. s Ast.expr -> string =
  fun expr ->
    Format.asprintf "\n\nExpr:\n```rizz\n%a\n```" Ast.pp_expr expr

let hover_text_for_expr : type s. s Ast.expr -> string =
  fun expr ->
    let base =
      match expr with
      | Ast.EConst (c, _) ->
          "constant "
          ^
          (match c with
           | Ast.CUnit -> "()"
           | Ast.CNever -> "never"
           | Ast.CInt n -> string_of_int n
           | Ast.CBool b -> if b then "true" else "false"
           | Ast.CString s -> s)
      | Ast.EVar (name, _) -> "variable " ^ name
      | Ast.ECtor ((name, _), _, _) -> "constructor " ^ name
      | Ast.ELet ((name, _), _, _, _) -> "let-binding " ^ name
      | Ast.EFun (params, _, _) -> "function(" ^ String.concat ", " (List.map fst params) ^ ")"
      | Ast.EApp _ -> "function application"
      | Ast.EUnary _ -> "unary expression"
      | Ast.EBinary _ -> "binary expression"
      | Ast.ETuple _ -> "tuple expression"
      | Ast.ECase _ -> "match expression"
      | Ast.EIfe _ -> "if expression"
      | Ast.EAnno _ -> "annotated expression"
    in
    base ^ type_info_block expr ^ expression_info_block expr

let rec pattern_bound_decls : type s. s Ast.pattern -> (string * range) list =
  fun pat ->
    match pat with
    | Ast.PWildcard | Ast.PConst _ -> []
    | Ast.PVar (name, ann) -> [ (name, range_of_ann ann) ]
  | Ast.PSigCons (p1, p2, _) | Ast.PStringCons (p1, p2, _) ->
        pattern_bound_decls p1 @ [ (fst p2, range_of_ann (snd p2)) ]
    | Ast.PTuple (p1, p2, _) ->
        pattern_bound_decls p1 @ pattern_bound_decls p2
    | Ast.PCtor (_, args, _) ->
        List.flatten (List.map pattern_bound_decls args)

let rec pattern_bound_symbols : type s. s Ast.pattern -> (string * completion_symbol) list =
  fun pat ->
    match pat with
    | Ast.PWildcard | Ast.PConst _ -> []
    | Ast.PVar (name, ann) ->
        [
          ( name,
            {
              kind = SemanticVariable;
              range = range_of_ann ann;
              detail = detail_of_ann_opt ann;
              source = CompletionLocal;
            } );
        ]
    | Ast.PSigCons (p1, p2, _) | Ast.PStringCons (p1, p2, _) ->
        pattern_bound_symbols p1
        @ [
            ( fst p2,
              {
                kind = SemanticVariable;
                range = range_of_ann (snd p2);
                detail = detail_of_ann_opt (snd p2);
                source = CompletionLocal;
              } );
          ]
    | Ast.PTuple (p1, p2, _) ->
        pattern_bound_symbols p1 @ pattern_bound_symbols p2
    | Ast.PCtor (_, args, _) ->
        List.flatten (List.map pattern_bound_symbols args)

  let rec pattern_bound_names : type s. s Ast.pattern -> s Ast.name list =
    fun pat ->
    match pat with
    | Ast.PWildcard | Ast.PConst _ -> []
    | Ast.PVar (name, ann) -> [ (name, ann) ]
    | Ast.PSigCons (p1, p2, _) | Ast.PStringCons (p1, p2, _) ->
      pattern_bound_names p1 @ [ p2 ]
    | Ast.PTuple (p1, p2, _) ->
      pattern_bound_names p1 @ pattern_bound_names p2
    | Ast.PCtor (_, args, _) ->
      List.flatten (List.map pattern_bound_names args)

let rec pattern_constructor_ranges : type s. s Ast.pattern -> range list =
  fun pat ->
    match pat with
    | Ast.PWildcard | Ast.PConst _ | Ast.PVar _ -> []
  | Ast.PSigCons (p1, _, _) | Ast.PStringCons (p1, _, _) ->
        pattern_constructor_ranges p1
    | Ast.PTuple (p1, p2, _) ->
        pattern_constructor_ranges p1 @ pattern_constructor_ranges p2
    | Ast.PCtor (ctor_name, args, _) ->
        range_of_name ctor_name :: List.flatten (List.map pattern_constructor_ranges args)

let keyword_range_from_expr : type s. name:string -> s Ast.expr -> range option =
  fun ~name expr ->
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

let completion_kind_of_semantic_kind = function
  | SemanticFunction -> 3
  | SemanticVariable -> 6
  | SemanticType -> 4

let completion_source_rank = function
  | CompletionLocal -> 0
  | CompletionTopLevel -> 1
  | CompletionBuiltin -> 2
  | CompletionConstructor -> 3

let completion_empty_list = {
  items = [];
  is_incomplete = false;
}

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
  | Ok { diagnostics; _ } -> { diagnostics }
  | Error diagnostic -> { diagnostics = [diagnostic] }

let document_symbols ~(uri : string) ~(filename : string option) ~(text : string) : document_symbol list =
  match parse_with_filename ~filename:(file_name ~uri ~filename) text with
  | Error _ -> []
  | Ok { typed_program = program; _ } ->
      top_level_declarations ~text program
      |> List.map (fun (name, kind, top_range, selection_range) ->
             { name; kind; range = top_range; selection_range })

let definition_at_position ~(uri : string) ~(filename : string option) ~(text : string) ~(position : position) : definition option =
  match parse_with_filename ~filename:(file_name ~uri ~filename) text with
  | Error _ -> None
  | Ok { typed_program = program; _ } ->
      let top_decls = top_level_declarations ~text program in
      let top_env =
        top_decls
        |> List.fold_left
             (fun env (name, kind, _top_range, selection_range) ->
               StringMap.add name { kind = semantic_kind_of_symbol_kind kind; range = selection_range } env)
             StringMap.empty
      in
      let rec find_in_expr (env : scoped_symbol StringMap.t) (expr : _ Ast.expr) : definition option =
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
        | Ast.ECtor (ctor_name, args, _) ->
            let ctor_range = range_of_name ctor_name in
            if range_contains_position ctor_range position then
              Some { name = name_text ctor_name; range = ctor_range }
            else
              List.find_map (find_in_expr env) args
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
        | Ast.EAnno (e, _, _) -> find_in_expr env e
      in
      let rec find_in_tops tops =
        match tops with
        | [] -> None
        | Ast.TopLet (top_name, rhs, _) :: rest ->
            let name = name_text top_name in
            let name_range = range_of_name top_name in
            if range_contains_position name_range position then
              Some { name; range = name_range }
            else
              let kind = semantic_kind_of_symbol_kind (symbol_kind_of_expr rhs) in
              let env' = StringMap.add name { kind; range = name_range } top_env in
              match find_in_expr env' rhs with
              | Some _ as found -> found
              | None -> find_in_tops rest
      in
      find_in_tops program

let hover_at_position ~(uri : string) ~(filename : string option) ~(text : string) ~(position : position) : hover_info option =
  match parse_with_filename ~filename:(file_name ~uri ~filename) text with
  | Error _ -> None
  | Ok { typed_program = program; _ } ->
      let top_level_hover =
        List.find_map
          (fun (top : Ast.typed Ast.top_expr) ->
            match top with
            | Ast.TopLet (top_name, rhs, top_ann) ->
                let name_range = range_of_name top_name in
                if range_contains_position name_range position then
                  let base =
                    match symbol_kind_of_expr rhs with
                    | Function -> "top-level function: " ^ name_text top_name
                    | Variable -> "top-level binding: " ^ name_text top_name
                  in
                  Some { range = name_range; contents = base ^ type_info_block_of_ann top_ann }
                else
                  None)
          program
      in
      match top_level_hover with
      | Some _ as hover -> hover
      | None ->
          let rec find_symbol_hover_in_expr : type s. s Ast.expr -> hover_info option =
            fun expr ->
              match expr with
              | Ast.EConst _ -> None
              | Ast.EVar var_name ->
                  let var_range = range_of_name var_name in
                  if range_contains_position var_range position then
                    Some { range = var_range; contents = hover_text_for_named_symbol ~label:"variable" var_name }
                  else
                    None
              | Ast.ECtor (ctor_name, args, _) ->
                  let ctor_range = range_of_name ctor_name in
                  if range_contains_position ctor_range position then
                    Some { range = ctor_range; contents = "constructor " ^ name_text ctor_name }
                  else
                    List.find_map find_symbol_hover_in_expr args
              | Ast.ELet (bound_name, e1, e2, _) ->
                  let binding_range = range_of_name bound_name in
                  if range_contains_position binding_range position then
                    Some { range = binding_range; contents = hover_text_for_named_symbol ~label:"let-binding" bound_name }
                  else
                    (match find_symbol_hover_in_expr e1 with
                     | Some _ as hover -> hover
                     | None -> find_symbol_hover_in_expr e2)
              | Ast.EFun (params, body, _) ->
                  let param_hover =
                    params
                    |> List.find_map (fun param ->
                           let param_range = range_of_name param in
                           if range_contains_position param_range position then
                             Some { range = param_range; contents = hover_text_for_named_symbol ~label:"parameter" param }
                           else
                             None)
                  in
                  (match param_hover with
                   | Some _ as hover -> hover
                   | None -> find_symbol_hover_in_expr body)
              | Ast.EApp (fn, args, _) ->
                  (match find_symbol_hover_in_expr fn with
                   | Some _ as hover -> hover
                   | None -> List.find_map find_symbol_hover_in_expr args)
              | Ast.EUnary (_, e, _) -> find_symbol_hover_in_expr e
              | Ast.EBinary (_, e1, e2, _) ->
                  (match find_symbol_hover_in_expr e1 with
                   | Some _ as hover -> hover
                   | None -> find_symbol_hover_in_expr e2)
              | Ast.ETuple (e1, e2, _) ->
                  (match find_symbol_hover_in_expr e1 with
                   | Some _ as hover -> hover
                   | None -> find_symbol_hover_in_expr e2)
              | Ast.ECase (scrutinee, branches, _) ->
                  (match find_symbol_hover_in_expr scrutinee with
                   | Some _ as hover -> hover
                   | None ->
                       List.find_map
                         (fun (pattern, branch_expr, _) ->
                           let pattern_hover =
                             pattern_bound_names pattern
                             |> List.find_map (fun bound_name ->
                                    let bound_range = range_of_name bound_name in
                                    if range_contains_position bound_range position then
                                      Some {
                                        range = bound_range;
                                        contents = hover_text_for_named_symbol ~label:"pattern binding" bound_name;
                                      }
                                    else
                                      None)
                           in
                           match pattern_hover with
                           | Some _ as hover -> hover
                           | None -> find_symbol_hover_in_expr branch_expr)
                         branches)
              | Ast.EIfe (c, t, e, _) ->
                  (match find_symbol_hover_in_expr c with
                   | Some _ as hover -> hover
                   | None ->
                       match find_symbol_hover_in_expr t with
                       | Some _ as hover -> hover
                       | None -> find_symbol_hover_in_expr e)
              | Ast.EAnno (e, _, _) -> find_symbol_hover_in_expr e
          in
          let symbol_hover =
            List.find_map
              (fun (top : Ast.typed Ast.top_expr) ->
                match top with
                | Ast.TopLet (_, rhs, _) -> find_symbol_hover_in_expr rhs)
              program
          in
          match symbol_hover with
          | Some _ as hover -> hover
          | None ->
              collect_expr_ranges program
              |> List.filter (fun (_, range) -> range_contains_position range position)
              |> List.sort (fun (_, r1) (_, r2) -> compare (range_span_score r1) (range_span_score r2))
              |> first_opt
              |> Option.map (fun (expr, range) -> { range; contents = hover_text_for_expr expr })

let completion_symbol_of_name : type s.
    source:completion_source -> kind:semantic_token_kind -> s Ast.name -> completion_symbol =
  fun ~source ~kind name ->
    {
      kind;
      range = range_of_name name;
      detail = detail_of_name name;
      source;
    }

let completion_add_prefer_high_priority
    (name : string)
    (symbol : completion_symbol)
    (env : completion_symbol StringMap.t) : completion_symbol StringMap.t =
  match StringMap.find_opt name env with
  | None -> StringMap.add name symbol env
  | Some existing ->
      if completion_source_rank symbol.source < completion_source_rank existing.source then
        StringMap.add name symbol env
      else
        env

let completion_add_shadowing
    (name : string)
    (symbol : completion_symbol)
    (env : completion_symbol StringMap.t) : completion_symbol StringMap.t =
  StringMap.add name symbol env

let builtin_scoped_symbols : scoped_symbol StringMap.t =
  List.fold_left
    (fun env ({ name; typ; _ } : Rizzo_builtins.builtin_info) ->
      let kind =
        match typ with
        | Ast.TFun _ -> SemanticFunction
        | _ -> SemanticVariable
      in
      let empty_range =
        {
          start_pos = { line = 0; character = 0 };
          end_pos = { line = 0; character = 0 };
        }
      in
      StringMap.add name { kind; range = empty_range } env)
    StringMap.empty
    Rizzo_builtins.public_builtins

let builtin_completion_symbols : completion_symbol StringMap.t =
  let empty_range =
    {
      start_pos = { line = 0; character = 0 };
      end_pos = { line = 0; character = 0 };
    }
  in
  List.fold_left
    (fun env ({ name; typ; _ } : Rizzo_builtins.builtin_info) ->
      let kind =
        match typ with
        | Ast.TFun _ -> SemanticFunction
        | _ -> SemanticVariable
      in
      completion_add_prefer_high_priority
        name
        {
          kind;
          range = empty_range;
          detail = Some (string_of_typ typ);
          source = CompletionBuiltin;
        }
        env)
    StringMap.empty
      Rizzo_builtins.public_builtins

let constructor_completion_specs : (string * Ast.typ) list =
  [
    ("Just", Ast.TFun (Ast.Cons1 (Ast.TParam "'a", []), Ast.TOption (Ast.TParam "'a")));
    ("Nothing", Ast.TOption (Ast.TParam "'a"));
    ("Left", Ast.TFun (Ast.Cons1 (Ast.TParam "'a", []), Ast.TSync (Ast.TParam "'a", Ast.TParam "'b")));
    ("Right", Ast.TFun (Ast.Cons1 (Ast.TParam "'b", []), Ast.TSync (Ast.TParam "'a", Ast.TParam "'b")));
    ("Both", Ast.TFun (Ast.Cons1 (Ast.TParam "'a", [Ast.TParam "'b"]), Ast.TSync (Ast.TParam "'a", Ast.TParam "'b")));
  ]

let constructor_completion_symbols : completion_symbol StringMap.t =
  let empty_range =
    {
      start_pos = { line = 0; character = 0 };
      end_pos = { line = 0; character = 0 };
    }
  in
  List.fold_left
    (fun env (name, typ) ->
      completion_add_prefer_high_priority
        name
        {
          kind = SemanticType;
          range = empty_range;
          detail = Some (string_of_typ typ);
          source = CompletionConstructor;
        }
        env)
    StringMap.empty
    constructor_completion_specs

let completion_prefix_at_position ~(text : string) ~(position : position) : string =
  let lines = lines_of_text text in
  match line_at lines position.line with
  | None -> ""
  | Some line_text ->
      let line_len = String.length line_text in
      let cursor = max 0 (min position.character line_len) in
      let rec find_start index =
        if index <= 0 then
          0
        else
          let previous = index - 1 in
          if is_identifier_char line_text.[previous] then
            find_start previous
          else
            index
      in
      let start_index = find_start cursor in
      if cursor <= start_index then
        ""
      else
        String.sub line_text start_index (cursor - start_index)

let completion_sort
    ~(prefix : string)
    ((left_name, left_symbol) : string * completion_symbol)
    ((right_name, right_symbol) : string * completion_symbol) : int =
  let exact_rank name =
    if prefix <> "" && String.length name = String.length prefix then 0 else 1
  in
  let by_exact = compare (exact_rank left_name) (exact_rank right_name) in
  if by_exact <> 0 then
    by_exact
  else
    let by_source = compare (completion_source_rank left_symbol.source) (completion_source_rank right_symbol.source) in
    if by_source <> 0 then
      by_source
    else
      String.compare (String.lowercase_ascii left_name) (String.lowercase_ascii right_name)

let completion_items_of_env
    ~(text : string)
    ~(position : position)
    (env : completion_symbol StringMap.t) : completion_list =
  let prefix = completion_prefix_at_position ~text ~position in
  let bindings : (string * completion_symbol) list = StringMap.bindings env in
  let filtered : (string * completion_symbol) list =
    bindings
    |> List.filter (fun ((name, _) : string * completion_symbol) -> prefix = "" || starts_with ~prefix name)
    |> List.sort (completion_sort ~prefix)
  in
  let items =
    filtered
    |> List.map (fun ((name, symbol) : string * completion_symbol) ->
           {
             label = name;
             kind = completion_kind_of_semantic_kind symbol.kind;
             detail = symbol.detail;
             documentation = None;
           })
  in
  {
    items;
    is_incomplete = false;
  }

let completions_at_position
    ~(uri : string)
    ~(filename : string option)
    ~(text : string)
    ~(position : position) : completion_list =
  match parse_with_filename ~filename:(file_name ~uri ~filename) text with
  | Error _ -> completion_empty_list
  | Ok { typed_program = program; _ } ->
      let _ = uri in
      let base_env =
        let with_constructors =
          StringMap.fold completion_add_prefer_high_priority constructor_completion_symbols builtin_completion_symbols
        in
        List.fold_left
          (fun env (top : Ast.typed Ast.top_expr) ->
            match top with
            | Ast.TopLet (top_name, rhs, _) ->
                let kind = semantic_kind_of_symbol_kind (symbol_kind_of_expr rhs) in
                completion_add_prefer_high_priority
                  (name_text top_name)
                  (completion_symbol_of_name ~source:CompletionTopLevel ~kind top_name)
                  env)
          with_constructors
          program
      in
      let rec env_in_expr
          (env : completion_symbol StringMap.t)
          (expr : Ast.typed Ast.expr) : completion_symbol StringMap.t option =
        let expr_range = range_of_ann (Ast.expr_get_ann expr) in
        if not (range_contains_position expr_range position) then
          None
        else
          match expr with
          | Ast.EConst _ | Ast.EVar _ -> Some env
          | Ast.ECtor (ctor_name, args, _) ->
              if range_contains_position (range_of_name ctor_name) position then
                Some env
              else
                (match List.find_map (env_in_expr env) args with
                 | Some _ as found -> found
                 | None -> Some env)
          | Ast.ELet (bound_name, e1, e2, _) ->
              let bound_range = range_of_name bound_name in
              if range_contains_position bound_range position then
                Some env
              else
                (match env_in_expr env e1 with
                 | Some _ as found -> found
                 | None ->
                     let env' =
                       completion_add_shadowing
                         (name_text bound_name)
                         (completion_symbol_of_name ~source:CompletionLocal ~kind:SemanticVariable bound_name)
                         env
                     in
                     (match env_in_expr env' e2 with
                      | Some _ as found -> found
                      | None -> Some env'))
          | Ast.EFun (params, body, _) ->
              let cursor_on_param =
                List.exists
                  (fun param -> range_contains_position (range_of_name param) position)
                  params
              in
              if cursor_on_param then
                Some env
              else
                let env' =
                  List.fold_left
                    (fun acc param ->
                      completion_add_shadowing
                        (name_text param)
                        (completion_symbol_of_name ~source:CompletionLocal ~kind:SemanticVariable param)
                        acc)
                    env
                    params
                in
                (match env_in_expr env' body with
                 | Some _ as found -> found
                 | None -> Some env')
          | Ast.EApp (fn, args, _) ->
              (match env_in_expr env fn with
               | Some _ as found -> found
               | None ->
                   (match List.find_map (env_in_expr env) args with
                    | Some _ as found -> found
                    | None -> Some env))
          | Ast.EUnary (_, e, _) ->
              (match env_in_expr env e with
               | Some _ as found -> found
               | None -> Some env)
          | Ast.EBinary (_, e1, e2, _) | Ast.ETuple (e1, e2, _) ->
              (match env_in_expr env e1 with
               | Some _ as found -> found
               | None ->
                   (match env_in_expr env e2 with
                    | Some _ as found -> found
                    | None -> Some env))
          | Ast.ECase (scrutinee, branches, _) ->
              (match env_in_expr env scrutinee with
               | Some _ as found -> found
               | None ->
                   let in_branch (pattern, branch_expr, branch_ann) =
                     let branch_range = range_of_ann branch_ann in
                     if not (range_contains_position branch_range position) then
                       None
                     else
                       let bound = pattern_bound_symbols pattern in
                       let bound : (string * completion_symbol) list = bound in
                       let cursor_on_pattern_binding =
                         List.exists
                           (fun ((_, symbol) : string * completion_symbol) -> range_contains_position symbol.range position)
                           bound
                       in
                       let cursor_on_pattern_constructor =
                         List.exists
                           (fun ctor_range -> range_contains_position ctor_range position)
                           (pattern_constructor_ranges pattern)
                       in
                       if cursor_on_pattern_binding || cursor_on_pattern_constructor then
                         Some env
                       else
                         let env' =
                           List.fold_left
                             (fun acc ((name, symbol) : string * completion_symbol) ->
                               completion_add_shadowing name symbol acc)
                             env
                             bound
                         in
                         (match env_in_expr env' branch_expr with
                          | Some _ as found -> found
                          | None -> Some env')
                   in
                   (match List.find_map in_branch branches with
                    | Some _ as found -> found
                    | None -> Some env))
          | Ast.EIfe (c, t, e, _) ->
              (match env_in_expr env c with
               | Some _ as found -> found
               | None ->
                   (match env_in_expr env t with
                    | Some _ as found -> found
                    | None ->
                        (match env_in_expr env e with
                         | Some _ as found -> found
                         | None -> Some env)))
          | Ast.EAnno (e, _, _) ->
              (match env_in_expr env e with
               | Some _ as found -> found
               | None -> Some env)
      in
      let rec env_in_tops = function
        | [] -> None
        | Ast.TopLet (top_name, rhs, top_ann) :: rest ->
            if range_contains_position (range_of_ann top_ann) position then
              if range_contains_position (range_of_name top_name) position then
                Some base_env
              else
                (match env_in_expr base_env rhs with
                 | Some _ as found -> found
                 | None -> Some base_env)
            else
              env_in_tops rest
      in
      let env =
        match env_in_tops program with
        | Some scope_env -> scope_env
        | None -> base_env
      in
      completion_items_of_env ~text ~position env

let semantic_tokens ~(uri : string) ~(filename : string option) ~(text : string) : semantic_token list =
  match parse_with_filename ~filename:(file_name ~uri ~filename) text with
  | Error _ -> []
  | Ok { typed_program = program; _ } ->
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
             builtin_scoped_symbols
      in
      let rec walk_expr (env : scoped_symbol StringMap.t) (expr : Ast.typed Ast.expr) : unit =
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
         | Ast.ECtor (ctor_name, args, _) ->
             push_token ~kind:SemanticType ~range:(range_of_name ctor_name);
             List.iter (walk_expr env) args
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
          | Ast.EUnary (Ast.UNot, e, _) ->
              (match keyword_range_from_expr ~name:"not" expr with
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
                 List.iter (fun ctor_range -> push_token ~kind:SemanticType ~range:ctor_range)
                   (pattern_constructor_ranges pat);
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
             walk_expr env e
         | Ast.EAnno (e, _, _) -> walk_expr env e)
      in
      List.iter
        (function
          | Ast.TopLet (top_name, rhs, _) ->
              let name = name_text top_name in
              let name_range = range_of_name top_name in
              let kind = semantic_kind_of_symbol_kind (symbol_kind_of_expr rhs) in
              let env' = StringMap.add name { kind; range = name_range } top_env in
              walk_expr env' rhs)
        program;
      !tokens
      |> List.filter (fun (token : semantic_token) -> valid_single_line_range token.range)
      |> List.sort (fun (left : semantic_token) (right : semantic_token) -> compare_range_position left.range right.range)
