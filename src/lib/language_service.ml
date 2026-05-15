open Collections
include Language_service_parts.Types
include Language_service_parts.Document
include Language_service_parts.Parse

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

let rec expr_is_function : type s. s Ast.expr -> bool =
  function
  | Ast.EFun _ -> true
  | Ast.EAnno (expr, _, _) -> expr_is_function expr
  | _ -> false

let symbol_kind_of_expr : type s. s Ast.expr -> symbol_kind =
  fun expr -> if expr_is_function expr then Function else Variable

let top_level_declarations : type s.
    filename:string option -> s Ast.program -> (string * symbol_kind * string * range * range) list =
  fun ~filename program ->
  let declaration_of_top : s Ast.top_expr -> (string * symbol_kind * string * range * range) option = fun top ->
    match top with
    | Ast.TopLet (top_name, rhs, ann) ->
        if Option.is_some filename && not (ann_in_file ~filename:(Option.get filename) ann) then
          None
        else
          let kind = symbol_kind_of_expr rhs in
          let name = name_text top_name in
          let filename = ann_filename ann in
          let top_range = range_of_ann ann in
          let selection_range = range_of_name top_name in
          Some (name, kind, filename, top_range, selection_range)
        | Ast.TopTypeDef _ -> None
  in
  List.filter_map declaration_of_top program

let top_level_constructor_declarations : type s.
    filename:string option -> s Ast.program -> (string * string * range) list =
  fun ~filename program ->
  let declarations_of_top : s Ast.top_expr -> (string * string * range) list = fun top ->
    match top with
    | Ast.TopLet _ -> []
    | Ast.TopTypeDef (_, _, ctors, top_ann) ->
        if Option.is_some filename && not (ann_in_file ~filename:(Option.get filename) top_ann) then
          []
        else
          List.map
            (fun (ctor_name, _, ctor_ann : s Ast.ctor_def) ->
              (name_text ctor_name, ann_filename ctor_ann, range_of_name ctor_name))
            ctors
  in
  List.concat_map declarations_of_top program

    let rec iter_expr : type s. (s Ast.expr -> unit) -> s Ast.expr -> unit =
      fun f expr ->
      f expr;
      match expr with
      | Ast.EConst _ | Ast.EError _ | Ast.EVar _ -> ()
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

    let collect_expr_ranges : type s. filename:string -> s Ast.program -> (s Ast.expr * range) list =
      fun ~filename program ->
        let acc = ref [] in
        let push expr =
          let expr_range = range_of_ann (Ast.expr_get_ann expr) in
          acc := (expr, expr_range) :: !acc
        in
        let visit_top = function
          | Ast.TopLet (_, rhs, ann) when ann_in_file ~filename ann -> iter_expr push rhs
          | Ast.TopLet _ -> ()
          | Ast.TopTypeDef _ -> ()
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
  | Some t ->
      let type_text = Format.asprintf "%a" Ast.pp_typ t |> dedent_text in
      Format.asprintf "\n\nType:\n```rizz\n%s\n```" type_text

let type_info_block_of_ann : type s. s Ast.ann -> string =
  fun ann -> type_info_block_of_typ_opt (typ_of_ann_opt ann)

let definition_type_info_block_of_typ_opt (typ : Ast.typ option) : string =
  match typ with
  | None -> ""
  | Some t ->
      let type_text = Format.asprintf "%a" Ast.pp_typ t |> dedent_text in
      Format.asprintf "\n\nDefinition type:\n```rizz\n%s\n```" type_text

let distinct_definition_type_block (usage_type : Ast.typ option) (definition_type : Ast.typ option) : string =
  match usage_type, definition_type with
  | Some usage_type, Some definition_type when Ast.eq_typ usage_type definition_type -> ""
  | _, definition_type -> definition_type_info_block_of_typ_opt definition_type

let top_level_type_definition_text : type s. s Ast.top_expr -> string =
  fun top ->
    let top_text = Format.asprintf "%a" Ast.pp_top_expr top |> dedent_text in
    Format.asprintf "\n\nType definition:\n```rizz\n%s\n```" top_text

let hover_text_for_named_symbol : type s. ?documentation:string -> s Ast.name -> string =
  fun ?documentation (_, ann) ->
    doc_info_block documentation ^ type_info_block_of_ann ann

let hover_text_for_named_symbol_with_definition_type : type s.
    ?documentation:string -> definition_type:Ast.typ option -> s Ast.name -> string =
  fun ?documentation ~definition_type (name, ann) ->
    hover_text_for_named_symbol ?documentation (name, ann)
    ^ distinct_definition_type_block (typ_of_ann_opt ann) definition_type

let hover_text_for_pattern_ann : type s. s Ast.ann -> string =
  fun ann -> type_info_block_of_ann ann

let type_info_block : type s. s Ast.expr -> string =
  fun expr ->
    type_info_block_of_typ_opt (typ_of_ann_opt (Ast.expr_get_ann expr))

let expression_info_block : type s. s Ast.expr -> string =
  fun expr ->
    let expr_text = Format.asprintf "%a" Ast.pp_expr expr |> dedent_text in
    Format.asprintf "\n\nExpr:\n```rizz\n%s\n```" expr_text

let hover_text_for_expr : type s. s Ast.expr -> string =
  fun expr ->
    type_info_block expr ^ expression_info_block expr

let rec pattern_bound_decls : type s. s Ast.pattern -> (string * range) list =
  fun pat ->
    match pat with
    | Ast.PWildcard _ | Ast.PConst _ | Ast.PError _ -> []
    | Ast.PVar (name, ann) -> [ (name, range_of_ann ann) ]
  | Ast.PSigCons (p1, p2, _) | Ast.PStringCons (p1, p2, _) ->
        pattern_bound_decls p1 @ [ (fst p2, range_of_ann (snd p2)) ]
    | Ast.PTuple (p1, p2, _) ->
        pattern_bound_decls p1 @ pattern_bound_decls p2
    | Ast.PCtor (_, args, _) ->
        List.flatten (List.map pattern_bound_decls args)

  let rec pattern_constructor_occurrences : type s. s Ast.pattern -> (string * range) list =
    fun pat ->
    match pat with
    | Ast.PWildcard _ | Ast.PConst _ | Ast.PVar _ | Ast.PError _ -> []
    | Ast.PSigCons (p1, _, _) | Ast.PStringCons (p1, _, _) ->
      pattern_constructor_occurrences p1
    | Ast.PTuple (p1, p2, _) ->
      pattern_constructor_occurrences p1 @ pattern_constructor_occurrences p2
    | Ast.PCtor (ctor_name, args, _) ->
      (name_text ctor_name, range_of_name ctor_name)
      :: List.flatten (List.map pattern_constructor_occurrences args)

let rec pattern_bound_symbols : type s. s Ast.pattern -> (string * completion_symbol) list =
  fun pat ->
    match pat with
    | Ast.PWildcard _ | Ast.PConst _ | Ast.PError _ -> []
    | Ast.PVar (name, ann) ->
        [
          ( name,
            {
              kind = SemanticVariable;
              range = range_of_ann ann;
              detail = detail_of_ann_opt ann;
              documentation = None;
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
                documentation = None;
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
    | Ast.PWildcard _ | Ast.PConst _ | Ast.PError _ -> []
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
    | Ast.PWildcard _ | Ast.PConst _ | Ast.PVar _ | Ast.PError _ -> []
  | Ast.PSigCons (p1, _, _) | Ast.PStringCons (p1, _, _) ->
        pattern_constructor_ranges p1
    | Ast.PTuple (p1, p2, _) ->
        pattern_constructor_ranges p1 @ pattern_constructor_ranges p2
    | Ast.PCtor (ctor_name, args, _) ->
        range_of_name ctor_name :: List.flatten (List.map pattern_constructor_ranges args)

let rec pattern_hover_at_position : type s. position:position -> s Ast.pattern -> hover_info option =
  fun ~position pat ->
    match pat with
    | Ast.PWildcard ann ->
        let pat_range = range_of_ann ann in
        if range_contains_position pat_range position then
          Some { range = pat_range; contents = hover_text_for_pattern_ann ann }
        else
          None
    | Ast.PVar (name, ann) ->
        let pat_range = range_of_ann ann in
        if range_contains_position pat_range position then
          Some { range = pat_range; contents = hover_text_for_named_symbol (name, ann) }
        else
          None
    | Ast.PConst (_, _) -> None
    | Ast.PError (_, _) -> None
    | Ast.PTuple (p1, p2, _) ->
        (match pattern_hover_at_position ~position p1 with
         | Some _ as hover -> hover
         | None -> pattern_hover_at_position ~position p2)
    | Ast.PSigCons (p1, p2, _) | Ast.PStringCons (p1, p2, _) ->
        (match pattern_hover_at_position ~position p1 with
         | Some _ as hover -> hover
         | None ->
              let pat_range = range_of_name p2 in
              if range_contains_position pat_range position then
                Some { range = pat_range; contents = hover_text_for_named_symbol p2 }
              else
                None)
    | Ast.PCtor (ctor_name, args, _) ->
        let ctor_range = range_of_name ctor_name in
        if range_contains_position ctor_range position then
          Some { range = ctor_range; contents = hover_text_for_named_symbol ctor_name }
        else
          List.find_map (pattern_hover_at_position ~position) args

let keyword_range_from_start ~(name : string) ~(start_pos : position) : range =
  {
    start_pos;
    end_pos =
      {
        line = start_pos.line;
        character = start_pos.character + String.length name;
      };
  }

let text_has_keyword_at_position ~(text : string) ~(name : string) ~(start_pos : position) : bool =
  let lines = lines_of_text text in
  match line_at lines start_pos.line with
  | None -> false
  | Some line_text ->
      let available = String.length line_text - start_pos.character in
      available >= String.length name
      && String.sub line_text start_pos.character (String.length name) = name

let keyword_range_from_expr : type s. text:string -> name:string -> s Ast.expr -> range option =
  fun ~text ~name expr ->
    let expr_range = range_of_ann (Ast.expr_get_ann expr) in
    if text_has_keyword_at_position ~text ~name ~start_pos:expr_range.start_pos then
      Some (keyword_range_from_start ~name ~start_pos:expr_range.start_pos)
    else
      None

let unary_keyword_name = function
  | Ast.UWait -> Some "wait"
  | Ast.UWatch -> Some "watch"
  | Ast.UTail -> Some "tail"
  | Ast.UDelay -> Some "delay"
  | Ast.UNot -> Some "not"
  | _ -> None

let binary_keyword_name = function
  | Ast.BSync -> Some "sync"
  | _ -> None

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
  let active_filename = file_name ~uri ~filename in
  match parse_with_filename ~filename:active_filename text with
  | Error _ -> []
  | Ok { typed_program = program; _ } ->
      top_level_declarations ~filename:(Some active_filename) program
      |> List.map (fun (name, kind, _decl_filename, top_range, selection_range) ->
             { name; kind; range = top_range; selection_range })

let definition_at_position ~(uri : string) ~(filename : string option) ~(text : string) ~(position : position) : definition option =
  let active_filename = file_name ~uri ~filename in
  match parse_with_filename ~filename:active_filename text with
  | Error _ -> None
  | Ok { typed_program = program; _ } ->
      let top_decls = top_level_declarations ~filename:None program in
      let top_env =
        top_decls
        |> List.fold_left
             (fun env (name, _kind, decl_filename, _top_range, selection_range) ->
               StringMap.add
                 name
                 { filename = decl_filename; range = selection_range }
                 env)
             StringMap.empty
      in
      let constructor_env =
        top_level_constructor_declarations ~filename:None program
        |> List.fold_left
             (fun env (name, decl_filename, declaration_range) ->
               StringMap.add
                 name
                 { filename = decl_filename; range = declaration_range }
                 env)
             StringMap.empty
      in
      let lookup_constructor name _range =
        match StringMap.find_opt name constructor_env with
        | Some symbol when String.equal symbol.filename active_filename ->
            Some { name; filename = symbol.filename; range = symbol.range }
        | _ -> None
      in
      let rec find_in_expr (env : definition_symbol StringMap.t) (expr : _ Ast.expr) : definition option =
        match expr with
        | Ast.EConst _ | Ast.EError _ -> None
        | Ast.EVar var_name ->
            let var_range = range_of_name var_name in
            if range_contains_position var_range position then
              let name = name_text var_name in
              (match StringMap.find_opt name env with
               | Some symbol -> Some { name; filename = symbol.filename; range = symbol.range }
               | None -> Some { name; filename = active_filename; range = var_range })
            else
              None
        | Ast.ECtor (ctor_name, args, _) ->
            let ctor_range = range_of_name ctor_name in
            if range_contains_position ctor_range position then
              Some { name = name_text ctor_name; filename = active_filename; range = ctor_range }
            else
              List.find_map (find_in_expr env) args
        | Ast.ELet (bound_name, e1, e2, _) ->
            let binding_range = range_of_name bound_name in
            if range_contains_position binding_range position then
              Some { name = name_text bound_name; filename = active_filename; range = binding_range }
            else
              (match find_in_expr env e1 with
               | Some _ as found -> found
               | None ->
                   let env' =
                     StringMap.add
                       (name_text bound_name)
                       { filename = active_filename; range = binding_range }
                       env
                   in
                   find_in_expr env' e2)
        | Ast.EFun (params, body, _) ->
            let declared_here =
              params
              |> List.find_map (fun param ->
                     pattern_bound_decls param
                     |> List.find_map (fun (name, param_range) ->
                            if range_contains_position param_range position then
                              Some { name; filename = active_filename; range = param_range }
                            else
                              None))
            in
            (match declared_here with
             | Some _ as found -> found
             | None ->
                 let constructor_here =
                   params
                   |> List.find_map (fun param ->
                          pattern_constructor_occurrences param
                          |> List.find_map (fun (ctor_name, ctor_range) ->
                                 if range_contains_position ctor_range position then
                                   lookup_constructor ctor_name ctor_range
                                 else
                                   None))
                 in
                 (match constructor_here with
                  | Some _ as found -> found
                  | None ->
                      let env' =
                        List.fold_left
                          (fun acc param ->
                            pattern_bound_decls param
                            |> List.fold_left
                                 (fun acc (name, param_range) ->
                                   StringMap.add
                                     name
                                     { filename = active_filename; range = param_range }
                                     acc)
                                 acc)
                          env
                          params
                      in
                      find_in_expr env' body))
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
                                Some { name; filename = active_filename; range = binding_range }
                              else
                                None)
                     in
                     match bound_here with
                     | Some _ as found -> found
                     | None ->
                         let env' =
                           List.fold_left
                             (fun acc (name, binding_range) ->
                               StringMap.add
                                 name
                                 { filename = active_filename; range = binding_range }
                                 acc)
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
        | Ast.TopTypeDef _ :: rest -> find_in_tops rest
        | Ast.TopLet (top_name, rhs, ann) :: rest ->
            if not (ann_in_file ~filename:active_filename ann) then
              find_in_tops rest
            else
              let name = name_text top_name in
              let name_range = range_of_name top_name in
              if range_contains_position name_range position then
                Some { name; filename = ann_filename ann; range = name_range }
              else
                let env' = StringMap.add name { filename = ann_filename ann; range = name_range } top_env in
                match find_in_expr env' rhs with
                | Some _ as found -> found
                | None -> find_in_tops rest
      in
      find_in_tops program

type rename_target = {
  name: string;
  filename: string;
  declaration_range: range;
  kind: rename_kind;
}

let rename_target_matches (left : rename_target) (right : rename_target) : bool =
  left.kind = right.kind
  && String.equal left.name right.name
  && String.equal left.filename right.filename
  && compare_range_position left.declaration_range right.declaration_range = 0

let rename_value_target ~(filename : string) ~(kind : rename_kind) (name : string) (declaration_range : range) : rename_target =
  {
    name;
    filename;
    declaration_range;
    kind;
  }

let valid_rename_name ~(kind : rename_kind) (name : string) : bool =
  let valid_start =
    match kind with
    | RenameValue ->
        (function
          | 'a' .. 'z' | '_' -> true
          | _ -> false)
    | RenameConstructor ->
        (function
          | 'A' .. 'Z' -> true
          | _ -> false)
  in
  let len = String.length name in
  len > 0
  && valid_start name.[0]
  &&
  let rec rest_ok index =
    if index >= len then
      true
    else if is_identifier_rest_char name.[index] then
      rest_ok (index + 1)
    else
      false
  in
  rest_ok 1

let rename_at_position ~(uri : string) ~(filename : string option) ~(text : string) ~(position : position) : rename_result option =
  let active_filename = file_name ~uri ~filename in
  match parse_with_filename ~filename:active_filename text with
  | Error _ -> None
  | Ok { typed_program = program; _ } ->
      let top_env : rename_target StringMap.t =
        top_level_declarations ~filename:None program
        |> List.fold_left
             (fun env (name, _symbol_kind, decl_filename, _top_range, selection_range) ->
               StringMap.add
                 name
                 (rename_value_target ~filename:decl_filename ~kind:RenameValue name selection_range)
                 env)
             StringMap.empty
      in
      let constructor_env : rename_target StringMap.t =
        top_level_constructor_declarations ~filename:None program
        |> List.fold_left
             (fun env (name, decl_filename, declaration_range) ->
               StringMap.add
                 name
                 (rename_value_target ~filename:decl_filename ~kind:RenameConstructor name declaration_range)
                 env)
             StringMap.empty
      in
      let local_target_of_opt (target : rename_target option) (range : range) : (rename_target * range) option =
        match target with
        | Some target when String.equal target.filename active_filename ->
        Some (target, range)
        | _ -> None
      in
      let lookup_value env name range =
        local_target_of_opt (StringMap.find_opt name env) range
      in
      let lookup_constructor name range =
        local_target_of_opt (StringMap.find_opt name constructor_env) range
      in
      let rec resolve_in_expr (env : rename_target StringMap.t) (expr : _ Ast.expr) : (rename_target * range) option =
        match expr with
        | Ast.EConst _ | Ast.EError _ -> None
        | Ast.EVar var_name ->
            let var_range = range_of_name var_name in
            if range_contains_position var_range position then
              lookup_value env (name_text var_name) var_range
            else
              None
        | Ast.ECtor (ctor_name, args, _) ->
            let ctor_range = range_of_name ctor_name in
            if range_contains_position ctor_range position then
              lookup_constructor (name_text ctor_name) ctor_range
            else
              List.find_map (resolve_in_expr env) args
        | Ast.ELet (bound_name, e1, e2, _) ->
            let binding_range = range_of_name bound_name in
            let binding_target =
              rename_value_target ~filename:active_filename ~kind:RenameValue (name_text bound_name) binding_range
            in
            if range_contains_position binding_range position then
              Some (binding_target, binding_range)
            else
              (match resolve_in_expr env e1 with
               | Some _ as found -> found
               | None ->
                   let env' = StringMap.add (name_text bound_name) binding_target env in
                   resolve_in_expr env' e2)
        | Ast.EFun (params, body, _) ->
            let declared_here =
              params
              |> List.find_map (fun param ->
                     pattern_bound_decls param
                     |> List.find_map (fun (name, param_range) ->
                            if range_contains_position param_range position then
                              let param_target =
                                rename_value_target ~filename:active_filename ~kind:RenameValue name param_range
                              in
                              Some (param_target, param_range)
                            else
                              None))
            in
            (match declared_here with
             | Some _ as found -> found
             | None ->
                 let constructor_here =
                   params
                   |> List.find_map (fun param ->
                          pattern_constructor_occurrences param
                          |> List.find_map (fun (ctor_name, ctor_range) ->
                                 if range_contains_position ctor_range position then
                                   lookup_constructor ctor_name ctor_range
                                 else
                                   None))
                 in
                 (match constructor_here with
                  | Some _ as found -> found
                  | None ->
                      let env' =
                        List.fold_left
                          (fun acc param ->
                            pattern_bound_decls param
                            |> List.fold_left
                                 (fun acc (name, param_range) ->
                                   StringMap.add
                                     name
                                     (rename_value_target ~filename:active_filename ~kind:RenameValue name param_range)
                                     acc)
                                 acc)
                          env
                          params
                      in
                      resolve_in_expr env' body))
        | Ast.EApp (fn, args, _) ->
            (match resolve_in_expr env fn with
             | Some _ as found -> found
             | None -> List.find_map (resolve_in_expr env) args)
        | Ast.EUnary (_, e, _) -> resolve_in_expr env e
        | Ast.EBinary (_, e1, e2, _) ->
            (match resolve_in_expr env e1 with
             | Some _ as found -> found
             | None -> resolve_in_expr env e2)
        | Ast.ETuple (e1, e2, _) ->
            (match resolve_in_expr env e1 with
             | Some _ as found -> found
             | None -> resolve_in_expr env e2)
        | Ast.ECase (scrutinee, branches, _) ->
            (match resolve_in_expr env scrutinee with
             | Some _ as found -> found
             | None ->
                 List.find_map
                   (fun (pattern, branch_expr, _) ->
                     let bound = pattern_bound_decls pattern in
                     let constructors = pattern_constructor_occurrences pattern in
                     let bound_here =
                       bound
                       |> List.find_map (fun (name, binding_range) ->
                              let binding_target =
                                rename_value_target ~filename:active_filename ~kind:RenameValue name binding_range
                              in
                              if range_contains_position binding_range position then
                                Some (binding_target, binding_range)
                              else
                                None)
                     in
                     match bound_here with
                     | Some _ as found -> found
                     | None ->
                         let constructor_here =
                           constructors
                           |> List.find_map (fun (name, ctor_range) ->
                                  if range_contains_position ctor_range position then
                                    lookup_constructor name ctor_range
                                  else
                                    None)
                         in
                         (match constructor_here with
                          | Some _ as found -> found
                          | None ->
                              let env' =
                                List.fold_left
                                  (fun acc (name, binding_range) ->
                                    StringMap.add
                                      name
                                      (rename_value_target
                                         ~filename:active_filename
                                         ~kind:RenameValue
                                         name
                                         binding_range)
                                      acc)
                                  env
                                  bound
                              in
                              resolve_in_expr env' branch_expr))
                   branches)
        | Ast.EIfe (c, t, e, _) ->
            (match resolve_in_expr env c with
             | Some _ as found -> found
             | None ->
                 match resolve_in_expr env t with
                 | Some _ as found -> found
                 | None -> resolve_in_expr env e)
        | Ast.EAnno (e, _, _) -> resolve_in_expr env e
      in
      let rec resolve_in_tops tops =
        match tops with
        | [] -> None
        | Ast.TopTypeDef (_, _, ctors, top_ann) :: rest ->
            if not (ann_in_file ~filename:active_filename top_ann) then
              resolve_in_tops rest
            else
              let found_ctor =
                ctors
                |> List.find_map (fun (ctor_name, _, _) ->
                       let ctor_range = range_of_name ctor_name in
                       if range_contains_position ctor_range position then
                         lookup_constructor (name_text ctor_name) ctor_range
                       else
                         None)
              in
              (match found_ctor with
               | Some _ as found -> found
               | None -> resolve_in_tops rest)
        | Ast.TopLet (top_name, rhs, ann) :: rest ->
            if not (ann_in_file ~filename:active_filename ann) then
              resolve_in_tops rest
            else
              let name = name_text top_name in
              let name_range = range_of_name top_name in
              let top_target =
                rename_value_target ~filename:active_filename ~kind:RenameValue name name_range
              in
              if range_contains_position name_range position then
                Some (top_target, name_range)
              else
                let env' = StringMap.add name top_target top_env in
                match resolve_in_expr env' rhs with
                | Some _ as found -> found
                | None -> resolve_in_tops rest
      in
      match resolve_in_tops program with
      | None -> None
      | Some (target, selected_range) ->
          let add_if_matching acc candidate_range candidate_target =
            if rename_target_matches candidate_target target then
              candidate_range :: acc
            else
              acc
          in
          let rec collect_expr_ranges (env : rename_target StringMap.t) (acc : range list) (expr : Ast.typed Ast.expr) : range list =
            match expr with
            | Ast.EConst _ | Ast.EError _ -> acc
            | Ast.EVar var_name ->
                let var_range = range_of_name var_name in
                let acc =
                  match StringMap.find_opt (name_text var_name) env with
                  | Some candidate_target -> add_if_matching acc var_range candidate_target
                  | None -> acc
                in
                acc
            | Ast.ECtor (ctor_name, args, _) ->
                let ctor_range = range_of_name ctor_name in
                let acc =
                  match StringMap.find_opt (name_text ctor_name) constructor_env with
                  | Some candidate_target -> add_if_matching acc ctor_range candidate_target
                  | None -> acc
                in
                List.fold_left (collect_expr_ranges env) acc args
            | Ast.ELet (bound_name, e1, e2, _) ->
                let acc = collect_expr_ranges env acc e1 in
                let binding_range = range_of_name bound_name in
                let binding_target =
                  rename_value_target ~filename:active_filename ~kind:RenameValue (name_text bound_name) binding_range
                in
                let acc = add_if_matching acc binding_range binding_target in
                let env' = StringMap.add (name_text bound_name) binding_target env in
                collect_expr_ranges env' acc e2
            | Ast.EFun (params, body, _) ->
                let acc =
                  List.fold_left
                    (fun acc param ->
                      let acc =
                        pattern_constructor_occurrences param
                        |> List.fold_left
                             (fun acc (name, ctor_range) ->
                               match StringMap.find_opt name constructor_env with
                               | Some candidate_target -> add_if_matching acc ctor_range candidate_target
                               | None -> acc)
                             acc
                      in
                      pattern_bound_decls param
                      |> List.fold_left
                           (fun acc (name, binding_range) ->
                             let binding_target =
                               rename_value_target ~filename:active_filename ~kind:RenameValue name binding_range
                             in
                             add_if_matching acc binding_range binding_target)
                           acc)
                    acc
                    params
                in
                let env' =
                  List.fold_left
                    (fun env param ->
                      pattern_bound_decls param
                      |> List.fold_left
                           (fun env (name, binding_range) ->
                             let binding_target =
                               rename_value_target ~filename:active_filename ~kind:RenameValue name binding_range
                             in
                             StringMap.add name binding_target env)
                           env)
                    env
                    params
                in
                collect_expr_ranges env' acc body
            | Ast.EApp (fn, args, _) ->
                let acc = collect_expr_ranges env acc fn in
                List.fold_left (collect_expr_ranges env) acc args
            | Ast.EUnary (_, e, _) -> collect_expr_ranges env acc e
            | Ast.EBinary (_, e1, e2, _) | Ast.ETuple (e1, e2, _) ->
                let acc = collect_expr_ranges env acc e1 in
                collect_expr_ranges env acc e2
            | Ast.ECase (scrutinee, branches, _) ->
                let acc = collect_expr_ranges env acc scrutinee in
                List.fold_left
                  (fun branch_acc (pattern, branch_expr, _) ->
                    let branch_acc =
                      pattern_constructor_occurrences pattern
                      |> List.fold_left
                           (fun ctor_acc (name, ctor_range) ->
                             match StringMap.find_opt name constructor_env with
                             | Some candidate_target -> add_if_matching ctor_acc ctor_range candidate_target
                             | None -> ctor_acc)
                           branch_acc
                    in
                    let env', branch_acc =
                      pattern_bound_decls pattern
                      |> List.fold_left
                           (fun (env_acc, range_acc) (name, binding_range) ->
                             let binding_target =
                               rename_value_target ~filename:active_filename ~kind:RenameValue name binding_range
                             in
                             let range_acc = add_if_matching range_acc binding_range binding_target in
                             (StringMap.add name binding_target env_acc, range_acc))
                           (env, branch_acc)
                    in
                    collect_expr_ranges env' branch_acc branch_expr)
                  acc
                  branches
            | Ast.EIfe (c, t, e, _) ->
                let acc = collect_expr_ranges env acc c in
                let acc = collect_expr_ranges env acc t in
                collect_expr_ranges env acc e
            | Ast.EAnno (e, _, _) -> collect_expr_ranges env acc e
          in
          let edits =
            List.fold_left
              (fun acc (top : Ast.typed Ast.top_expr) ->
                match top with
                | Ast.TopTypeDef (_, _, ctors, top_ann) ->
                    if not (ann_in_file ~filename:active_filename top_ann) then
                      acc
                    else
                      List.fold_left
                        (fun ctor_acc (ctor_name, _, _) ->
                          let ctor_target =
                            rename_value_target
                              ~filename:active_filename
                              ~kind:RenameConstructor
                              (name_text ctor_name)
                              (range_of_name ctor_name)
                          in
                          add_if_matching ctor_acc (range_of_name ctor_name) ctor_target)
                        acc
                        ctors
                | Ast.TopLet (top_name, rhs, ann) ->
                    if not (ann_in_file ~filename:active_filename ann) then
                      acc
                    else
                      let name = name_text top_name in
                      let name_range = range_of_name top_name in
                      let top_target =
                        rename_value_target ~filename:active_filename ~kind:RenameValue name name_range
                      in
                      let acc = add_if_matching acc name_range top_target in
                      let env' = StringMap.add name top_target top_env in
                      collect_expr_ranges env' acc rhs)
              []
              program
            |> List.sort_uniq compare_range_position
          in
          Some { range = selected_range; filename = active_filename; edits; kind = target.kind }

let hover_at_position ~(uri : string) ~(filename : string option) ~(text : string) ~(position : position) : hover_info option =
  let active_filename = file_name ~uri ~filename in
  match parse_with_filename ~filename:active_filename text with
  | Error _ -> None
  | Ok { typed_program = program; _ } ->
      let doc_index = doc_index_for_document ~filename:active_filename text in
      let top_level_function_types =
        program
        |> List.fold_left
             (fun env (top : Ast.typed Ast.top_expr) ->
               match top with
               | Ast.TopLet (top_name, rhs, _) when symbol_kind_of_expr rhs = Function ->
                   StringMap.add (name_text top_name) (typ_of_ann_opt (snd top_name)) env
               | Ast.TopLet _ | Ast.TopTypeDef _ -> env)
             StringMap.empty
      in
      let top_level_docs =
        program
        |> List.fold_left
             (fun env (top : Ast.typed Ast.top_expr) ->
               match top with
               | Ast.TopLet (top_name, _, _) ->
                   (match documentation_for_name doc_index top_name with
                    | Some doc -> StringMap.add (name_text top_name) doc env
                    | None -> env)
               | Ast.TopTypeDef _ -> env)
             StringMap.empty
      in
      let top_level_hover =
        List.find_map
          (fun (top : Ast.typed Ast.top_expr) ->
            match top with
            | Ast.TopLet (top_name, _, top_ann) ->
                if not (ann_in_file ~filename:active_filename top_ann) then
                  None
                else
                  let name_range = range_of_name top_name in
                  if range_contains_position name_range position then
                    let documentation = documentation_for_name doc_index top_name in
                    Some { range = name_range; contents = doc_info_block documentation ^ type_info_block_of_ann top_ann }
                  else
                    None
            | Ast.TopTypeDef (_, _, _, top_ann) as top_def ->
                if not (ann_in_file ~filename:active_filename top_ann) then
                  None
                else
                  let top_range = range_of_ann top_ann in
                  if range_contains_position top_range position then
                    let documentation =
                      match top_def with
                      | Ast.TopTypeDef (type_name, _, _, _) -> documentation_for_name doc_index type_name
                      | Ast.TopLet _ -> None
                    in
                    Some { range = top_range; contents = doc_info_block documentation ^ top_level_type_definition_text top_def }
                  else
                    None)
          program
      in
      match top_level_hover with
      | Some _ as hover -> hover
      | None ->
          let rec find_symbol_hover_in_expr : type s. StringSet.t -> s Ast.expr -> hover_info option =
            fun local_names expr ->
              match expr with
              | Ast.EConst _ | Ast.EError _ -> None
              | Ast.EVar var_name ->
                  let var_range = range_of_name var_name in
                  if range_contains_position var_range position then
                    let var_text = name_text var_name in
                    let definition_type =
                      if StringSet.mem var_text local_names then
                        None
                      else
                        match StringMap.find_opt var_text top_level_function_types with
                        | Some definition_type -> definition_type
                        | None -> None
                    in
                    let documentation =
                      if StringSet.mem var_text local_names then
                        None
                      else
                        StringMap.find_opt var_text top_level_docs
                    in
                    Some
                      {
                        range = var_range;
                        contents =
                          hover_text_for_named_symbol_with_definition_type
                            ?documentation
                            ~definition_type
                            var_name;
                      }
                  else
                    None
              | Ast.ECtor (ctor_name, args, _) ->
                  let ctor_range = range_of_name ctor_name in
                  if range_contains_position ctor_range position then
                    Some { range = ctor_range; contents = hover_text_for_named_symbol ctor_name }
                  else
                    List.find_map (find_symbol_hover_in_expr local_names) args
              | Ast.ELet (bound_name, e1, e2, _) ->
                  let binding_range = range_of_name bound_name in
                  if range_contains_position binding_range position then
                    Some { range = binding_range; contents = hover_text_for_named_symbol bound_name }
                  else
                    (match find_symbol_hover_in_expr local_names e1 with
                     | Some _ as hover -> hover
                     | None ->
                         find_symbol_hover_in_expr
                           (StringSet.add (name_text bound_name) local_names)
                           e2)
              | Ast.EFun (params, body, _) ->
                  let hover_for_param = function
                    | Ast.PVar (name, ann) ->
                        let param_range = range_of_ann ann in
                        if range_contains_position param_range position then
                          Some { range = param_range; contents = hover_text_for_named_symbol (name, ann) }
                        else
                          None
                    | param -> pattern_hover_at_position ~position param
                  in
                  (match List.find_map hover_for_param params with
                   | Some _ as hover -> hover
                   | None ->
                       let local_names' =
                         params
                         |> List.fold_left
                              (fun acc param ->
                                pattern_bound_names param
                                |> List.fold_left (fun acc (name, _) -> StringSet.add name acc) acc)
                              local_names
                       in
                       find_symbol_hover_in_expr local_names' body)
              | Ast.EApp (fn, args, _) ->
                  (match find_symbol_hover_in_expr local_names fn with
                   | Some _ as hover -> hover
                   | None -> List.find_map (find_symbol_hover_in_expr local_names) args)
              | Ast.EUnary (_, e, _) -> find_symbol_hover_in_expr local_names e
              | Ast.EBinary (_, e1, e2, _) ->
                  (match find_symbol_hover_in_expr local_names e1 with
                   | Some _ as hover -> hover
                   | None -> find_symbol_hover_in_expr local_names e2)
              | Ast.ETuple (e1, e2, _) ->
                  (match find_symbol_hover_in_expr local_names e1 with
                   | Some _ as hover -> hover
                   | None -> find_symbol_hover_in_expr local_names e2)
              | Ast.ECase (scrutinee, branches, _) ->
                  (match find_symbol_hover_in_expr local_names scrutinee with
                   | Some _ as hover -> hover
                   | None ->
                       List.find_map
                         (fun (pattern, branch_expr, _) ->
                           let pattern_hover = pattern_hover_at_position ~position pattern in
                           match pattern_hover with
                           | Some _ as hover -> hover
                           | None ->
                               let local_names' =
                                 pattern_bound_names pattern
                                 |> List.fold_left
                                      (fun acc (name, _) -> StringSet.add name acc)
                                      local_names
                               in
                               find_symbol_hover_in_expr local_names' branch_expr)
                         branches)
              | Ast.EIfe (c, t, e, _) ->
                  (match find_symbol_hover_in_expr local_names c with
                   | Some _ as hover -> hover
                   | None ->
                       match find_symbol_hover_in_expr local_names t with
                       | Some _ as hover -> hover
                       | None -> find_symbol_hover_in_expr local_names e)
              | Ast.EAnno (e, _, _) -> find_symbol_hover_in_expr local_names e
          in
          let symbol_hover =
            List.find_map
              (fun (top : Ast.typed Ast.top_expr) ->
                match top with
                | Ast.TopLet (_, rhs, top_ann) ->
                    if ann_in_file ~filename:active_filename top_ann then
                      find_symbol_hover_in_expr StringSet.empty rhs
                    else
                      None
                | Ast.TopTypeDef _ -> None)
              program
          in
          match symbol_hover with
          | Some _ as hover -> hover
          | None ->
              collect_expr_ranges ~filename:active_filename program
              |> List.filter (fun (_, range) -> range_contains_position range position)
              |> List.sort (fun (_, r1) (_, r2) -> compare (range_span_score r1) (range_span_score r2))
              |> first_opt
              |> Option.map (fun (expr, range) -> { range; contents = hover_text_for_expr expr })

let completion_symbol_of_name : type s.
    ?documentation:string -> source:completion_source -> kind:semantic_token_kind -> s Ast.name -> completion_symbol =
  fun ?documentation ~source ~kind name ->
    {
      kind;
      range = range_of_name name;
      detail = detail_of_name name;
      documentation;
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
      StringMap.add name { kind } env)
    StringMap.empty
    Rizzo_builtins.public_builtins

let builtin_completion_symbols : completion_symbol StringMap.t =
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
          documentation = None;
          source = CompletionBuiltin;
        }
        env)
    StringMap.empty
      Rizzo_builtins.public_builtins

let constructor_completion_symbol_of_definition
    ~(type_definitions : Type_env.typedefinition_env)
    ({ arg_types; result_typ; _ } : Type_env.constructor_defintion) : completion_symbol =
  let result_type =
    match StringMap.find_opt result_typ type_definitions.types with
    | Some type_definition ->
        if type_definition.type_params = [] then
          Ast.TName result_typ
        else
          Ast.TApp
            (Ast.TName result_typ, List.map (fun type_param -> Ast.TParam type_param) type_definition.type_params)
    | None -> Ast.TName result_typ
  in
  let typ =
    match arg_types with
    | [] -> result_type
    | first_arg :: rest_args -> Ast.TFun (Ast.Cons1 (first_arg, rest_args), result_type)
  in
  {
    kind = SemanticType;
    range = empty_range;
    detail = Some (string_of_typ typ);
    documentation = None;
    source = CompletionConstructor;
  }

let constructor_completion_symbols_of_typedefinitions
    ~(type_definitions : Type_env.typedefinition_env) : completion_symbol StringMap.t =
  StringMap.fold
    (fun _ (constructor_definition : Type_env.constructor_defintion) env ->
      completion_add_prefer_high_priority
        constructor_definition.name
        (constructor_completion_symbol_of_definition ~type_definitions constructor_definition)
        env)
    type_definitions.constructors
    StringMap.empty

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
             documentation = symbol.documentation;
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
  let active_filename = file_name ~uri ~filename in
  match parse_with_filename ~filename:active_filename text with
  | Error _ -> completion_empty_list
  | Ok { typed_program = program; type_definitions; _ } ->
      let doc_index = doc_index_for_document ~filename:active_filename text in
      let base_env =
        let with_constructors =
          StringMap.fold
            completion_add_prefer_high_priority
            (constructor_completion_symbols_of_typedefinitions ~type_definitions)
            builtin_completion_symbols
        in
        List.fold_left
          (fun env (top : Ast.typed Ast.top_expr) ->
            match top with
            | Ast.TopLet (top_name, rhs, _) ->
                let kind = semantic_kind_of_symbol_kind (symbol_kind_of_expr rhs) in
                let documentation = documentation_for_name doc_index top_name in
                completion_add_prefer_high_priority
                  (name_text top_name)
                  (completion_symbol_of_name ?documentation ~source:CompletionTopLevel ~kind top_name)
                  env
            | Ast.TopTypeDef _ -> env)
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
          | Ast.EConst _ | Ast.EError _ | Ast.EVar _ -> Some env
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
              if List.exists (fun param -> Option.is_some (pattern_hover_at_position ~position param)) params then
                Some env
              else
                let env' =
                  List.fold_left
                    (fun acc param ->
                      pattern_bound_decls param
                      |> List.fold_left
                           (fun acc (name, binding_range) ->
                             completion_add_shadowing
                               name
                               {
                                 kind = SemanticVariable;
                                 range = binding_range;
                                 detail = None;
                                 documentation = None;
                                 source = CompletionLocal;
                               }
                               acc)
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
            if not (ann_in_file ~filename:active_filename top_ann) then
              env_in_tops rest
            else if range_contains_position (range_of_ann top_ann) position then
              if range_contains_position (range_of_name top_name) position then
                Some base_env
              else
                (match env_in_expr base_env rhs with
                 | Some _ as found -> found
                 | None -> Some base_env)
            else
              env_in_tops rest
        | Ast.TopTypeDef _ :: rest -> env_in_tops rest
      in
      let env =
        match env_in_tops program with
        | Some scope_env -> scope_env
        | None -> base_env
      in
      completion_items_of_env ~text ~position env

let semantic_tokens ~(uri : string) ~(filename : string option) ~(text : string) : semantic_token list =
  let active_filename = file_name ~uri ~filename in
  match parse_with_filename ~filename:active_filename text with
  | Error _ -> []
  | Ok { typed_program = program; _ } ->
      let top_decls = top_level_declarations ~filename:None program in
      let visible_top_decls = top_level_declarations ~filename:(Some active_filename) program in
      let tokens : semantic_token list ref = ref [] in
      let push_token ~(kind : semantic_token_kind) ~(range : range) =
        tokens := { range; kind; declaration = false } :: !tokens
      in
      let top_env =
        visible_top_decls
        |> List.fold_left
             (fun env (name, kind, _decl_filename, _top_range, selection_range) ->
               let semantic_kind = semantic_kind_of_symbol_kind kind in
               push_token ~kind:semantic_kind ~range:selection_range;
               StringMap.add name { kind = semantic_kind } env)
             (top_decls
              |> List.fold_left
                   (fun env (name, kind, _decl_filename, _top_range, _selection_range) ->
                     let semantic_kind = semantic_kind_of_symbol_kind kind in
                     StringMap.add name { kind = semantic_kind } env)
                   builtin_scoped_symbols)
      in
      let rec walk_expr (env : scoped_symbol StringMap.t) (expr : Ast.typed Ast.expr) : unit =
        (match expr with
         | Ast.EConst _ | Ast.EError _ -> ()
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
             let env' = StringMap.add (name_text bound_name) { kind = SemanticVariable } env in
             walk_expr env' e2
         | Ast.EFun (params, body, _) ->
             List.iter
               (fun param ->
                 pattern_constructor_occurrences param
                 |> List.iter (fun (_, ctor_range) -> push_token ~kind:SemanticType ~range:ctor_range))
               params;
             let env' =
               List.fold_left
                 (fun env param ->
                   pattern_bound_decls param
                   |> List.fold_left
                        (fun env (name, binding_range) ->
                          push_token ~kind:SemanticVariable ~range:binding_range;
                          StringMap.add name { kind = SemanticVariable } env)
                        env)
                 env
                 params
             in
             walk_expr env' body
         | Ast.EApp (fn, args, _) ->
             walk_expr env fn;
             List.iter (walk_expr env) args
         | Ast.EUnary (op, e, _) ->
             (match unary_keyword_name op with
              | Some name ->
                  (match keyword_range_from_expr ~text ~name expr with
                   | Some keyword_range -> push_token ~kind:SemanticFunction ~range:keyword_range
                   | None -> ())
              | None -> ());
             walk_expr env e
         | Ast.EBinary (op, e1, e2, _) ->
             (match binary_keyword_name op with
              | Some name ->
                  (match keyword_range_from_expr ~text ~name expr with
                   | Some keyword_range -> push_token ~kind:SemanticFunction ~range:keyword_range
                   | None -> ())
              | None -> ());
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
                       StringMap.add name { kind = SemanticVariable } acc)
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
          | Ast.TopTypeDef _ -> ()
          | Ast.TopLet (top_name, rhs, ann) ->
              if ann_in_file ~filename:active_filename ann then
                let name = name_text top_name in
                let kind = semantic_kind_of_symbol_kind (symbol_kind_of_expr rhs) in
                let env' = StringMap.add name { kind } top_env in
                walk_expr env' rhs
              else
                ())
        program;
      !tokens
      |> List.filter (fun (token : semantic_token) -> valid_single_line_range token.range)
      |> List.sort (fun (left : semantic_token) (right : semantic_token) -> compare_range_position left.range right.range)
