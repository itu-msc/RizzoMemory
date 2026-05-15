open Types
open Document
open Docs

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
  | Ast.ECtor (_, args, _) -> List.iter (iter_expr f) args
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
      Format.asprintf "```rizz\n%s\n```" type_text

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
    Format.asprintf "```rizz\n%s\n```" top_text

let hover_text_for_named_symbol : type s. ?documentation:string -> s Ast.name -> string =
  fun ?documentation (_, ann) ->
    type_info_block_of_ann ann ^ doc_info_block documentation

let hover_text_for_named_symbol_with_definition_type : type s.
    ?documentation:string -> definition_type:Ast.typ option -> s Ast.name -> string =
  fun ?documentation ~definition_type (_, ann) ->
    type_info_block_of_ann ann
    ^ distinct_definition_type_block (typ_of_ann_opt ann) definition_type
    ^ doc_info_block documentation

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
