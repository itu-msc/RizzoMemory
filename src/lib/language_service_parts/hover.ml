open Collections
open Types
open Document
open Parse
open Docs
open Symbols

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
                    Some { range = name_range; contents = type_info_block_of_ann top_ann ^ doc_info_block documentation }
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
                    Some { range = top_range; contents = top_level_type_definition_text top_def ^ doc_info_block documentation }
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
