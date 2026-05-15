open Collections
open Types
open Document
open Parse
open Docs
open Symbols

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
