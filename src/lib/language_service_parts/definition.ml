open Collections
open Types
open Document
open Parse
open Symbols

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
