open Collections
open Types
open Document
open Parse
open Symbols

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
        | Some target when String.equal target.filename active_filename -> Some (target, range)
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
                        List.fold_left (fun acc param -> 
                            pattern_bound_decls param 
                            |> List.fold_left (fun acc (name, param_range) ->
                                StringMap.add
                                  name
                                  (rename_value_target ~filename:active_filename ~kind:RenameValue name param_range)
                                  acc
                                ) acc
                          ) env params
                      in
                      resolve_in_expr env' body
                 ))
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
