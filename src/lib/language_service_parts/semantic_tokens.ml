open Collections
open Types
open Document
open Parse
open Symbols
open Completion

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
