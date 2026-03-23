open Rizzoc
open Rizzoc.Ast

let parse_and_typecheck input =
  let parsed = Parser.parse_string input in
  let typed, errors = typecheck parsed in
  Alcotest.(check int) "type errors" 0 (List.length errors);
  typed

let rec find_typed_let_binding target (e : typed expr) : (typ * typ * typed expr * typed expr) option =
  match e with
  | ELet ((name, name_ann), rhs, body, _) ->
    let bind_t =
      match name_ann with
      | Ann_typed (_, t) -> t
      | Ann_parsed _ | Ann_bound _ -> ann_get_type (expr_get_ann rhs)
    in
    let rhs_t = ann_get_type (expr_get_ann rhs) in
    if String.equal name target then Some (bind_t, rhs_t, rhs, body)
      else
    (match find_typed_let_binding target rhs with
        | Some _ as found -> found
    | None -> find_typed_let_binding target body)
  | EConst _ | EVar _ -> None
  | ECtor (_, args, _) ->
    List.find_map (find_typed_let_binding target) args
  | EFun (_, body, _) -> find_typed_let_binding target body
  | EApp (fn, args, _) ->
    (match find_typed_let_binding target fn with
      | Some _ as found -> found
    | None -> List.find_map (find_typed_let_binding target) args)
  | EUnary (_, e, _) -> find_typed_let_binding target e
  | EBinary (_, e1, e2, _) ->
    (match find_typed_let_binding target e1 with
      | Some _ as found -> found
    | None -> find_typed_let_binding target e2)
  | ETuple (e1, e2, _) ->
    (match find_typed_let_binding target e1 with
      | Some _ as found -> found
    | None -> find_typed_let_binding target e2)
  | ECase (scrutinee, branches, _) ->
    (match find_typed_let_binding target scrutinee with
      | Some _ as found -> found
    | None -> List.find_map (fun (_, body, _) -> find_typed_let_binding target body) branches)
  | EIfe (c, t, e, _) ->
    (match find_typed_let_binding target c with
      | Some _ as found -> found
      | None ->
      (match find_typed_let_binding target t with
          | Some _ as found -> found
      | None -> find_typed_let_binding target e))
  | EAnno (e, _, _) -> find_typed_let_binding target e

let is_sync_signal_to_signal = function
  | TFun (Cons1 (TSync (TSignal _, TSignal _), []), TSignal _) -> true
  | _ -> false

let is_signal = function
  | TSignal _ -> true
  | _ -> false

let function_body (e : typed expr) =
  match e with
  | EFun (_, body, _) -> Some body
  | EAnno (EFun (_, body, _), _, _) -> Some body
  | _ -> None

let test_map2_annotated_cont_and_x_stay_signal () =
  let typed =
    parse_and_typecheck
      (
      "fun map2 f xs ys : ('a -> 'b -> 'c) -> Signal 'a -> Signal 'b -> Signal 'c =\n"
      ^ "    let cont = fun mysync -> match mysync with\n"
      ^ "        | Left (xs_)      ->\n"
      ^ "            let x = map2 f xs_ ys in x\n"
      ^ "        | Right (ys_)     -> map2 f xs ys_\n"
      ^ "        | Both (xs_, ys_) -> map2 f xs_ ys_\n"
      ^ "    in\n"
      ^ "    let id = fun x -> x in\n"
      ^ "    f (id head xs) (id head ys) :: (cont |> sync (tail xs) (tail ys))\n"
      )
  in
  match typed with
  | [TopLet (_, map2_expr, Ann_typed (_, map2_t))] ->
      let map2_return_t =
        match map2_t with
        | TFun (_, ret) -> ret
        | _ -> Alcotest.fail "map2 should infer as a function"
      in
      (match function_body map2_expr with
      | Some body ->
            (match find_typed_let_binding "cont" body with
            | Some (cont_bind_t, cont_rhs_t, cont_rhs, _) ->
              Alcotest.(check bool) "cont binder has Sync(Signal _, Signal _) -> Signal _" true (is_sync_signal_to_signal cont_bind_t);
              Alcotest.(check bool) "cont rhs has Sync(Signal _, Signal _) -> Signal _" true (is_sync_signal_to_signal cont_rhs_t);
              Alcotest.(check bool) "cont binder type equals rhs type" true (Ast.eq_typ cont_bind_t cont_rhs_t);
              (match find_typed_let_binding "x" cont_rhs with
              | Some (x_bind_t, x_rhs_t, _, _) ->
                Alcotest.(check bool) "x binder is a Signal" true (is_signal x_bind_t);
                Alcotest.(check bool) "x rhs is a Signal" true (is_signal x_rhs_t);
                Alcotest.(check bool) "x binder type equals rhs type" true (Ast.eq_typ x_bind_t x_rhs_t);
                Alcotest.(check bool) "x binder type equals map2 return type" true (Ast.eq_typ x_bind_t map2_return_t)
              | None -> Alcotest.fail "expected let x binding in cont")
          | None -> Alcotest.fail "expected let cont binding in map2")
      | None -> Alcotest.fail "unexpected typed AST for map2")
  | _ -> Alcotest.fail "unexpected typed AST shape for map2"

let test_map2_no_annotation_cont_and_x_stay_signal () =
  let typed =
    parse_and_typecheck
      (
      "fun map2_no_annotation f xs ys =\n"
      ^ "    let cont = fun mysync -> match mysync with\n"
      ^ "        | Left (xs_)      ->\n"
      ^ "            let x = map2_no_annotation f xs_ ys in x\n"
      ^ "        | Right (ys_)     -> map2_no_annotation f xs ys_\n"
      ^ "        | Both (xs_, ys_) -> map2_no_annotation f xs_ ys_\n"
      ^ "    in\n"
      ^ "    let id = fun x -> x in\n"
      ^ "    f (id head xs) (id head ys) :: (cont |> sync (tail xs) (tail ys))\n"
      )
  in
  match typed with
  | [TopLet (_, map2_expr, Ann_typed (_, map2_t))] ->
      let map2_return_t =
        match map2_t with
        | TFun (_, ret) -> ret
        | _ -> Alcotest.fail "map2_no_annotation should infer as a function"
      in
      (match function_body map2_expr with
      | Some body ->
            (match find_typed_let_binding "cont" body with
            | Some (cont_bind_t, cont_rhs_t, cont_rhs, _) ->
              Alcotest.(check bool) "cont binder has Sync(Signal _, Signal _) -> Signal _" true (is_sync_signal_to_signal cont_bind_t);
              Alcotest.(check bool) "cont rhs has Sync(Signal _, Signal _) -> Signal _" true (is_sync_signal_to_signal cont_rhs_t);
              Alcotest.(check bool) "cont binder type equals rhs type" true (Ast.eq_typ cont_bind_t cont_rhs_t);
              (match find_typed_let_binding "x" cont_rhs with
              | Some (x_bind_t, x_rhs_t, _, _) ->
                Alcotest.(check bool) "x binder is a Signal" true (is_signal x_bind_t);
                Alcotest.(check bool) "x rhs is a Signal" true (is_signal x_rhs_t);
                Alcotest.(check bool) "x binder type equals rhs type" true (Ast.eq_typ x_bind_t x_rhs_t);
                Alcotest.(check bool) "x binder type equals map2_no_annotation return type" true (Ast.eq_typ x_bind_t map2_return_t)
              | None -> Alcotest.fail "expected let x binding in cont")
          | None -> Alcotest.fail "expected let cont binding in map2_no_annotation")
      | None -> Alcotest.fail "unexpected typed AST for map2_no_annotation")
  | _ -> Alcotest.fail "unexpected typed AST shape for map2_no_annotation"

let map2_tests = [
  "map2 annotated keeps cont/x as Signal", `Quick, test_map2_annotated_cont_and_x_stay_signal;
  "map2 no-annotation keeps cont/x as Signal", `Quick, test_map2_no_annotation_cont_and_x_stay_signal;
]