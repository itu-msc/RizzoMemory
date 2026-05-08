open Rizzoc.Ast
open Rizzoc.Transformations
open Ast_test_helpers

let rec count_let_bindings target = function
  | ELet ((name, _), rhs, body, _) ->
      (if String.equal name target then 1 else 0)
      + count_let_bindings target rhs
      + count_let_bindings target body
  | EConst _ | EVar _ | EError _ -> 0
  | ECtor (_, args, _) -> List.fold_left (fun acc e -> acc + count_let_bindings target e) 0 args
  | EApp (fn, args, _) ->
      count_let_bindings target fn
      + List.fold_left (fun acc e -> acc + count_let_bindings target e) 0 args
  | EBinary (_, e1, e2, _) | ETuple (e1, e2, _) ->
      count_let_bindings target e1 + count_let_bindings target e2
  | EUnary (_, e, _) -> count_let_bindings target e
  | EIfe (c, t, e, _) ->
      count_let_bindings target c
      + count_let_bindings target t
      + count_let_bindings target e
  | ECase (scrutinee, branches, _) ->
      count_let_bindings target scrutinee
      + List.fold_left (fun acc (_, branch, _) -> acc + count_let_bindings target branch) 0 branches
  | EFun (_, body, _) -> count_let_bindings target body
  | EAnno (e, _, _) -> count_let_bindings target e
  
let rec count_let_bindings_prefix prefix : _ expr -> int = function
  | ELet ((name, _), rhs, body, _) ->
    let matches_prefix =
    String.length name >= String.length prefix && String.sub name 0 (String.length prefix) = prefix
    in
    (if matches_prefix then 1 else 0)
    + count_let_bindings_prefix prefix rhs
    + count_let_bindings_prefix prefix body
  | EConst _ | EVar _ | EError _ -> 0
  | ECtor (_, args, _) -> List.fold_left (fun acc e -> acc + count_let_bindings_prefix prefix e) 0 args
  | EFun (_, body, _) -> count_let_bindings_prefix prefix body
  | EApp (fn, args, _) ->
    count_let_bindings_prefix prefix fn
    + List.fold_left (fun acc e -> acc + count_let_bindings_prefix prefix e) 0 args
  | EUnary (_, e, _) -> count_let_bindings_prefix prefix e
  | EBinary (_, e1, e2, _) | ETuple (e1, e2, _) ->
    count_let_bindings_prefix prefix e1 + count_let_bindings_prefix prefix e2
  | ECase (scrutinee, branches, _) ->
    count_let_bindings_prefix prefix scrutinee
    + List.fold_left (fun acc (_, body, _) -> acc + count_let_bindings_prefix prefix body) 0 branches
  | EIfe (c, t, e, _) ->
    count_let_bindings_prefix prefix c
    + count_let_bindings_prefix prefix t
    + count_let_bindings_prefix prefix e
  | EAnno (e, _, _) -> count_let_bindings_prefix prefix e

let rec first_ife_branches = function
  | EIfe (_, then_branch, else_branch, _) -> Some (then_branch, else_branch)
  | ELet (_, rhs, body, _) ->
      (match first_ife_branches rhs with
        | Some _ as found -> found
        | None -> first_ife_branches body)
  | ECtor (_, args, _) | EApp (_, args, _) ->
      let rec search = function
        | [] -> None
        | expr :: rest ->
            (match first_ife_branches expr with
              | Some _ as found -> found
              | None -> search rest)
      in
      search args
  | EBinary (_, e1, e2, _) | ETuple (e1, e2, _) ->
      (match first_ife_branches e1 with
        | Some _ as found -> found
        | None -> first_ife_branches e2)
  | EUnary (_, e, _) | EAnno (e, _, _) -> first_ife_branches e
  | EFun (_, body, _) -> first_ife_branches body
  | ECase (scrutinee, branches, _) ->
      (match first_ife_branches scrutinee with
        | Some _ as found -> found
        | None ->
            let rec search = function
              | [] -> None
              | (_, body, _) :: rest ->
                  (match first_ife_branches body with
                  | Some _ as found -> found
                  | None -> search rest)
            in
            search branches)
  | EConst _ | EVar _ | EError _ -> None


let extract_single_case_branch_body = function
  | [TopLet (_, EFun (_, ECase (_, [(_, body, _)], _), _), _)] -> body
  | _ -> Alcotest.fail "unexpected transformed AST shape"

let test_simple_patterns_sigcons_tail_bound_once_in_else_branch () =
  let program =
    [
      toplet "stop"
        (fun_ ["f"; "s"]
           (case (var "s")
              [
                ( psigcons (pvar "x") (name "xs"),
                  ife
                    (app (var "f") [var "x"])
                    (binary SigCons (var "x") (const CNever))
                    (binary SigCons (var "x") (var "xs")) );
              ]));
    ]
  in
  let transformed = eliminate_simple_patterns program in
  let branch = extract_single_case_branch_body transformed in
  Alcotest.(check int) "single xs let binding" 1 (count_let_bindings "xs" branch);
  match branch with
  | ELet (("x", _), _, EIfe (_, then_branch, else_branch, _), _) ->
      Alcotest.(check int) "no xs let in then" 0 (count_let_bindings "xs" then_branch);
      Alcotest.(check int) "xs let in else" 1 (count_let_bindings "xs" else_branch)
  | _ -> Alcotest.fail "expected head binding followed by if-expression"

let test_patterns_tree_sigcons_tail_in_let_rhs_is_bound_once () =
  let program =
    [
      toplet "stop"
        (fun_ ["s"]
           (case (var "s")
              [
                ( psigcons (pvar "x") (name "xs"),
                  let_ "cont" (app (var "lifted") [var "xs"])
                    (binary SigCons (var "x") (app (var "jump") [var "cont"])) );
              ]));
    ]
  in
  let transformed = eliminate_simple_patterns program in
  let branch = extract_single_case_branch_body transformed in
  Alcotest.(check int) "single xs let when used in let-rhs" 1 (count_let_bindings "xs" branch)

let test_sigcons_tail_unused_is_not_bound () =
  let program =
    [
      toplet "stop"
        (fun_ ["s"]
           (case (var "s") [ (psigcons (pvar "x") (name "xs"), var "x") ]));
    ]
  in
  let transformed = eliminate_simple_patterns program in
  let branch = extract_single_case_branch_body transformed in
  Alcotest.(check int) "no xs let when unused" 0 (count_let_bindings "xs" branch)

let test_tree_signal_tail_is_sunk_only_where_used () =
  let open Rizzoc in
  let open Ast_test_helpers in
  let rec first_case_branch_body = function
  | ELet (_, _, body, _) -> first_case_branch_body body
  | ECase (_, (_, body, _) :: _, _) -> body
  | _ -> Alcotest.fail "expected case in transformed body"
  in

  let program =
    [
      toplet "stop"
        (fun_ ["f"; "s"]
           (case (var "s")
              [
                ( psigcons (pvar "x") (name "xs"),
                  ife
                    (app (var "f") [var "x"])
                    (binary SigCons (var "x") (const CNever))
                    (binary SigCons (var "x") (var "xs")) );
              ]));
    ]
  in
  Utilities.new_name_reset ();
  let transformed = Transformations.eliminate_patterns_tree program in
  match transformed with
  | [TopLet (_, EFun (_, body, _), _)] ->
      let branch = first_case_branch_body body in
      Alcotest.(check int) "single xs let binding" 1 (count_let_bindings "xs" branch);
      Alcotest.(check int) "single signal-tail binding" 1 (count_let_bindings_prefix "sig_tail" branch);
      (match first_ife_branches branch with
       | Some (then_branch, else_branch) ->
         Alcotest.(check int) "no xs let in then" 0 (count_let_bindings "xs" then_branch);
         Alcotest.(check int) "xs let in else" 1 (count_let_bindings "xs" else_branch)
       | None -> Alcotest.fail "expected if-expression in branch")
  | _ -> Alcotest.fail "unexpected transformed AST shape"

let test_patterns_tree_flattens_same_scrutinee_cases () =
  let open Rizzoc in
  Utilities.new_name_reset ();
  let program =
    [
      toplet "describe"
        (fun_ ["value"]
           (case (var "value")
              [
                (pctor (name "Some") [pvar "x"], var "x");
                (pctor (name "None") [], int 0);
                (pwild, int 1);
              ]));
    ]
  in
  let transformed = Transformations.eliminate_patterns_tree program in
  match transformed with
  | [TopLet (_, EFun (_, ELet ((scrutinee_name, _), _, ECase (EVar (case_name, _), branches, _), _), _), _)] ->
      Alcotest.(check string) "case uses generated scrutinee" scrutinee_name case_name;
      Alcotest.(check int) "same-scrutinee cases are flattened" 3 (List.length branches);
      (match List.rev branches with
       | (PWildcard _, body, _) :: _ ->
           (match body with
            | ECase (EVar (nested_name, _), _, _) when String.equal nested_name scrutinee_name ->
                Alcotest.fail "default branch should not nest a match on the same scrutinee"
            | _ -> ())
       | _ -> Alcotest.fail "expected a default branch")
  | _ -> Alcotest.fail "unexpected transformed AST shape"

let pattern_matching_tests =
  [
    ( "SIMPLE PATTERNS: signal tail is bound once in else",
      `Quick,
      test_simple_patterns_sigcons_tail_bound_once_in_else_branch );
    ( "PATTERN TREE: signal tail in let-rhs is bound once",
      `Quick,
      test_patterns_tree_sigcons_tail_in_let_rhs_is_bound_once );
    ( "unused signal tail is not bound",
      `Quick,
      test_sigcons_tail_unused_is_not_bound );
    ( "PATTERN TREE: same-scrutinee case fallback is flattened",
      `Quick,
      test_patterns_tree_flattens_same_scrutinee_cases );
  ]
