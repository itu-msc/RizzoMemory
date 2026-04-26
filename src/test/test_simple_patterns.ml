open Rizzoc.Ast
open Rizzoc.Transformations
open Ast_test_helpers

let rec count_let_bindings target = function
  | ELet ((name, _), rhs, body, _) ->
      (if String.equal name target then 1 else 0)
      + count_let_bindings target rhs
      + count_let_bindings target body
  | EConst _ | EVar _ -> 0
  | ECtor (_, args, _) -> List.fold_left (fun acc e -> acc + count_let_bindings target e) 0 args
  | EApp (fn, args, _) ->
      count_let_bindings target fn
      + List.fold_left (fun acc e -> acc + count_let_bindings target e) 0 args
  | EBinary (_, e1, e2, _) ->
    count_let_bindings target e1 + count_let_bindings target e2
  | ETuple (e1, e2, es, _) ->
    let first_two = count_let_bindings target e1 + count_let_bindings target e2 in
    List.fold_left (fun acc e -> acc + count_let_bindings target e) first_two es
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

let extract_single_case_branch_body = function
  | [TopLet (_, EFun (_, ECase (_, [(_, body, _)], _), _), _)] -> body
  | _ -> Alcotest.fail "unexpected transformed AST shape"

let test_sigcons_tail_bound_once_in_else_branch () =
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

let test_sigcons_tail_in_let_rhs_is_bound_once () =
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

let simple_pattern_tests =
  [
    ( "signal tail is bound once in else",
      `Quick,
      test_sigcons_tail_bound_once_in_else_branch );
    ( "signal tail in let-rhs is bound once",
      `Quick,
      test_sigcons_tail_in_let_rhs_is_bound_once );
    ( "unused signal tail is not bound",
      `Quick,
      test_sigcons_tail_unused_is_not_bound );
  ]