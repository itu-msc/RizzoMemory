open! Rizzoc.RefCount
open Ast_test_helpers
open Rizzoc.Collections

let tagged_arm tag num_fields body =
  { tag = Some tag; num_fields = Some num_fields; body }

let swap : rc_fun = 
  (* xs: cons(hd, tl)*)
  Fun(["xs"],  FnCase ("xs", [
    tagged_arm 0 0 (FnRet (Var "xs"));
    tagged_arm 2 2 (
      FnLet ("t1", RProj (2, "xs"),
      FnCase ("t1", [
        tagged_arm 0 0 (FnRet (Var "xs"));
        tagged_arm 2 2 (
          FnLet ("h1", RProj (1, "xs"),
          FnLet ("h2", RProj (1, "t1"),
          FnLet ("t2", RProj (2, "t1"),
          FnLet ("r1", RCtor (Ctor { tag = 2; fields = [Var "h1"; Var "t2"]}),
          FnLet ("r2", RCtor (Ctor { tag = 2; fields = [Var "h2"; Var "r1"]}),
          FnRet (Var "r2")))))))
      ]))
    )
  ]))


let test_swap_case () =
  Rizzoc.Utilities.new_name_reset ();

  let Fun(_, e) = swap in
  let e' = Rizzoc.RefCount.insert_reset_and_reuse_pairs_fn e in

  let expected_nested_swap_branch =
    FnLet ("h1", RProj (1, "xs"),
    FnLet ("var2", RReset "xs",
    FnLet ("h2", RProj (1, "t1"),
    FnLet ("t2", RProj (2, "t1"),
    FnLet ("var1", RReset "t1",
    FnLet ("r1", RReuse ("var1", Ctor { tag = 2; fields = [Var "h1"; Var "t2"]}),
    FnLet ("r2", RReuse ("var2", Ctor { tag = 2; fields = [Var "h2"; Var "r1"]}),
    FnRet (Var "r2"))))))))
  in

  let Fun (_, expeceted_reset_reuse_swap) = (* xs: cons(hd, tl)*)
    Fun(["xs"],  FnCase ("xs", [
      tagged_arm 0 0 (FnRet (Var "xs"));
      tagged_arm 2 2 (
        FnLet ("t1", RProj (2, "xs"),
        FnCase ("t1", [
          tagged_arm 0 0 (FnRet (Var "xs"));
          tagged_arm 2 2 expected_nested_swap_branch
        ]))
      )
    ]))
  in

  (* let count = !Rizzoc.Utilities.new_name_cnt in
  Alcotest.(check int) "new_name count is 2" 2 count; *)

  (* should not crash and should not change the function *)
  Alcotest.(check fnbody_testable) "swap case is preserved" expeceted_reset_reuse_swap e'

let test_paper_goforward () = 
  Rizzoc.Utilities.new_name_reset ();

  let go_forward_inner_branch =
    FnLet ("bs", RProj (2, "p"),
      FnLet ("x", RProj (1, "xs"),
      FnLet ("xs'", RProj (2, "xs"),
      FnLet ("bs'", RCtor (Ctor {tag = 2; fields = [Var "x"; Var "bs"]}),
      FnLet ("r", RCtor (Ctor {tag = 1; fields = [Var "xs'"; Var "bs'"]}),
      FnRet (Var "r"))))))
  in
  
  let go_forward = Fun(["p"], 
    FnCase ("p", [tagged_arm 2 2 (
      FnLet ("xs", RProj (1, "p"),
      (* FnInc("xs",  *)
      FnCase("xs", [
        tagged_arm 0 0 (FnRet (Var "p"));
        tagged_arm 2 2 go_forward_inner_branch
      ])
      )
    )])
  )
  in

  let p_own = StringMap.empty in
  let prog = RefProg { functions = [("go_forward", go_forward)]; globals = []} in
  let newOwned, go_forward' = Rizzoc.RefCount.reference_count_program p_own StringMap.empty  prog in

  (* ensure p is owned *)
  let p_ownership = StringMap.find "go_forward" newOwned |> List.hd in
  Alcotest.(check ownership_testable) "p is owned" Owned p_ownership;

  let go_forward_expected_inner_branch =
    FnLet ("bs", RProj (2, "p"),
      FnInc ("bs",
      FnLet ("var2", RReset "p",
      FnLet ("x", RProj (1, "xs"),
      FnInc ("x",
      FnLet ("xs'", RProj (2, "xs"),
      FnInc ("xs'",
      FnLet ("var1", RReset "xs",
      FnLet ("bs'", RReuse ("var1", Ctor {tag = 2; fields = [Var "x"; Var "bs"]}),
      FnLet ("r", RReuse ("var2", Ctor {tag = 1; fields = [Var "xs'"; Var "bs'"]}),
        FnRet (Var "r")))))))))))
  in

  let go_forward_expected = [("go_forward", Fun(["p"],
    FnCase ("p", [tagged_arm 2 2 (
      FnLet ("xs", RProj (1, "p"),
      FnInc("xs",
      FnCase("xs", [
        tagged_arm 0 0 (FnDec ("xs", FnRet (Var "p"))); (* We deviate from the paper here and added a dec *)
        tagged_arm 2 2 go_forward_expected_inner_branch
      ])
      ))
    )])
  ))]
  in
  let go_forward_expected_prog = RefProg { functions = go_forward_expected; globals = []} in

  (* should not crash and should not change the function *)
  Alcotest.(check ref_counted_program_testable) "go forward case is preserved" go_forward_expected_prog go_forward'

let test_tuple_swap () = 
  Rizzoc.Utilities.new_name_reset ();

  let tuple_swap_body =
    FnLet ("a", RProj (1, "t"),
    FnLet ("b", RProj (2, "t"),
    FnLet ("r", RCtor (Ctor { tag = 2; fields = [Var "b"; Var "a"]}),
    FnRet (Var "r"))))
  in
  
  let Fun(_, swap_tuple) = Fun(["t"],
    FnCase ("t", [tagged_arm 2 2 tuple_swap_body])
  ) in 

  let sut = Rizzoc.RefCount.insert_reset_and_reuse_pairs_fn swap_tuple in

  let tuple_swap_expected_body =
    FnLet ("a", RProj (1, "t"),
    FnLet ("b", RProj (2, "t"),
    FnLet ("var1", RReset "t",
    FnLet ("r", RReuse ("var1", Ctor { tag = 2; fields = [Var "b"; Var "a"]}),
    FnRet (Var "r")))))
  in

  let Fun(_, expected) = Fun(["t"],
    FnCase ("t", [tagged_arm 2 2 tuple_swap_expected_body])
  ) in

  Alcotest.(check fnbody_testable) "tuple swap case is preserved" expected sut

let test_globals_are_borrowed () =
  let globals = [
    ("pair",
      FnLet ("pair0", RCtor (Ctor { tag = 2; fields = [Const (Rizzoc.Ast.CInt 1); Const (Rizzoc.Ast.CInt 2)] }),
      FnRet (Var "pair0")));
    ("swapped",
      FnLet ("a", RProj (1, "pair"),
      FnLet ("b", RProj (2, "pair"),
      FnLet ("r", RCtor (Ctor { tag = 2; fields = [Var "b"; Var "a"] }),
      FnRet (Var "r")))))
  ] in

  let prog = RefProg { functions = []; globals } in
  let _, actual = Rizzoc.RefCount.reference_count_program StringMap.empty StringMap.empty  prog in

  let expected = RefProg {
    functions = [];
    globals = [
      ("pair",
        FnLet ("pair0", RCtor (Ctor { tag = 2; fields = [Const (Rizzoc.Ast.CInt 1); Const (Rizzoc.Ast.CInt 2)] }),
        FnRet (Var "pair0")));
      ("swapped",
        FnLet ("a", RProj (1, "pair"),
        FnLet ("b", RProj (2, "pair"),
        FnInc ("b",
        FnInc ("a",
        FnLet ("r", RCtor (Ctor { tag = 2; fields = [Var "b"; Var "a"] }),
        FnRet (Var "r")))))))
    ]
  } in

  Alcotest.(check ref_counted_program_testable) "globals are treated as borrowed values" expected actual

let test_function_can_use_global_without_dec () =
  let globals = [
    ("pair",
      FnLet ("pair0", RCtor (Ctor { tag = 2; fields = [Const (Rizzoc.Ast.CInt 1); Const (Rizzoc.Ast.CInt 2)] }),
      FnRet (Var "pair0")))
  ] in
  let functions = [
    ("swap_global", Fun ([],
      FnLet ("a", RProj (1, "pair"),
      FnLet ("b", RProj (2, "pair"),
      FnLet ("r", RCtor (Ctor { tag = 2; fields = [Var "b"; Var "a"] }),
      FnRet (Var "r"))))))
  ] in

  let prog = RefProg { functions; globals } in
  let _, actual = Rizzoc.RefCount.reference_count_program StringMap.empty StringMap.empty prog in

  let expected = RefProg {
    functions = [
      ("swap_global", Fun ([],
        FnLet ("a", RProj (1, "pair"),
        FnLet ("b", RProj (2, "pair"),
        FnInc ("b",
        FnInc ("a",
        FnLet ("r", RCtor (Ctor { tag = 2; fields = [Var "b"; Var "a"] }),
        FnRet (Var "r"))))))))
    ];
    globals = [
      ("pair",
        FnLet ("pair0", RCtor (Ctor { tag = 2; fields = [Const (Rizzoc.Ast.CInt 1); Const (Rizzoc.Ast.CInt 2)] }),
        FnRet (Var "pair0")))
    ]
  } in

  Alcotest.(check ref_counted_program_testable) "functions can use globals without decrementing them" expected actual

let reset_reuse_tests = [
	"Ullrich & De Moura - Swap function with case", `Quick, test_swap_case;
  "Ullrich & De Moura - go forward example", `Quick, test_paper_goforward;
  "Tuple swap example", `Quick, test_tuple_swap;
  "Globals are treated as borrowed values", `Quick, test_globals_are_borrowed;
  "Functions can use globals", `Quick, test_function_can_use_global_without_dec;
]
