open! Rizzoc.RefCount
open Ast_test_helpers

let swap : fn = 
  (* xs: cons(hd, tl)*)
  Fun(["xs"],  FnCase ("xs", [
    (0, FnRet (Var "xs"));
    (2, FnLet ("t1", RProj (2, "xs"), FnCase ("t1", [
      (0, FnRet (Var "xs"));
      (2, 
      FnLet ("h1", RProj (1, "xs"),
      FnLet("h2", RProj (1, "t1"),
      FnLet("t2", RProj (2, "t1"),
      FnLet("r1", RCtor{ tag = 2; fields = [Var "h1"; Var "t2"]},
      FnLet("r2", RCtor{ tag = 2; fields = [Var "h2"; Var "r1"]},
      FnRet (Var "r2")))
      ))))
    ])))
  ]))


let test_swap_case () =
  Rizzoc.Utilities.new_name_reset ();

  let Fun(_, e) = swap in
  let e' = Rizzoc.RefCount.insert_reset_and_reuse_pairs_fn e in

  let Fun (_, expeceted_reset_reuse_swap) = (* xs: cons(hd, tl)*)
    Fun(["xs"],  FnCase ("xs", [
      (0, FnRet (Var "xs"));
      (2, FnLet ("t1", RProj (2, "xs"), FnCase ("t1", [
        (0, FnRet (Var "xs"));
        (2, 
        FnLet ("h1", RProj (1, "xs"),
          FnLet("var2", RReset "xs",
        FnLet("h2", RProj (1, "t1"),
        FnLet("t2", RProj (2, "t1"),
          FnLet("var1", RReset "t1",
        FnLet("r1", RReuse("var1", { tag = 2; fields = [Var "h1"; Var "t2"]}),
        FnLet("r2", RReuse("var2", { tag = 2; fields = [Var "h2"; Var "r1"]}),
        FnRet (Var "r2")))
        ))))))
      ])))
    ]))
  in

  (* let count = !Rizzoc.Utilities.new_name_cnt in
  Alcotest.(check int) "new_name count is 2" 2 count; *)

  (* should not crash and should not change the function *)
  Alcotest.(check fnbody_testable) "swap case is preserved" expeceted_reset_reuse_swap e'

let test_paper_goforward () = 
  Rizzoc.Utilities.new_name_reset ();
  
  let go_forward = Fun(["p"], 
    FnCase ("p", [(2, 
      FnLet ("xs", RProj (1, "p"),
      (* FnInc("xs",  *)
      FnCase("xs", [
        (0, FnRet (Var "p"));
        (2, FnLet ("bs", RProj (2, "p"),
            (* FnInc("bs",  *)
            FnLet("x", RProj (1, "xs"), 
            (* FnInc("x",  *)
            FnLet("xs'", RProj (2, "xs"), 
            (* FnInc("xs'", *)
            FnLet("bs'", RCtor{tag = 2; fields = [Var "x"; Var "bs"]},
            FnLet("r", RCtor{tag = 1; fields = [Var "xs'"; Var "bs'"]},
            FnRet (Var "r"))))))
        )]
      )))
    ])
  )
  in

  let p_own = StringMap.empty in
  let newOwned, go_forward' = Rizzoc.RefCount.reference_count_program p_own ["go_forward", go_forward] in

  (* ensure p is owned *)
  let p_ownership = StringMap.find "go_forward" newOwned |> List.hd in
  Alcotest.(check ownership_testable) "p is owned" Owned p_ownership;

  let go_forward_expected = [("go_forward", Fun(["p"],
    FnCase ("p", [(2,
      FnLet ("xs", RProj (1, "p"),
      FnInc("xs",
      FnCase("xs", [
        (0, FnDec ("xs", FnRet (Var "p"))); (* We deviate from the paper here and added a dec *)
        (2, FnLet ("bs", RProj (2, "p"),
            FnInc("bs",
              FnLet("var2", RReset "p",
            FnLet("x", RProj (1, "xs"),
            FnInc("x",
            FnLet("xs'", RProj (2, "xs"),
            FnInc("xs'",
              FnLet("var1", RReset "xs",
            FnLet("bs'", RReuse("var1", {tag = 2; fields = [Var "x"; Var "bs"]}),
            FnLet("r", RReuse("var2", {tag = 1; fields = [Var "xs'"; Var "bs'"]}),
            FnRet (Var "r")))))))))))
        )]
      )))
    )])
  ))]
  in

  Alcotest.(check ref_counted_program_testable) "go forward case is preserved" go_forward_expected go_forward'
  
let reset_reuse_tests = [
	"Ullrich & De Moura - Swap function with case", `Quick, test_swap_case;
  "Ullrich & De Moura - go forward example", `Quick, test_paper_goforward;
]
