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

let reset_reuse_tests = [
	"Swap function with case", `Quick, test_swap_case;
]
