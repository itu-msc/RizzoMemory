open Rizzoc
open Rizzoc.Ast
open Rizzoc.Transformations



module Utils = Utilities

let program_testable = Alcotest.testable Ast.pp_program Ast.eq_program

let test_lift_preserves_top_level_order () =
	Utils.new_name_reset ();

	let p:program = [
		TLet ("a", EConst (CInt 1));
		TLet ("b", EConst (CInt 2));
	] in

	let transformed = lift p in

	Alcotest.check program_testable "top-level order preserved" p transformed

let test_lift_free_vars_exclude_params_and_outer_locals () =
	Utils.new_name_reset ();

	let p:program = [
		TLet ("f", EFun (["x"], EFun (["y"], EVar "x")));
	] in

	let transformed = lift p in

	Utils.new_name_reset ();
	let outer_name = Utils.new_name "lifted_fun" in
	let inner_name = Utils.new_name "lifted_fun" in

	let expected:program = [
		TLet (inner_name, EFun (["x"; "y"], EVar "x"));
		TLet (outer_name, EFun (["x"], EApp (EVar inner_name, [EVar "x"])));
		TLet ("f", EVar outer_name);
	] in

	Alcotest.check program_testable "free vars exclude params/outer locals" expected transformed

let test_lift_deduplicates_free_vars () =
	Utils.new_name_reset ();

	let p:program = [
		TLet ("f", EFun (["y"], ETuple (EVar "x", EVar "x")));
	] in

	let transformed = lift p in

	Utils.new_name_reset ();
	let lifted_name = Utils.new_name "lifted_fun" in

	let expected:program = [
		TLet (lifted_name, EFun (["x"; "y"], ETuple (EVar "x", EVar "x")));
		TLet ("f", EApp (EVar lifted_name, [EVar "x"]));
	] in

	Alcotest.check program_testable "free vars are deduplicated" expected transformed

let lift_tests = [
	"top-level order", `Quick, test_lift_preserves_top_level_order;
	"free vars", `Quick, test_lift_free_vars_exclude_params_and_outer_locals;
	"dedup free vars", `Quick, test_lift_deduplicates_free_vars;
]
