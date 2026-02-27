open Rizzoc
open Rizzoc.Ast
open Rizzoc.Transformations
open Ast_test_helpers



module Utils = Utilities

let test_lift_preserves_top_level_order () =
	Utils.new_name_reset ();

	let p : parsed program = [
		toplet "a" (int 1);
		toplet "b" (int 2);
	] in

	let transformed = lift p in

	Alcotest.check program_testable "top-level order preserved" p transformed

let test_lift_free_vars_exclude_params_and_outer_locals () =
	Utils.new_name_reset ();

	(* let f = fun x -> fun y -> x *)
	let p : parsed program = [
		toplet "f" (fun_ ["x"] (fun_ ["y"] (var "x")));
	] in

	let transformed = lift p in

	Utils.new_name_reset ();
	let inner_name = Utils.new_name "lifted_fun" in

	(*TODO: should we differentiate TopLet and TFun?
		fun lifted_fun2 x y -> x 
		let f = fun x -> lifted_fun1(x)
	*)
	let expected : parsed program = [
		toplet inner_name (fun_ ["x"; "y"] (var "x"));
		toplet "f" (fun_ ["x"] (app (var inner_name) [var "x"]))
		(* TopLet (outer_name, EFun (["x"], EApp (EVar inner_name, [EVar "x"]))); *)
		(* TopLet ("f", EVar outer_name); *)
	] in

	Alcotest.check program_testable "free vars exclude params/outer locals" expected transformed

let test_lift_deduplicates_free_vars () =
	Utils.new_name_reset ();

	(* let f = fun z -> fun y -> (x,x)*)
	let p : parsed program = [
		toplet "f" (fun_ ["z"] (fun_ ["y"] (tuple (var "x") (var "x"))));
	] in

	let transformed = lift p in

	Utils.new_name_reset ();
	let lifted = Utils.new_name "lifted_fun" in
	(* let f = fun z -> lifted_fun1(x) - looks weird but we are not type checking? *)
	let expected : parsed program = [
		toplet lifted (fun_ ["x"; "y"] (tuple (var "x") (var "x")));
		toplet "f" (fun_ ["z"] (app (var lifted) [var "x"]));
	] in

	Alcotest.check program_testable "free vars are deduplicated" expected transformed

let lift_tests = [
	"top-level order", `Quick, test_lift_preserves_top_level_order;
	"free vars", `Quick, test_lift_free_vars_exclude_params_and_outer_locals;
	"dedup free vars", `Quick, test_lift_deduplicates_free_vars;
]
