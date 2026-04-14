(*
	Based on the work:
	- 'How to compile pattern matching' by Jules Jacos 2021, https://julesjacobs.com/notes/patternmatching/patternmatching.pdf
	- and Yorick Peterse 2022, https://gitlab.com/yorickpeterse/pattern-matching-in-rust/-/tree/main/jacobs2021
*)


open! Ast

module StringMap = Map.Make (String)

let string_eq = (Rizzo_builtins.get "string_eq").name
let head_elim = (Rizzo_builtins.get "head").name
let string_is_empty = (Rizzo_builtins.get "string_is_empty").name
let string_head = (Rizzo_builtins.get "string_head").name
let string_tail = (Rizzo_builtins.get "string_tail").name
let match_fail_name = (Rizzo_builtins.get "match_fail").name

let var ((name, ann) : _ name) = EVar (name, ann)

let app ((name, ann) : _ name) args = EApp (var (name, ann), args, ann)

let proj index ann expr = EUnary (UProj index, expr, ann)

let is_simple_expr = function
	| EVar _ | EConst _ -> true
	| _ -> false

let match_fail ann message = app (match_fail_name, ann) [EConst (CString message, ann)]

let rec sink_until_first_use name proj ann expr =
	let (var_name, _) = name in
	let used_in expr = Collections.StringSet.mem var_name (Ast_helpers.free_vars_expr_no_globals expr) in
	match expr with
	| EVar (n1, _) when var_name = n1 -> ELet (name, proj, expr, ann)
	| EVar _ | EConst _ -> expr
	| ELet (name', rhs, body, ann') ->
		if used_in rhs then ELet (name, proj, expr, ann)
		else ELet (name', rhs, sink_until_first_use name proj ann body, ann')
	| EApp (f, args, _ann') when List.exists used_in args || used_in f -> ELet (name, proj, expr, ann)
	| EBinary (_, e1, e2, _ann') when used_in e1 || used_in e2 -> ELet (name, proj, expr, ann)
	| EUnary (op, e, ann') -> EUnary (op, sink_until_first_use name proj ann e, ann')
	| ECtor (_, args, _ann') when List.exists used_in args -> ELet (name, proj, expr, ann)
	| EAnno (e, t, ann') -> EAnno (sink_until_first_use name proj ann e, t, ann')
	| ETuple (e1, e2, _ann') when used_in e1 || used_in e2 -> ELet (name, proj, expr, ann)
	| EFun (_, body, _ann') when used_in body -> ELet (name, proj, expr, ann)
	| EIfe (cond, e1, e2, ann') ->
		if used_in cond then ELet (name, proj, expr, ann)
		else
			let e1' = if used_in e1 then sink_until_first_use name proj ann e1 else e1 in
			let e2' = if used_in e2 then sink_until_first_use name proj ann e2 else e2 in
			EIfe (cond, e1', e2', ann')
	| ECase (scrutinee, branches, ann') ->
		if used_in scrutinee then ELet (name, proj, expr, ann)
		else
			let cases = List.map (fun (pat, branch, ann'') -> (pat, sink_until_first_use name proj ann branch, ann'')) branches in
			ECase (sink_until_first_use name proj ann scrutinee, cases, ann')
	| _ -> expr

let is_string_case_head = function
	| PWildcard _ | PVar _ | PConst (CString _, _) -> true
	| _ -> false

let is_string_case_pattern = function
	| PWildcard _ | PVar _ | PConst (CString _, _) -> true
	| PStringCons (head, _, _) -> is_string_case_head head
	| _ -> false

let is_string_case cases =
	let has_string_discriminator =
		List.exists
			(fun (pattern, _, _) ->
				 match pattern with
				 | PConst (CString _, _) | PStringCons _ -> true
				 | _ -> false)
			cases
	in
	has_string_discriminator
	&& List.for_all (fun (pattern, _, _) -> is_string_case_pattern pattern) cases

type 's clause = {
	pats: 's pattern StringMap.t;
	bindings: (string * string * 's ann) list;
	body: 's expr;
}

let normalize_clause clause =
	let bindings = ref clause.bindings in
	let pats =
		StringMap.fold
			(fun var pat acc ->
				 match pat with
				 | PWildcard _ -> acc
				 | PVar (name, ann) ->
					 bindings := (name, var, ann) :: !bindings;
					 acc
				 | _ -> StringMap.add var pat acc)
			clause.pats
			StringMap.empty
	in
	{ clause with pats; bindings = List.rev !bindings }

let is_tree_testable = function
	| PWildcard _ | PVar _ | PStringCons _ -> false
	| _ -> true

let same_test_pattern left right =
	match left, right with
	| PConst (c1, _), PConst (c2, _) -> c1 = c2
	| PTuple _, PTuple _ -> true
	| PCtor ((name1, _), args1, _), PCtor ((name2, _), args2, _) ->
		String.equal name1 name2 && List.length args1 = List.length args2
	| PSigCons _, PSigCons _ -> true
	| _ -> false

let test_fresh_vars = function
	| PConst _ -> []
	| PTuple _ -> [Utilities.new_name "tuple_left"; Utilities.new_name "tuple_right"]
	| PCtor (_, args, _) -> List.init (List.length args) (fun _ -> Utilities.new_name "ctor_field")
	| PSigCons _ -> [Utilities.new_name "sig_head"; Utilities.new_name "sig_tail"]
	| _ -> []

let rec compile_string_case lower scrutinee cases ann =
	let scrutinee = lower scrutinee in
	let compile_with scrutinee_expr = compile_string_branches lower scrutinee_expr cases ann in
	if is_simple_expr scrutinee then compile_with scrutinee
	else
		let tmp = (Utilities.new_name "string_match", ann) in
		ELet (tmp, scrutinee, compile_with (var tmp), ann)

and compile_string_branches lower scrutinee cases ann =
	match cases with
	| [] -> match_fail ann "Non-exhaustive string match"
	| (pattern, body, _) :: rest ->
		compile_string_pattern lower scrutinee pattern (lower body) (fun () -> compile_string_branches lower scrutinee rest ann) ann

and compile_string_pattern lower scrutinee pattern success next ann =
	match pattern with
	| PWildcard _ -> success
	| PVar (name, name_ann) -> ELet ((name, name_ann), scrutinee, success, ann)
	| PConst (CString s, patt_ann) ->
		EIfe (app (string_eq, ann) [scrutinee; EConst (CString s, patt_ann)], success, next (), ann)
	| PStringCons (head_pat, tail_name, _) ->
		compile_string_cons lower scrutinee head_pat tail_name success next ann
	| _ -> failwith "Unexpected non-string pattern in compile_string_pattern"

and compile_string_cons _lower scrutinee head_pat tail_name success next ann =
	let head_tmp = (Utilities.new_name "string_head", ann) in
	let bind_tail body = ELet (tail_name, app (string_tail, ann) [scrutinee], body, ann) in
	let head_body =
		match head_pat with
		| PWildcard _ -> bind_tail success
		| PVar (name, name_ann) -> ELet ((name, name_ann), var head_tmp, bind_tail success, ann)
		| PConst (CString s, patt_ann) ->
			EIfe (app (string_eq, ann) [var head_tmp; EConst (CString s, patt_ann)], bind_tail success, next (), ann)
		| _ -> failwith "Unexpected non-string head pattern in compile_string_cons"
	in
	EIfe (
		app (string_is_empty, ann) [scrutinee],
		next (),
		ELet (head_tmp, app (string_head, ann) [scrutinee], head_body, ann),
		ann)

let add_test_subpatterns branch_var fresh_vars test_pat clause =
	let pats = StringMap.remove branch_var clause.pats in
	match test_pat, fresh_vars with
	| PConst _, [] -> { clause with pats }
	| PTuple (p1, p2, _), [left; right] ->
		{ clause with
			pats = pats |> StringMap.add left p1 |> StringMap.add right p2 }
	| PCtor (_, args, _), fresh_vars when List.length args = List.length fresh_vars ->
		let pats = List.fold_left2 (fun acc fresh pat -> StringMap.add fresh pat acc) pats fresh_vars args in
		{ clause with pats }
	| PSigCons (head_pat, (tail_name, tail_ann), _), [head; tail] ->
		{ clause with
			pats = pats |> StringMap.add head head_pat |> StringMap.add tail (PVar (tail_name, tail_ann)) }
	| _ -> failwith "Unexpected test shape in pattern lowering"

let branching_heuristic clauses =
	match clauses with
	| [] -> None
	| first :: _ ->
		let candidates =
			StringMap.fold
				(fun var pat acc -> if is_tree_testable pat then var :: acc else acc)
				first.pats
				[]
		in
		let count var =
			List.fold_left
				(fun total clause ->
					 match StringMap.find_opt var clause.pats with
					 | Some pat when is_tree_testable pat -> total + 1
					 | _ -> total)
				0
				clauses
		in
		match candidates with
		| [] -> None
		| v :: vs ->
			Some (
				List.fold_left
					(fun best candidate -> if count candidate > count best then candidate else best)
					v
					vs)

let emit_bindings bindings body =
	List.fold_right
		(fun (name, source, ann) acc -> ELet ((name, ann), EVar (source, ann), acc, ann))
		bindings
		body

let rec compile_leaf_clauses lower ann clauses =
	match clauses with
	| [] -> match_fail ann "Non-exhaustive pattern match"
	| clause :: rest ->
		let clause = normalize_clause clause in
		compile_leaf_clause lower ann clause rest

and compile_leaf_clause lower ann clause rest =
	match StringMap.bindings clause.pats with
	| [] -> emit_bindings clause.bindings clause.body
	| (branch_var, pattern) :: remaining ->
		let continue_clause =
			compile_leaf_clause lower ann { clause with pats = StringMap.of_list remaining } rest
		in
		let next_clause () = compile_leaf_clauses lower ann rest in
		(match pattern with
		| PWildcard _ | PVar _ | PConst (CString _, _) | PStringCons _ ->
			compile_string_pattern lower (var (branch_var, ann)) pattern continue_clause next_clause ann
		| _ -> failwith "Unexpected tree leaf pattern in decision-tree compiler")

let emit_test ann branch_var test_pat fresh_vars yes no =
	let scrutinee = var (branch_var, ann) in
	match test_pat with
	| PConst (CString s, patt_ann) ->
		EIfe (app (string_eq, ann) [scrutinee; EConst (CString s, patt_ann)], yes, no, ann)
	| PConst (c, patt_ann) ->
		EIfe (app (("eq", ann)) [scrutinee; EConst (c, patt_ann)], yes, no, ann)
	| PTuple _ ->
		(match fresh_vars with
		| [left; right] ->
			ECase (
				scrutinee,
				[
					(PTuple (PWildcard ann, PWildcard ann, ann),
					 	ELet ((left, ann), proj 0 ann scrutinee, ELet ((right, ann), proj 1 ann scrutinee, yes, ann), ann), ann);
					(PWildcard ann, no, ann)
				],
				ann)
		| _ -> failwith "Tuple tests expect two field variables")
	| PCtor ((ctor_name, ctor_ann), _, _) ->
		let field_pats = List.map (fun _ -> PWildcard ann) fresh_vars in
		let yes_branch =
			List.fold_right2
				(fun field index body -> ELet ((field, ann), proj index ann scrutinee, body, ann))
				fresh_vars
				(List.init (List.length fresh_vars) Fun.id)
				yes
		in
		ECase (
			scrutinee,
			[
				(PCtor ((ctor_name, ctor_ann), field_pats, ann), yes_branch, ann);
				(PWildcard ann, no, ann)
			],
			ann)
	| PSigCons _ ->
		(match fresh_vars with
		| [head; tail] ->
			let yes_branch =
				ELet (
					(head, ann),
					app (head_elim, ann) [scrutinee],
					ELet ((tail, ann), EUnary (UTail, scrutinee, ann), yes, ann),
					ann)
			in
			ECase (
				scrutinee,
				[
					(PSigCons (PWildcard ann, (tail, ann), ann), yes_branch, ann);
					(PWildcard ann, no, ann)
				],
				ann)
		| _ -> failwith "Signal-cons tests expect two field variables")
	| PStringCons _ ->
		failwith "String-cons patterns should be lowered by the string pass"
	| PWildcard _ | PVar _ ->
		failwith "Unexpected non-test pattern in emit_test"

let rec compile_tree_rows lower ann clauses =
	let clauses = List.map normalize_clause clauses in
	match clauses with
	| [] -> match_fail ann "Non-exhaustive pattern match"
	| clause1 :: _ when StringMap.is_empty clause1.pats -> emit_bindings clause1.bindings clause1.body
	| clause1 :: _ ->
		(match branching_heuristic clauses with
		| None -> compile_leaf_clauses lower ann clauses
		| Some branch_var ->
			let test_pat = StringMap.find branch_var clause1.pats in
			let fresh_vars = test_fresh_vars test_pat in
			let yes_clauses, no_clauses =
				List.fold_right
					(fun clause (yes, no) ->
						 match StringMap.find_opt branch_var clause.pats with
						 | None -> (clause :: yes, clause :: no)
						 | Some pat when same_test_pattern test_pat pat ->
							 (add_test_subpatterns branch_var fresh_vars pat clause :: yes, no)
						 | Some _ -> (yes, clause :: no))
					clauses
					([], [])
			in
			emit_test ann branch_var test_pat fresh_vars
				(compile_tree_rows lower ann yes_clauses)
				(compile_tree_rows lower ann no_clauses))

and compile_tree_case lower scrutinee cases ann =
	let scrutinee = lower scrutinee in
	let root_name = (Utilities.new_name "scrut", expr_get_ann scrutinee) in
	let clauses =
		List.map
			(fun (pat, body, _) ->
				 {
					 pats = StringMap.singleton (fst root_name) pat;
					 bindings = [];
					 body = lower body;
				 })
			cases
	in
	ELet (root_name, scrutinee, compile_tree_rows lower ann clauses, ann)

and compile_match e =
	match e with
	| EVar _ | EConst _ -> e
	| ECtor (name, args, ann) -> ECtor (name, List.map compile_match args, ann)
	| ECase (scrutinee, cases, ann) when is_string_case cases ->
		compile_string_case compile_match scrutinee cases ann
	| ECase (_, cases, _) when List.exists (fun (pat, _, _) -> match pat with PStringCons _ -> true | _ -> false) cases ->
		failwith "Mixed string-cons and non-string patterns are not supported by the decision-tree compiler"
	| ECase (scrutinee, cases, ann) ->
		compile_tree_case compile_match scrutinee cases ann
	| ELet (name, e1, e2, ann) -> ELet (name, compile_match e1, compile_match e2, ann)
	| EApp (e, args, ann) -> EApp (compile_match e, List.map compile_match args, ann)
	| EUnary (u, e, ann) -> EUnary (u, compile_match e, ann)
	| ETuple (e1, e2, ann) -> ETuple (compile_match e1, compile_match e2, ann)
	| EFun (args, body, ann) -> EFun (args, compile_match body, ann)
	| EBinary (op, e1, e2, ann) -> EBinary (op, compile_match e1, compile_match e2, ann)
	| EIfe (cond, e1, e2, ann) -> EIfe (compile_match cond, compile_match e1, compile_match e2, ann)
	| EAnno (e, t, ann) -> EAnno (compile_match e, t, ann)

let transform_patterns (program: 's Ast.program) =
	List.map (fun (TopLet (name, expr, ann)) -> TopLet (name, compile_match expr, ann)) program
