open Ast
open Collections

let rec has_effectful_call (e : _ expr) : bool =
	match e with
	| EConst _ | EVar _ | EError _ -> false
	| ECtor (_, args, _) -> List.exists has_effectful_call args
	| EApp (f, args, _) ->
			let direct_effectful =
				match f with
				| EVar (name, _) -> Effectful.is_effectful name
				| _ -> false
			in
			direct_effectful || has_effectful_call f || List.exists has_effectful_call args
	| EUnary (_, e, _) -> has_effectful_call e
	| EBinary (_, e1, e2, _) | ETuple (e1, e2, _) ->
			has_effectful_call e1 || has_effectful_call e2
	| EIfe (cond, e1, e2, _) ->
			has_effectful_call cond || has_effectful_call e1 || has_effectful_call e2
	| ECase (scrutinee, branches, _) ->
			has_effectful_call scrutinee
			|| List.exists (fun (_, branch, _) -> has_effectful_call branch) branches
	| EFun (_, _, _) -> false
	| ELet (_, e1, e2, _) -> has_effectful_call e1 || has_effectful_call e2
	| EAnno (e, _, _) -> has_effectful_call e

let rec eliminate_dead_let (e : _ expr) : _ expr =
	match e with
	| EConst _ | EVar _ | EError _ -> e
	| ECtor (name, args, ann) -> ECtor (name, List.map eliminate_dead_let args, ann)
	| EApp (f, args, ann) -> EApp (eliminate_dead_let f, List.map eliminate_dead_let args, ann)
	| EUnary (op, e, ann) -> EUnary (op, eliminate_dead_let e, ann)
	| EBinary (op, e1, e2, ann) ->
			EBinary (op, eliminate_dead_let e1, eliminate_dead_let e2, ann)
	| ETuple (e1, e2, ann) -> ETuple (eliminate_dead_let e1, eliminate_dead_let e2, ann)
	| EIfe (cond, e1, e2, ann) ->
			EIfe (eliminate_dead_let cond, eliminate_dead_let e1, eliminate_dead_let e2, ann)
	| ECase (scrutinee, branches, ann) ->
			ECase (
				eliminate_dead_let scrutinee,
				List.map
					(fun (pattern, branch, ann) -> (pattern, eliminate_dead_let branch, ann))
					branches,
				ann)
	| EFun (params, body, ann) -> EFun (params, eliminate_dead_let body, ann)
	| ELet (((x, _) as name), e1, e2, ann) ->
			let e1' = eliminate_dead_let e1 in
			let e2' = eliminate_dead_let e2 in
			if Collections.StringSet.mem x (Ast_helpers.free_vars_expr_no_globals e2') || has_effectful_call e1'
			then ELet (name, e1', e2', ann)
			else e2'
	| EAnno (e, t, ann) -> EAnno (eliminate_dead_let e, t, ann)

let dead_let_eliminate (p : _ program) : _ program =
	p
	|> List.map (function 
		| TopTypeDef _ as e -> e
		| TopLet (x, e, ann) -> TopLet (x, eliminate_dead_let e, ann))

let rec is_function_expr : type s. s expr -> bool = function
	| EFun _ -> true
	| EAnno (e, _, _) -> is_function_expr e
	| _ -> false

let top_let_name = function
	| TopLet ((name, _), _, _) -> Some name
	| TopTypeDef _ -> None

let top_let_rhs = function
	| TopLet ((name, _), rhs, _) -> Some (name, rhs)
	| TopTypeDef _ -> None

let keep_as_effectful_root = function
	| TopLet (_, rhs, _) -> (not (is_function_expr rhs)) && has_effectful_call rhs
	| TopTypeDef _ -> false

let eliminate_unused_top_levels (p : _ program) : _ program =
	let top_names =
		p
		|> List.filter_map top_let_name
		|> StringSet.of_list
	in
	if not (StringSet.mem "entry" top_names) then p
	else
		let top_rhs =
			p
			|> List.filter_map top_let_rhs
			|> List.fold_left (fun acc (name, rhs) -> StringMap.add name rhs acc) StringMap.empty
		in
		let effectful_roots =
			p
			|> List.filter keep_as_effectful_root
			|> List.filter_map top_let_name
			|> StringSet.of_list
		in
		let rec mark reachable pending =
			match pending with
			| [] -> reachable
			| name :: rest when StringSet.mem name reachable -> mark reachable rest
			| name :: rest ->
					let reachable = StringSet.add name reachable in
					let deps =
						match StringMap.find_opt name top_rhs with
						| None -> StringSet.empty
						| Some rhs ->
								Ast_helpers.free_vars_expr_no_globals rhs
								|> StringSet.inter top_names
					in
					mark reachable (StringSet.elements deps @ rest)
		in
		let roots = StringSet.add "entry" effectful_roots |> StringSet.elements in
		let reachable = mark StringSet.empty roots in
		List.filter
			(function
			| TopTypeDef _ -> true
			| TopLet ((name, _), _, _) -> StringSet.mem name reachable)
			p
