open Ast

let rec has_effectful_call (e : _ expr) : bool =
	match e with
	| EConst _ | EVar _ -> false
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
	| EConst _ | EVar _ -> e
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
			if Ast_helpers.StringSet.mem x (Ast_helpers.free_vars_expr_no_globals e2') || has_effectful_call e1'
			then ELet (name, e1', e2', ann)
			else e2'
	| EAnno (e, t, ann) -> EAnno (eliminate_dead_let e, t, ann)

let dead_let_eliminate (p : _ program) : _ program =
	List.map (fun (TopLet (x, e, ann)) -> TopLet (x, eliminate_dead_let e, ann)) p
