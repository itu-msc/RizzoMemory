open! Ast

let head_elim = (Rizzo_builtins.get "head").name
let string_eq = (Rizzo_builtins.get "string_eq").name
let string_is_empty = (Rizzo_builtins.get "string_is_empty").name
let string_head = (Rizzo_builtins.get "string_head").name
let string_tail = (Rizzo_builtins.get "string_tail").name

let var ((name, ann) : _ name) = EVar (name, ann)

let app ((name, ann) : _ name) args =
	EApp (var (name, ann), args, ann)
  
  let is_simple_expr = function
	| EVar _ | EConst _ -> true
	| _ -> false
  
let match_fail = (Rizzo_builtins.get "match_fail").name
let match_fail ann message =
	app (match_fail, ann) [EConst (CString message, ann)]

let rec transform_patterns (program: 's Ast.program) =
	program 
	|> List.map (function
    | TopTypeDef _ as e -> e
    | (TopLet (name, expr, ann)) -> TopLet (name, compile_match expr, ann))

and compile_pattern p scrutinee good bad =
	match p with
	| PWildcard _ -> good scrutinee
	| PVar (name, ann) ->
		ELet ((name, ann), scrutinee, good (EVar (name, ann)), ann)
	| PConst (CString s, ann) ->
		let matched = (Utilities.new_name "match", ann) in
		let equality = EApp (var (string_eq, ann), [scrutinee; EConst (CString s, ann)], ann) in
		ELet (matched, equality, EIfe (EVar matched, good scrutinee, bad (), ann), ann)
	| PConst (c, ann) ->
		let matched = (Utilities.new_name "match", ann) in
		let equality = EApp (EVar ("eq", ann), [scrutinee; EConst (c, ann)], ann) in
		ELet (matched, equality, EIfe (EVar matched, good scrutinee, bad (), ann), ann)
	| PTuple (PWildcard _, PWildcard _, ps, _) 
		when List.for_all (function | PWildcard _ -> true | _ -> false) ps ->
		good scrutinee
	| PTuple (p1, PWildcard _, [], ann) ->
		let left = (Utilities.new_name "tuple_left", ann) in
		ELet (
			left,
			EApp (EVar ("fst", ann), [scrutinee], ann),
			compile_pattern p1 (EVar left) good bad,
			ann)
	| PTuple (PWildcard _, p2, [], ann) ->
		let right = (Utilities.new_name "tuple_right", ann) in
		ELet (
			right,
			EApp (EVar ("snd", ann), [scrutinee], ann),
			compile_pattern p2 (EVar right) good bad,
			ann)
	| PTuple (p1, p2, ps, ann) as pat ->
		let rec compile_rest idx rest = 
			match rest with
			| [] -> good scrutinee
			| PWildcard _ :: ps -> compile_rest (idx + 1) ps
			| p :: ps ->
				let field_name = (Utilities.new_name "tuple_nth", ann) in
				ELet (
					field_name,
					EUnary (UProj idx, scrutinee, ann),
					compile_pattern p (EVar field_name)
						(fun _ -> compile_rest (idx + 1) ps)
						bad,
					ann)
		in
		let matched_branch = compile_rest 0 (p1 :: p2 :: ps) in
		ECase (scrutinee, [ (pat, matched_branch, ann); (PWildcard ann, bad (), ann) ], ann)
		
	| PSigCons (head_pat, (_, tail_ann as tail), ann) ->
		let head_name = (Utilities.new_name "sig_head", ann) in
		let head_proj = EApp (EVar (head_elim, ann), [scrutinee], ann) in
    let body = 
      compile_pattern head_pat (EVar head_name)
      (fun _ -> ELet (tail,EUnary (UTail, scrutinee, tail_ann), good (var tail), ann))
      bad 
    in
		ECase (
			scrutinee,
			[ (p,ELet (head_name, head_proj, body, ann), ann);
				(PWildcard ann, bad (), ann) ],
			ann)
	| PStringCons (head_pat, (_tail_name, tail_ann as tail), ann) ->
		let head_name = (Utilities.new_name "string_head", ann) in
		let head_expr = EVar head_name in
		let bind_tail body =
			ELet (tail, EApp (EVar (string_tail, tail_ann), [scrutinee], tail_ann), body, ann)
		in
		let head_body =
			compile_pattern head_pat head_expr
				(fun head_expr' -> bind_tail (good head_expr'))
				bad
		in
		EIfe (
			app (string_is_empty, ann) [scrutinee],
			bad (),
			ELet (head_name, app (string_head, ann) [scrutinee], head_body, ann),
			ann)
	| PCtor (_ctor_name, args, ann) as pat ->
		let rec compile_fields index fields =
			match fields with
			| [] -> good scrutinee
			| subpat :: rest ->
				let field_name = (Utilities.new_name "ctor_field", ann) in
				ELet (
					field_name,
					EUnary (UProj index, scrutinee, ann),
					compile_pattern subpat (EVar field_name)
						(fun _ -> compile_fields (index + 1) rest)
						bad,
					ann)
		in
		ECase (scrutinee, [ (pat, compile_fields 0 args, ann); (PWildcard ann, bad (), ann) ], ann)

and compile_match_cases scrutinee cases =
	match cases with
	| [] -> match_fail (expr_get_ann scrutinee) "Non-exhaustive pattern match"
	| (pat, body, _) :: rest ->
		let compiled_case = compile_match body in
		compile_pattern pat scrutinee
			(fun _ -> compiled_case)
			(fun () -> compile_match_cases scrutinee rest)

and compile_match e =
	match e with
	| EVar _ | EConst _ -> e
	| ECtor (name, args, ann) -> ECtor (name, List.map compile_match args, ann)
	| ECase (scrutinee, cases, ann) ->
		let scrutinee = compile_match scrutinee in
		if is_simple_expr scrutinee then
			compile_match_cases scrutinee cases
		else
			let scrutinee_name = (Utilities.new_name "scrut", expr_get_ann scrutinee) in
			ELet (scrutinee_name, scrutinee, compile_match_cases (var scrutinee_name) cases, ann)
	| ELet (name, e1, e2, ann) -> ELet (name, compile_match e1, compile_match e2, ann)
	| EApp (e, args, ann) -> EApp (compile_match e, List.map compile_match args, ann)
	| EUnary (u, e, ann) -> EUnary (u, compile_match e, ann)
	| ETuple (e1, e2, es, ann) -> ETuple (compile_match e1, compile_match e2, List.map compile_match es, ann)
	| EFun (args, body, ann) -> EFun (args, compile_match body, ann)
	| EBinary (op, e1, e2, ann) -> EBinary (op, compile_match e1, compile_match e2, ann)
	| EIfe (cond, e1, e2, ann) -> EIfe (compile_match cond, compile_match e1, compile_match e2, ann)
	| EAnno (e, t, ann) -> EAnno (compile_match e, t, ann)
