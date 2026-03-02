open! Rizzoc

let usage_msg = "Usage: rizzoc <program.rizz>"

let ensure_rizz_extension path =
	let ext = Filename.extension path in
	if ext <> ".rizz" then (
		Printf.eprintf "Expected a .rizz file, got: %s\n" path;
		exit 1
	)

let () =
	let input_file = ref None in
	let set_input_file path =
		match !input_file with
		| None -> input_file := Some path
		| Some _ -> raise (Arg.Bad "Only one input file is allowed")
	in
	Arg.parse [] set_input_file usage_msg;
	let input_file =
		match !input_file with
		| Some path -> path
		| None ->
			Arg.usage [] usage_msg;
			exit 1
	in
	ensure_rizz_extension input_file;
	try
		let program = Rizzoc.Parser.parse_file input_file in
		Format.printf "------- Parsed -------\n%a\n\n" Ast.pp_program program;
		let (typed, errors) = Rizzoc.typecheck program in
		(match errors with
		| [] -> Format.printf "------- Type checked -------\n%a\n\n" Ast.pp_typed_program typed
		| _ ->
			Format.printf "------- Type checking failed, showing partial typechecked program -------\n%a\n\n" Ast.pp_typed_program typed;
			List.iter (fun err -> 
				match err with
				| (loc, msg) -> Location.show_error_context loc msg
			) errors;
		);
    let transformed = Rizzoc.apply_transforms program in
    Format.printf "------- Transformed -------\n%a\n\n" Ast.pp_program transformed;
    let rc_program = Rizzoc.ref_count transformed in
		Format.printf "------- Reference counted -------\n%a\n" RefCount.pp_ref_counted_program rc_program;
		Rizzoc.emit rc_program "output.c"
	with
	| Rizzoc.Lexer.Error (loc, msg) ->
			Location.show_error_context loc msg;
			exit 1
	| Rizzoc.Parser.Error (loc, msg) ->
			Location.show_error_context loc msg;
			exit 1
