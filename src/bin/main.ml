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
		Format.printf "%a\n" Ast.pp_program program
	with
	| Rizzoc.Parser.Error (loc, msg) ->
			Location.show_error_context loc msg;
			exit 1
