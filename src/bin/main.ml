open! Rizzoc

let usage_msg = "Usage: rizzoc <program.rizz>"

let print_section title pp value =
	Fmt.pr "@[<v>@{<cyan>%s@}@,%a@]@.@." title pp value

let ansi_of_tag = function
	| "red" -> "\027[31m"
	| "yellow" -> "\027[33m"
	| "green" -> "\027[32m"
	| "ligtgreen" -> "\027[92m"
	| "orange" -> "\027[38;5;214m"
	| "blue" -> "\027[34m"
	| "magenta" -> "\027[35m"
	| "cyan" -> "\027[36m"
	| "lightcyan" -> "\027[38;5;117m"
	| "bold" -> "\027[1m"
	| _ -> ""

let setup_color_tags ppf =
	let current = Format.pp_get_formatter_stag_functions ppf () in
	Format.pp_set_mark_tags ppf true;
	Format.pp_set_print_tags ppf false;
	Format.pp_set_formatter_stag_functions ppf {
		current with
		mark_open_stag = (function
			| Format.String_tag tag -> ansi_of_tag tag
			| _ -> "");
		mark_close_stag = (function
			| Format.String_tag _ -> "\027[0m"
			| _ -> "");
	}

let setup_tty () =
	Fmt_tty.setup_std_outputs ();
	setup_color_tags Format.std_formatter;
	setup_color_tags Format.err_formatter

let ensure_rizz_extension path =
	let ext = Filename.extension path in
	if ext <> ".rizz" then (
		Fmt.epr "@{<red>Error@}: expected a .rizz file, got: %s@." path;
		exit 1
	)

let () =
	setup_tty ();
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
		print_section "------- Parsed -------" Ast.pp_program program;
		let (typed, errors) = Rizzoc.typecheck program in
		(match errors with
		| [] -> print_section "------- Type checked -------" Ast.pp_typed_program typed
		| _ ->
			Fmt.pr "@{<yellow>------- Type checking failed, showing partial typechecked program -------@}@.";
			List.iter (fun err -> 
				match err with
				| (loc, msg) -> Location.show_error_context loc msg
			) errors;
		);
    let transformed = Rizzoc.apply_transforms program in
    print_section "------- Transformed -------" Ast.pp_program transformed;
    let rc_env, rc_program = Rizzoc.ref_count transformed in
		print_section "------- Reference counted -------" (RefCount.pp_ref_counted_program ~ownerships:(Some rc_env)) rc_program;
		Rizzoc.emit rc_program "output.c"
	with
	| Rizzoc.Lexer.Error (loc, msg) ->
			Location.show_error_context loc msg;
			exit 1
	| Rizzoc.Parser.Error (loc, msg) ->
			Location.show_error_context loc msg;
			exit 1
