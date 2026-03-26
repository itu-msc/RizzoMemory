open! Rizzoc

let usage_msg = "Usage: rizzoc [--overwrite-stdpath <path>] [-I <path>] <program.rizz> [more-files.rizz ...]"


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
	let stdlib_path = ref None in
	let include_paths = ref [] in
	let input_files = ref [] in
	let set_stdlib_path path =
		stdlib_path := Some path
	in
	let add_include_path path =
		include_paths := !include_paths @ [path]
	in
	let add_input_file path =
		input_files := !input_files @ [path]
	in
	Arg.parse
		[ ("--overwrite-stdpath", Arg.String set_stdlib_path, "Override the implicit stdlib with a .rizz file or stdlib directory")
		; ("-I", Arg.String add_include_path, "Include an extra .rizz file or a directory of .rizz files before user files")
		]
		add_input_file
		usage_msg;
	let input_files =
		match !input_files with
		| _ :: _ -> !input_files
		| [] ->
			Arg.usage [] usage_msg;
			exit 1
	in
	List.iter ensure_rizz_extension input_files;
	Rizzoc.compile_from_files ?stdlib_path:!stdlib_path ~include_paths:!include_paths input_files "output.c"
