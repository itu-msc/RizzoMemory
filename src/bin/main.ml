open! Rizzoc

let usage_msg = "Usage: rizzoc [--version] [--overwrite-stdpath <path>] [--print-ast] [-I <path>] [more-files.rizz ...] <entrypoint.rizz>"

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
	let print_version () =
		Fmt.pr "%s@." Rizzoc.Build_version.current;
		exit 0
	in
	let stdlib_path = ref None in
	let include_paths = ref [] in
	let input_files = ref [] in
	let print_ast = ref false in
	let set_stdlib_path path =
		stdlib_path := Some path
	in
	let set_print_ast () =
		print_ast := true
	in
	let add_include_path path =
		include_paths := !include_paths @ [path]
	in
	let add_input_file path =
		input_files := !input_files @ [path]
	in
	let debug_malloc = ref false in
	let debug_info = ref false in
	let heap_info = ref false in
	let out_name = ref "output" in

	Arg.parse
		[ ("--version", Arg.Unit print_version, "Print compiler version and exit")
		; ("--overwrite-stdpath", Arg.String set_stdlib_path, "Override the implicit stdlib with a .rizz file or stdlib directory")
		; ("--print-ast", Arg.Unit set_print_ast, "Print parsed, typed, transformed, and reference-counted ASTs during compilation")
		; ("-I", Arg.String add_include_path, "Include an extra .rizz file or a directory of .rizz files before user files")
		; ("--debug-malloc", Arg.Set debug_malloc, "Print debug info about memory allocations and deallocations at runtime")
		; ("--debug-info"	 , Arg.Set debug_info	 , "Print debug info about the signal heap, when new time steps occur, and which channels produced the input")
		; ("--info-heap"	 , Arg.Set heap_info	 , "On exit, print the number of signals left in the signal heap, how many steps was taken and average step time in microseconds")
		; ("-o", Arg.Set_string out_name, "Set the output executable name - defaults to output.exe")
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
	print_ast_dumps := !print_ast;
	if Sys.file_exists !out_name && Sys.is_directory !out_name then (
		Fmt.epr "@{<red>Error@}: output path cannot be a directory: %s@." !out_name;
		exit 1
	);
	let output_name = Filename.remove_extension !out_name in
	let output_c = output_name ^ ".c" in
	let output_file = 
		if Filename.extension !out_name = "" && Sys.win32 then output_name ^ ".exe"
		else !out_name
	in
	Rizzoc.compile_from_files ?stdlib_path:!stdlib_path ~include_paths:!include_paths input_files output_c;
	let runtime_include = "src/runtime" in
	let shellCommand = Rizzoc.to_shell_command (
			Rizzoc.generated_c_compiler_invocation
				~runtime_include ~input_file:output_c ~output_file 
				~debug_malloc:!debug_malloc ~debug_info:!debug_info ~heap_info:!heap_info ()
	) in
	let returnCode = Sys.command shellCommand in
	if returnCode <> 0 then (
		Fmt.epr "@{<red>Error@}: C compilation failed with exit code %d.@." returnCode;
		Fmt.epr "Command: %s@." shellCommand;
		exit 1
	) else (
		Fmt.pr "Compilation successful! Executable generated at: %s@." output_file
	)

