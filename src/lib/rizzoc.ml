
module Location = struct
include Location
end

module Lexer = struct
include Lexer
end

module Parser = struct
include Source_units.Parser
end

module Source_units = struct
include Source_units
end

module Ast = struct 
include Ast
end

module Effectful = struct
include Effectful
end

module RefCount = struct
include Refcount
end

module TypeCheck = struct
  type typing_result = Typecheck.typing_result
  type type_definition = Type_env.typedefinition
  type ctor_definition = Type_env.constructor_defintion
  type type_definition_env = Type_env.typedefinition_env

  let typecheck = Typecheck.typecheck
  let type_definitions_to_ctor_mappings = Type_env.TypeDefinitionEnv.to_ctor_mappings
end

module Transformations = struct
  module ANF = struct
    let anf_expr = Transforms.Anf.normalize_expr
    let anf = Transforms.Anf.normalize_program
  end
  let eliminate_consecutive_lambdas = Transforms.Consecutive_lambda.eliminate_consecutive_lambdas_expr
  let eliminate_consecutive_lambdas_program = Transforms.Consecutive_lambda.eliminate_consecutive_lambdas_program
  let remove_duplicate_names = Transforms.Remove_dup_names.subst_program
  let lift = Transforms.Lambda_lifting.lift
  let explicit_lift_function_values_program = Transforms.Explicit_lifts.make_explicit

  let eliminate_copy_propagation = Transforms.Copy_propagation.eliminate_copy_propagation
  let eliminate_copy_propagation_program = Transforms.Copy_propagation.copy_propagate
  let eliminate_dead_let = Transforms.Dead_let_elimination.eliminate_dead_let
  let eliminate_dead_let_program = Transforms.Dead_let_elimination.dead_let_eliminate
  let eliminate_unused_top_levels = Transforms.Dead_let_elimination.eliminate_unused_top_levels
  let eliminate_patterns = Transforms.Patterns.transform_patterns
  let eliminate_simple_patterns = Transforms.Simple_patterns.transform_patterns
  let eliminate_patterns_tree = Transforms.Patterns_tree.transform_patterns
  let lower_typed_program = Transforms.Typed_lowering.lower_program
  let ast_to_rc_ir = Transforms.Pure_to_rc.to_rc_intermediate_representation
  
  let builtins = Rizzo_builtins.builtins_ownerships_map
  
  let auto_ref_count ctor_mappings (program: Ast.parsed Ast.program) = 
    let program = ast_to_rc_ir ctor_mappings builtins program in
    RefCount.reference_count_program builtins Rizzo_builtins.builtins_projection_map program
end

module Utilities = struct include Utilities end
module Collections = struct include Collections end
module Build_version = struct include Build_version end

type generated_c_compiler_invocation = {
  compiler : string;
  arguments : string list;
}

let c_compiler_candidates = ["gcc"; "clang"]

let runtime_installed_relative_parts = [".."; "lib"; "rizzoc"; "runtime"]
let runtime_dev_relative_parts = [".."; ".."; ".."; ".."; "src"; "runtime"]

let path_exists path =
  Sys.file_exists path

let is_directory path =
  try Sys.is_directory path with Sys_error _ -> false

let join_path base parts =
  List.fold_left Filename.concat base parts

let realpath path =
  try Unix.realpath path with Unix.Unix_error (_, _, _) -> path

let executable_path () =
  realpath Sys.executable_name

let candidate_runtime_roots ?executable_path:maybe_executable_path () =
  let resolved_executable_path = Option.value maybe_executable_path ~default:(executable_path ()) in
  let executable_dir = Filename.dirname resolved_executable_path in
  [ join_path executable_dir runtime_dev_relative_parts;
    join_path executable_dir runtime_installed_relative_parts;
  ]
  |> List.map realpath

let resolve_default_runtime_root ?executable_path () =
  match List.find_opt is_directory (candidate_runtime_roots ?executable_path ()) with
  | Some root -> root
  | None ->
      let tried = String.concat ", " (candidate_runtime_roots ?executable_path ()) in
      failwith (Printf.sprintf "Could not locate runtime directory. Looked in: %s" tried)

let is_executable_file path =
  Sys.file_exists path && not (Sys.is_directory path)

let split_search_path value =
  let separator = if Sys.win32 then ';' else ':' in
  value
  |> String.split_on_char separator
  |> List.map (fun segment -> if segment = "" then "." else segment)

let command_candidates command =
  let has_dir_separator =
    String.contains command '/' || (Sys.win32 && String.contains command '\\')
  in
  let suffixes = if Sys.win32 then [""; ".exe"; ".cmd"; ".bat"] else [""] in
  if Filename.is_relative command && not has_dir_separator then
    let search_paths =
      Sys.getenv_opt "PATH"
      |> Option.value ~default:""
      |> split_search_path
    in
    List.concat_map
      (fun search_path ->
        List.map (fun suffix -> Filename.concat search_path (command ^ suffix)) suffixes)
      search_paths
  else
    List.map (fun suffix -> command ^ suffix) suffixes

let command_exists command =
  List.exists is_executable_file (command_candidates command)

let find_available_c_compiler () =
  match List.find_opt command_exists c_compiler_candidates with
  | Some compiler -> compiler
  | None ->
      failwith
        "No supported C compiler found. Expected gcc or clang to be available on PATH."

let resolve_c_compiler ?compiler () =
  match compiler with
  | Some compiler when String.trim compiler <> "" ->
      let compiler = String.trim compiler in
      if command_exists compiler then compiler
      else
        failwith
          (Printf.sprintf "Configured C compiler is not available on PATH: %s" compiler)
  | _ -> find_available_c_compiler ()

let generated_c_compiler_invocation ?compiler ?(runtime_include = "src/runtime")
    ?(is_windows = Sys.win32) ?(debug_malloc = false) ?(debug_info = false) ?(heap_info = false)
    ~input_file ~output_file () =
  let compiler = resolve_c_compiler ?compiler () in
  let runtime_include =
    if runtime_include = "src/runtime" then resolve_default_runtime_root ()
    else runtime_include
  in
  let arguments =
    (if is_windows then ["-m64"] else [])
    @ (if debug_malloc then ["-D__RZ_DEBUG_MALLOC"] else [])
    @ (if debug_info then ["-D__RZ_DEBUG_INFO"] else [])
    @ (if heap_info then ["-D__RZ_HEAP_INFO"] else [])
    @ (if debug_info || debug_malloc || heap_info then ["-g"] else [])
    @ ["-I"; runtime_include; input_file; "-o"; output_file]
    @ (if is_windows then ["-lws2_32"] else ["-lm"])
  in
  { compiler; arguments }

let to_shell_command { compiler; arguments } =
  Printf.sprintf "%s %s" compiler (String.concat " " (List.map Filename.quote (arguments)))

module Language_service = struct
include Language_service
end

let apply_transforms p =
  p
  |> Transformations.eliminate_consecutive_lambdas_program
  |> Transformations.lift
  |> Transformations.explicit_lift_function_values_program
  |> Transformations.eliminate_unused_top_levels
  |> Transformations.eliminate_copy_propagation_program
  |> Transformations.remove_duplicate_names
  |> Transformations.eliminate_patterns_tree
  |> Transformations.eliminate_dead_let_program
  |> Transformations.ANF.anf
  |> Transformations.eliminate_copy_propagation_program
  |> Transformations.eliminate_unused_top_levels

let ref_count p = Transformations.auto_ref_count p

let lower_typed_program = Transformations.lower_typed_program
let apply_typed_transforms p = p |> lower_typed_program |> apply_transforms

let emit = Backend_c.emit_c_code

let source_units_for_compile ?executable_path ?stdlib_path ?(include_paths = []) input_files =
  Source_units.default_source_units ?executable_path ?stdlib_path ()
  @ Source_units.included_source_units include_paths
  @ List.map Source_units.source_file input_files

type ast_dump_format =
  | Dump_parsed
  | Dump_typed
  | Dump_transformed
  | Dump_ref_counted
  | Dump_all

let print_ast_dump = ref None

let rec compile_from_files ?executable_path ?stdlib_path ?(include_paths = []) input_files output_file =
  try
    let program = Source_units.parse_source_units (source_units_for_compile ?executable_path ?stdlib_path ~include_paths input_files) in
    compile program output_file
  with
  | Source_units.Validation_failed errors ->
      Source_units.report_validation_errors errors;
      failwith "Validation failed"
  | Parser.Error (loc, msg) ->
      Location.show_error_context loc msg;
      failwith "Parsing failed"
  | Lexer.Error (loc, msg) ->
      Location.show_error_context loc msg;
      failwith "Lexing failed"
  | exn ->
      let msg = Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn) in
      Fmt.epr "@{<red>Error@}: %s@." msg;
      failwith "Compilation failed"

and compile_from_file ?executable_path ?stdlib_path ?(include_paths = []) input_file output_file =
  compile_from_files ?executable_path ?stdlib_path ~include_paths [input_file] output_file
  
and compile_from_string input_string output_file =
  try
    let program = Parser.parse_string input_string in
    compile program output_file
  with
  | Source_units.Validation_failed errors ->
      Source_units.report_validation_errors errors;
      failwith "Validation failed"
  | Parser.Error (loc, msg) ->
      Location.show_error_context loc msg;
      failwith "Parsing failed"
  | Lexer.Error (loc, msg) ->
      Location.show_error_context loc msg;
      failwith "Lexing failed"
  | exn ->
      let msg = Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn) in
      Fmt.epr "@{<red>Error@}: %s@." msg;
      failwith "Compilation failed"

and compile parsed_program output_file =
  let print_section title pp value =
	  Fmt.pr "@[<v>@{<cyan>%s@}@,%a@]@.@." title pp value
  in
  try
    let validation_errors = Source_units.validate_program parsed_program in
    (match validation_errors with
    | [] -> ()
    | _ -> raise (Source_units.Validation_failed validation_errors)
    );
    if !print_ast_dump = Some Dump_all || !print_ast_dump = Some Dump_parsed then
      print_section "------- Parsed -------" Ast.pp_program parsed_program;
		let {typed_program = typed; type_errors = errors; type_definitions} : TypeCheck.typing_result = TypeCheck.typecheck parsed_program in
		(match errors with
    | [] ->
        if !print_ast_dump = Some Dump_all || !print_ast_dump = Some Dump_typed then
          print_section "------- Type checked -------" Ast.pp_typed_program typed
		| _ ->
			Fmt.pr "@{<yellow>------- Type checking failed, showing partial typechecked program -------@}@.";
			List.iter (fun err -> 
				match err with
				| (loc, msg) -> Location.show_error_context loc msg
			) errors;
			(* exit 1 *)
      failwith "Type checking failed"
		);
    let transformed = apply_typed_transforms typed in
    if !print_ast_dump = Some Dump_all || !print_ast_dump = Some Dump_transformed then
      print_section "------- Transformed -------" Ast.pp_program transformed;
    let ctor_mappings = TypeCheck.type_definitions_to_ctor_mappings type_definitions in
    let rc_env, rc_program = ref_count ctor_mappings transformed in
    if !print_ast_dump = Some Dump_all || !print_ast_dump = Some Dump_ref_counted then
      print_section "------- Reference counted -------" (RefCount.pp_ref_counted_program ~ownerships:(Some rc_env)) rc_program;
		emit rc_program output_file
	with
	| Lexer.Error (loc, msg) ->
			Location.show_error_context loc msg;
			failwith "Lexing failed"
	| Parser.Error (loc, msg) ->
			Location.show_error_context loc msg;
			failwith "Parsing failed"
