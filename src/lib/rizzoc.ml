
module Location = struct
include Location
end

module Lexer = struct
include Lexer
end

module Parser = struct
include Parser

exception Error of Location.t * string

let parse_with lexbuf =
  Effectful.reset_custom ();
  try Parser.main Lexer.read lexbuf 
  with
  | Lexer.Error _ as exn ->
      raise exn
  | exn ->
      let loc = Location.mk lexbuf.Lexing.lex_start_p lexbuf.Lexing.lex_curr_p in
      let msg = Printf.sprintf "Menhir parse error: %s" (Printexc.to_string exn) in
      raise (Error (loc, msg))

let parse_string (s : string) =
  let lexbuf = Lexing.from_string s in
  parse_with lexbuf

let parse_string_with_filename ~(filename : string) (s : string) =
  let lexbuf = Lexing.from_string s in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  parse_with lexbuf

let parse_file (filename : string) =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  let result = parse_with lexbuf in
  close_in ic;
  result
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

module Transformations = struct
  module ANF = struct
    let anf_expr = Transforms.Anf.normalize_expr
    let anf = Transforms.Anf.normalize_program
  end
  let eliminate_consecutive_lambdas = Transforms.Consecutive_lambda.eliminate_consecutive_lambdas_expr
  let eliminate_consecutive_lambdas_program = Transforms.Consecutive_lambda.eliminate_consecutive_lambdas_program
  let remove_duplicate_names = Transforms.Remove_dup_names.subst_program
  let lift = Transforms.Lambda_lifting.lift

  let eliminate_copy_propagation = Transforms.Copy_propagation.eliminate_copy_propagation
  let eliminate_copy_propagation_program = Transforms.Copy_propagation.copy_propagate
  let eliminate_dead_let = Transforms.Dead_let_elimination.eliminate_dead_let
  let eliminate_dead_let_program = Transforms.Dead_let_elimination.dead_let_eliminate
  let eliminate_patterns = Transforms.Patterns.transform_patterns
  let eliminate_simple_patterns = Transforms.Simple_patterns.transform_patterns
  let lower_typed_program = Transforms.Typed_lowering.lower_program
  let ast_to_rc_ir = Transforms.Pure_to_rc.to_rc_intermediate_representation
  
  let builtins = Rizzo_builtins.builtins_ownerships_map
  
  let auto_ref_count (program: Ast.parsed Ast.program) = 
    let program = ast_to_rc_ir builtins program in
    RefCount.reference_count_program builtins program
end

module Utilities = struct include Utilities end

module Language_service = struct
include Language_service
end

let apply_transforms p =
  p
  |> Transformations.eliminate_consecutive_lambdas_program
  |> Transformations.lift
  |> Transformations.eliminate_copy_propagation_program
  (* |> Transformations.eliminate_dead_let_program *)
  (* |> Transformations.eliminate_patterns *)
  |> Transformations.remove_duplicate_names
  |> Transformations.eliminate_simple_patterns
  |> Transformations.eliminate_dead_let_program
  |> Transformations.ANF.anf
  |> Transformations.eliminate_copy_propagation_program (* TODO *)

let ref_count p = Transformations.auto_ref_count p

let typecheck = Typecheck.typecheck
let lower_typed_program = Transformations.lower_typed_program
let apply_typed_transforms p = p |> lower_typed_program |> apply_transforms

let emit = Backend_c.emit_c_code

let rec compile_from_file input_file output_file =
  try
    let program = Parser.parse_file input_file in
    compile program output_file
  with
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
  
and compile_from_string input_string output_file =
  try
    let program = Parser.parse_string input_string in
    compile program output_file
  with
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
		print_section "------- Parsed -------" Ast.pp_program parsed_program;
		let (typed, errors) = typecheck parsed_program in
		(match errors with
		| [] -> print_section "------- Type checked -------" Ast.pp_typed_program typed
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
    print_section "------- Transformed -------" Ast.pp_program transformed;
    let rc_env, rc_program = ref_count transformed in
		print_section "------- Reference counted -------" (RefCount.pp_ref_counted_program ~ownerships:(Some rc_env)) rc_program;
		emit rc_program output_file
	with
	| Lexer.Error (loc, msg) ->
			Location.show_error_context loc msg;
			failwith "Lexing failed"
	| Parser.Error (loc, msg) ->
			Location.show_error_context loc msg;
			failwith "Parsing failed"

