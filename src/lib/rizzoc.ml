
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

module RefCount = struct
include Refcount
end

module Transformations = struct
  module ANF = struct
    let anf_expr = Transform_anf.normalize_expr
    let anf = Transform_anf.normalize_program
  end
  let lift = Transform_lift.lift
  let eliminate_copy_propagation = Transform_copr.eliminate_copy_propagation
  let eliminate_copy_propagation_program = Transform_copr.copy_propagate

  let to_rc_ir = Transform_rc.to_rc_intermediate_representation
  
  let builtins = 
    let module StringMap = Map.Make(String) in
    StringMap.of_list [
      "sync", [RefCount.Owned; RefCount.Owned];
      "wait", [RefCount.Owned];
      "watch", [RefCount.Owned];
      "tail", [RefCount.Owned];
      "delay", [RefCount.Owned];
    ]

  let auto_ref_count (program: Ast.program) = 
    let module StringMap = Map.Make(String) in
    let constants = to_rc_ir program in
    let beta = Refcount.infer_all ~builtins:builtins constants in
    let insert_ref_count (c_name, RefCount.Fun (params, c_body)) = 
      let params_ownership = Refcount.lookup_params beta c_name in
      let var_env = StringMap.of_list @@ List.combine params params_ownership in
      let ref_counted_body = Refcount.insert_rc c_body var_env beta in
      (c_name, RefCount.Fun (params, Refcount.insert_dec_many params ref_counted_body var_env))
    in
    beta, List.map insert_ref_count constants
end

module Utilities = struct include Utilities end

let apply_transforms p =
  p
  |> Transformations.lift
  |> Transformations.eliminate_copy_propagation_program
  |> Transformations.ANF.anf

let ref_count p = snd @@ Transformations.auto_ref_count p