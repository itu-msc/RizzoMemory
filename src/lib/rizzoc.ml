
module Location = struct
include Location
end

module Lexer = struct
include Lexer
end

module Parser = struct
include Parser
end

module Ast = struct 
include Ast
end

module RefCount = struct
include Refcount
end

module Transformations = struct
  module ANF = struct
    let anf_expr = Transform_rc.ANF.normalize_expr
    let anf = Transform_rc.ANF.normalize_program
  end
  let lift = Transform_lift.lift
  let eliminate_copy_propagation = Transform_copr.eliminate_copy_propagation
  let eliminate_copy_propagation_program = Transform_copr.copy_propagate

  let to_rc_ir = Transform_rc.to_rc_intermediate_representation
  
  let auto_ref_count (program: Ast.program) = 
    let module StringMap = Map.Make(String) in
    (* TODO: builtins? - For example deref and assign for mutable ref *)
    let builtins = StringMap.of_list [ "ref", [RefCount.Owned] ] in
    let constants = to_rc_ir (ANF.anf program) in
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

let parse_string (s : string) =
  let lexbuf = Lexing.from_string s in
  Parser.main Lexer.read lexbuf
