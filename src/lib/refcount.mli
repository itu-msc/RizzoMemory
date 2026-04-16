include module type of Refcount_private.Refcount_core
include module type of Refcount_private.Refcount_eq
include module type of Refcount_private.Refcount_pp

module Core = Refcount_private.Refcount_core
module Pp = Refcount_private.Refcount_pp
module Eq = Refcount_private.Refcount_eq
module Analysis = Refcount_private.Refcount_analysis
module Insert = Refcount_private.Refcount_insert

val free_vars : fn_body -> Collections.StringSet.t
val free_vars_rexpr : rexpr -> Collections.StringSet.t
val collect : parameter_ownership -> fn_body -> Collections.StringSet.t
val infer_all_simple : program -> parameter_ownership
val infer_all : ?builtins:parameter_ownership -> program -> parameter_ownership

val insert_dec_many : primitive list -> fn_body -> beta_env -> fn_body
val insert_rc : fn_body -> beta_env -> parameter_ownership -> fn_body
val insert_reset_and_reuse_pairs_program : program -> program
val insert_reset_and_reuse_pairs_fn : fn_body -> fn_body
val insert_reset : string -> int -> fn_body -> fn_body
val insert_reuse : string -> int -> fn_body -> fn_body
val reference_count_program : parameter_ownership -> int Collections.StringMap.t -> program -> parameter_ownership * program