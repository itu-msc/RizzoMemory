val insert_dec_many : Refcount_core.primitive list -> Refcount_core.fn_body -> Refcount_core.beta_env -> Refcount_core.fn_body
val insert_rc : Refcount_core.fn_body -> Refcount_core.beta_env -> Refcount_core.parameter_ownership -> Refcount_core.fn_body

val insert_owned_partial_app_wrapper :
  Refcount_core.parameter_ownership ->
  int Collections.StringMap.t ->
  Refcount_core.program ->
  Refcount_core.program * Refcount_core.parameter_ownership

val insert_reset_and_reuse_pairs_program : Refcount_core.program -> Refcount_core.program
val insert_reset_and_reuse_pairs_fn : Refcount_core.fn_body -> Refcount_core.fn_body
val insert_reset : string -> int -> Refcount_core.fn_body -> Refcount_core.fn_body
val insert_reuse : string -> int -> Refcount_core.fn_body -> Refcount_core.fn_body

val reference_count_program :
  Refcount_core.parameter_ownership ->
  int Collections.StringMap.t ->
  Refcount_core.program ->
  Refcount_core.parameter_ownership * Refcount_core.program