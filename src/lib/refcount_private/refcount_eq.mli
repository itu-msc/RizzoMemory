val eq_program : Refcount_core.program -> Refcount_core.program -> bool
val eq_fn : string * Refcount_core.rc_fun -> string * Refcount_core.rc_fun -> bool
val eq_fnbody : Refcount_core.fn_body -> Refcount_core.fn_body -> bool
val eq_rexpr : Refcount_core.rexpr -> Refcount_core.rexpr -> bool
val eq_rexpr_ctor : Refcount_core.ctor -> Refcount_core.ctor -> bool