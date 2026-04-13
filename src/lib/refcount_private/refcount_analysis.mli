val set_global_names : Refcount_core.parameter_ownership -> Refcount_core.program -> unit
val free_vars : Refcount_core.fn_body -> Refcount_core.StringSet.t
val free_vars_rexpr : Refcount_core.rexpr -> Refcount_core.StringSet.t
val collect : Refcount_core.parameter_ownership -> Refcount_core.fn_body -> Refcount_core.StringSet.t
val infer_all_simple : Refcount_core.program -> Refcount_core.parameter_ownership
val infer_all : ?builtins:Refcount_core.parameter_ownership -> Refcount_core.program -> Refcount_core.parameter_ownership