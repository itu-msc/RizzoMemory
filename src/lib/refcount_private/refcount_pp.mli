val pp_primitive : Format.formatter -> Refcount_core.primitive -> unit
val pp_rexpr : Format.formatter -> Refcount_core.rexpr -> unit
val pp_fnbody : Format.formatter -> Refcount_core.fn_body -> unit

val pp_rcfun :
  ?ownerships:Refcount_core.parameter_ownership option ->
  Format.formatter ->
  string * Refcount_core.rc_fun ->
  unit

val pp_ref_counted_program :
  ?ownerships:Refcount_core.parameter_ownership option ->
  Format.formatter ->
  Refcount_core.program ->
  unit