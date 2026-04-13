include Refcount_private.Refcount_core

module Core = Refcount_private.Refcount_core
module Pp = Refcount_private.Refcount_pp
module Eq = Refcount_private.Refcount_eq
module Analysis = Refcount_private.Refcount_analysis
module Insert = Refcount_private.Refcount_insert

let pp_primitive = Refcount_private.Refcount_pp.pp_primitive
let pp_rexpr = Refcount_private.Refcount_pp.pp_rexpr
let pp_fnbody = Refcount_private.Refcount_pp.pp_fnbody
let pp_rcfun = Refcount_private.Refcount_pp.pp_rcfun
let pp_ref_counted_program = Refcount_private.Refcount_pp.pp_ref_counted_program

let eq_program = Refcount_private.Refcount_eq.eq_program
let eq_fn = Refcount_private.Refcount_eq.eq_fn
let eq_fnbody = Refcount_private.Refcount_eq.eq_fnbody
let eq_rexpr = Refcount_private.Refcount_eq.eq_rexpr
let eq_rexpr_ctor = Refcount_private.Refcount_eq.eq_rexpr_ctor

let free_vars = Refcount_private.Refcount_analysis.free_vars
let free_vars_rexpr = Refcount_private.Refcount_analysis.free_vars_rexpr
let collect = Refcount_private.Refcount_analysis.collect
let infer_all_simple = Refcount_private.Refcount_analysis.infer_all_simple
let infer_all = Refcount_private.Refcount_analysis.infer_all

let insert_dec_many = Refcount_private.Refcount_insert.insert_dec_many
let insert_rc = Refcount_private.Refcount_insert.insert_rc
let insert_reset_and_reuse_pairs_program = Refcount_private.Refcount_insert.insert_reset_and_reuse_pairs_program
let insert_reset_and_reuse_pairs_fn = Refcount_private.Refcount_insert.insert_reset_and_reuse_pairs_fn
let insert_reset = Refcount_private.Refcount_insert.insert_reset
let insert_reuse = Refcount_private.Refcount_insert.insert_reuse
let reference_count_program = Refcount_private.Refcount_insert.reference_count_program
