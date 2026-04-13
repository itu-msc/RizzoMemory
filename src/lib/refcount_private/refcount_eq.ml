open! Refcount_core

let rec eq_program (RefProg { functions = f1; globals = g1 } : program) (RefProg { functions = f2; globals = g2 } : program) =
  List.length f1 = List.length f2
  && List.length g1 = List.length g2
  && List.for_all2 eq_fn f1 f2
  && List.for_all2 (fun (n1, b1) (n2, b2) -> n1 = n2 && eq_fnbody b1 b2) g1 g2

and eq_fn (n1, Fun (p1, b1)) (n2, Fun (p2, b2)) =
  n1 = n2
  && List.for_all2 String.equal p1 p2
  && eq_fnbody b1 b2

and eq_fnbody f1 f2 =
  match f1, f2 with
  | FnRet p1, FnRet p2 -> p1 = p2
  | FnLet (x1, r1, f1), FnLet (x2, r2, f2) -> x1 = x2 && r1 = r2 && eq_fnbody f1 f2
  | FnCase (x1, fs1), FnCase (x2, fs2) ->
      x1 = x2
      && List.length fs1 = List.length fs2
      && List.for_all2 (fun arm1 arm2 -> arm1.tag = arm2.tag && arm1.num_fields = arm2.num_fields && eq_fnbody arm1.body arm2.body) fs1 fs2
  | FnInc (x1, f1), FnInc (x2, f2) -> x1 = x2 && eq_fnbody f1 f2
  | FnDec (x1, f1), FnDec (x2, f2) -> x1 = x2 && eq_fnbody f1 f2
  | _ -> false

and eq_rexpr a b =
  match a, b with
  | RCall (c1, ps1), RCall (c2, ps2) -> c1 = c2 && ps1 = ps2
  | RPartialApp (c1, ps1), RPartialApp (c2, ps2) -> c1 = c2 && ps1 = ps2
  | RVarApp (x1, p1), RVarApp (x2, p2) -> x1 = x2 && p1 = p2
  | RCtor Ctor { tag = t1; fields = f1 }, RCtor Ctor { tag = t2; fields = f2 } -> t1 = t2 && f1 = f2
  | RCtor Signal { head = h1; tail = t1 }, RCtor Signal { head = h2; tail = t2 } -> h1 = h2 && t1 = t2
  | RProj (i1, x1), RProj (i2, x2) -> i1 = i2 && x1 = x2
  | RReset x1, RReset x2 -> x1 = x2
  | RReuse (x1, c1), RReuse (x2, c2) -> x1 = x2 && eq_rexpr_ctor c1 c2
  | _ -> false

and eq_rexpr_ctor a b =
  match a, b with
  | Ctor { tag = t1; fields = f1 }, Ctor { tag = t2; fields = f2 } -> t1 = t2 && f1 = f2
  | Signal { head = h1; tail = t1 }, Signal { head = h2; tail = t2 } -> h1 = h2 && t1 = t2
  | _ -> false