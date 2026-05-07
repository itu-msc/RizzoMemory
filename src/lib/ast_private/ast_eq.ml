open! Ast_core

let rec eq_expr a b =
  match a, b with
  | EConst (c1, _), EConst (c2, _) -> c1 = c2
  | EVar (x1, _), EVar (x2, _) -> x1 = x2
  | ECtor (name1, args1, _), ECtor (name2, args2, _) ->
    eq_name name1 name2 && List.length args1 = List.length args2 && List.for_all2 eq_expr args1 args2
  | ELet (x1, e1_1, e1_2, _), ELet (x2, e2_1, e2_2, _) -> eq_name x1 x2 && eq_expr e1_1 e2_1 && eq_expr e1_2 e2_2
  | EFun (params1, body1, _), EFun (params2, body2, _) -> List.for_all2 eq_pattern params1 params2 && eq_expr body1 body2
  | EApp (f1, args1, _), EApp (f2, args2, _) -> eq_expr f1 f2 && List.for_all2 eq_expr args1 args2
  | EUnary (op1, e1, _), EUnary (op2, e2, _) -> op1 = op2 && eq_expr e1 e2
  | EBinary (op1, e11, e12, _), EBinary (op2, e21, e22, _) -> op1 = op2 && eq_expr e11 e21 && eq_expr e12 e22
  | ETuple (e11, e12, es1, _), ETuple (e21, e22, es2, _) -> 
    eq_expr e11 e21 && eq_expr e12 e22
    && List.length es1 = List.length es2 
    && List.for_all2 eq_expr es1 es2
  | ECase (e1, cases1, _), ECase (e2, cases2, _) ->
    eq_expr e1 e2
    && List.length cases1 = List.length cases2
    && List.for_all2 (fun (p1, b1, _) (p2, b2, _) -> eq_pattern p1 p2 && eq_expr b1 b2) cases1 cases2
  | EIfe (e1_1, e1_2, e1_3, _), EIfe (e2_1, e2_2, e2_3, _) ->
    eq_expr e1_1 e2_1 && eq_expr e1_2 e2_2 && eq_expr e1_3 e2_3
  | _ -> false

and eq_name (a : _ name) (b : _ name) = fst a = fst b

and eq_pattern a b =
  match a, b with
  | PWildcard _, PWildcard _ -> true
  | PVar (x1, _), PVar (x2, _) -> x1 = x2
  | PConst (c1, _), PConst (c2, _) -> c1 = c2
  | PTuple (a1, b1, cs1, _), PTuple (a2, b2, cs2, _) -> 
    eq_pattern a1 a2 && eq_pattern b1 b2
    && List.length cs1 = List.length cs2
    && List.for_all2 eq_pattern cs1 cs2
  | PSigCons (a1, b1, _), PSigCons (a2, b2, _) -> eq_pattern a1 a2 && eq_name b1 b2
  | PStringCons (a1, b1, _), PStringCons (a2, b2, _) -> eq_pattern a1 a2 && eq_name b1 b2
  | PCtor (name1, args1, _), PCtor (name2, args2, _) ->
    eq_name name1 name2 && List.length args1 = List.length args2 && List.for_all2 eq_pattern args1 args2
  | _ -> false

and eq_typ a b =
  match a, b with
  | TUnit, TUnit | TInt, TInt | TString, TString | TBool, TBool -> true
  | TName n1, TName n2 -> String.equal n1 n2
  | TParam p1, TParam p2 -> String.equal p1 p2
  | TVar v1, TVar v2 -> v1 = v2
  | TFun (Cons1 (p1, p_rest1), r1), TFun (Cons1 (p2, p_rest2), r2) ->
    eq_typ p1 p2 && eq_typ r1 r2 && List.length p_rest1 = List.length p_rest2 && List.for_all2 eq_typ p_rest1 p_rest2
  | TTuple (a1, b1, cs1), TTuple (a2, b2, cs2) -> 
    eq_typ a1 a2 && eq_typ b1 b2
    && List.length cs1 = List.length cs2
    && List.for_all2 eq_typ cs1 cs2
  | TSignal t1, TSignal t2
  | TLater t1, TLater t2
  | TDelay t1, TDelay t2
  | TChan t1, TChan t2 -> eq_typ t1 t2
  | TApp (t1, ts1), TApp (t2, ts2) ->
    eq_typ t1 t2 && List.length ts1 = List.length ts2 && List.for_all2 eq_typ ts1 ts2
  | _ -> false

let eq_top_expr a b =
  match a, b with
  | TopLet (x1, e1, _), TopLet (x2, e2, _) -> eq_name x1 x2 && eq_expr e1 e2
  | TopTypeDef (name1, params1, ctors1, _), TopTypeDef (name2, params2, ctors2, _) ->
    eq_name name1 name2
    && List.length params1 = List.length params2
    && List.for_all2 eq_name params1 params2
    && List.length ctors1 = List.length ctors2
    && List.for_all2 (fun (ctor_name1, arg_types1, _) (ctor_name2, arg_types2, _) ->
         eq_name ctor_name1 ctor_name2
         && List.length arg_types1 = List.length arg_types2
         && List.for_all2 eq_typ arg_types1 arg_types2) ctors1 ctors2
  | _ -> false

let eq_program a b =
  List.length a = List.length b && List.for_all2 eq_top_expr a b