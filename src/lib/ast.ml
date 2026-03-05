(** Source location tracking - re-export from Location module *)
module Loc = Location

type const = 
  | CUnit
  | CNever
  | CInt of int
  | CString of string
  | CBool of bool

type unary_op =
  | UWait | UWatch | UTail | UDelay
  | UProj of int (* proj_i for internal use *)
  (* | Inl | Inr *) (* constructors for sum type *)

type binary_op =
  | SigCons
  | BSync | BOStar | BLaterApp
  | Add | Mul | Sub | Div | Eq | Lt | Leq

(** Source location for an expression node *)
type location = Location.t

type 'a list1 = Cons1 of 'a * 'a list

type typ = 
  | TError (* typing error *)
  | TUnit 
  | TInt
  | TString
  | TBool
  | TName of string
  | TParam of string  (* type parameters 'a -> 'b ... *)
  | TVar of int (* placeholder types - think 'weak42 *)
  | TFun of typ list1 * typ
  | TTuple of typ * typ
  | TSignal of typ
  | TLater of typ
  | TDelay of typ
  | TSync of typ * typ
  | TOption of typ
  | TChan of typ

type parsed (* just parsed *)
type bound 
type typed (* typechecking *)

type scope = 
  | Scope_global
  | Scope_local of { level: int }

type _ ann =
  | Ann_parsed : Location.t -> parsed ann
  | Ann_bound : Location.t * scope -> bound ann
  | Ann_typed : Location.t * typ -> typed ann

type _ pattern =
  | PWildcard
  | PVar : string * 's ann -> 's pattern 
  | PConst : const * 's ann -> 's pattern
  | PTuple : 's pattern * 's pattern * 's ann -> 's pattern
  | PSigCons : 's pattern * 's name * 's ann -> 's pattern
  | PCtor : 's name * 's pattern list * 's ann-> 's pattern
  (* could include more complex patterns like lists, records, etc. *)

and _ expr =
  | EConst : const * 's ann -> 's expr
  | EVar : 's name -> 's expr
  | ECtor : 's name * 's expr list * 's ann -> 's expr
  | ELet : 's name * 's expr * 's expr * 's ann -> 's expr   (* variable binding *)
  | EFun : 's name list * 's expr * 's ann -> 's expr        (* Rizzo has functions with 1 parameter *)
  | EApp : 's expr * 's expr list * 's ann -> 's expr        (* Rizzo has applications with 1 arg *)
  | EUnary : unary_op * 's expr  * 's ann -> 's expr
  | EBinary : binary_op * 's expr * 's expr  * 's ann -> 's expr
  | ETuple : 's expr * 's expr  * 's ann -> 's expr
  | ECase : 's expr * 's case_branch list  * 's ann -> 's expr
  | EIfe : 's expr * 's expr * 's expr  * 's ann -> 's expr      (* if-then-else *) 
  | EAnno : 's expr * typ * 's ann -> 's expr
  (* | EPartialApp : 's expr * 's expr list  * 's ann -> 's expr *)
and 's case_branch = 's pattern * 's expr  * 's ann
and 's name = string * 's ann

type _ top_expr =
  | TopLet : 's name * 's expr  * 's ann -> 's top_expr
  (* could include types, modules idk *)

type 'stage program  = 'stage top_expr list

module ExprLocTable = Hashtbl.Make(struct
  type t = parsed expr
  let equal a b = a == b
  let hash v = Hashtbl.hash (Obj.magic v : int)
end)

module TopExprLocTable = Hashtbl.Make(struct
  type t = parsed top_expr
  let equal a b = a == b
  let hash v = Hashtbl.hash (Obj.magic v : int)
end)

let get_location : type stage. stage ann -> Location.t = fun a ->
  match a with
  | Ann_parsed loc -> loc
  | Ann_bound (loc, _) -> loc
  | Ann_typed (loc, _) -> loc

let ann_get_type (Ann_typed (_, t))= t

let expr_get_ann : type stage. stage expr -> stage ann = fun e ->
  match e with
  | EConst (_, ann) | EVar (_, ann) | ECtor (_, _, ann) | ELet (_, _, _, ann) | EFun (_, _, ann)
  | EApp (_, _, ann) | EUnary (_, _, ann) | EBinary (_, _, _, ann)
  | ETuple (_, _, ann) | ECase (_, _, ann) | EIfe (_, _, _, ann) 
  | EAnno (_,_, ann)-> ann

let rec pattern_bound_vars = function
  | PWildcard | PConst _ -> []
  | PVar (x, _) -> [x]
  | PSigCons (p1, p2, _) -> pattern_bound_vars p1 @ [fst p2]
  | PTuple (p1, p2, _) -> pattern_bound_vars p1 @ pattern_bound_vars p2
  | PCtor (_, ps, _) -> List.concat_map pattern_bound_vars ps

let rec eq_expr a b = 
  match a, b with
  | EConst (c1, _), EConst (c2,_) -> c1 = c2
  | EVar (x1, _), EVar (x2, _) -> x1 = x2
  | ECtor (name1, args1, _), ECtor (name2, args2, _) ->
    eq_name name1 name2 && List.length args1 = List.length args2 && List.for_all2 eq_expr args1 args2
  | ELet (x1, e1_1, e1_2, _), ELet (x2, e2_1, e2_2, _) -> eq_name x1 x2 && eq_expr e1_1 e2_1 && eq_expr e1_2 e2_2
  | EFun (params1, body1, _), EFun (params2, body2, _) -> List.for_all2 eq_name params1 params2 && eq_expr body1 body2
  | EApp (f1, args1, _), EApp (f2, args2, _) -> eq_expr f1 f2 && List.for_all2 eq_expr args1 args2
  | EUnary (op1, e1, _), EUnary (op2, e2, _) -> op1 = op2 && eq_expr e1 e2
  | EBinary (op1, e11, e12, _), EBinary (op2, e21, e22, _) -> op1 = op2 && eq_expr e11 e21 && eq_expr e12 e22
  | ETuple (e11, e12, _), ETuple (e21, e22, _) -> eq_expr e11 e21 && eq_expr e12 e22
  | ECase (e1, cases1, _), ECase (e2, cases2, _) ->
    eq_expr e1 e2
    && List.length cases1 = List.length cases2
    && List.for_all2 (fun (p1, b1, _) (p2, b2, _) -> eq_pattern p1 p2 && eq_expr b1 b2) cases1 cases2
  | EIfe (e1_1, e1_2, e1_3, _), EIfe (e2_1, e2_2, e2_3, _) ->
    eq_expr e1_1 e2_1 && eq_expr e1_2 e2_2 && eq_expr e1_3 e2_3
  | _ -> false
and eq_name (a: _ name) (b: _ name) = fst a = fst b

and eq_pattern a b =
  match a, b with
  | PWildcard, PWildcard -> true
  | PVar (x1, _), PVar (x2, _) -> x1 = x2
  | PConst (c1, _), PConst (c2, _) -> c1 = c2
  | PTuple (a1, b1, _), PTuple (a2, b2, _) -> eq_pattern a1 a2 && eq_pattern b1 b2
  | PSigCons (a1, b1, _), PSigCons (a2, b2, _) -> eq_pattern a1 a2 && eq_name b1 b2
  | PCtor (name1, args1, _), PCtor (name2, args2, _) ->
    eq_name name1 name2 && List.length args1 = List.length args2 && List.for_all2 eq_pattern args1 args2
  | _ -> false
and eq_typ a b = match a,b with
  | TUnit, TUnit | TInt, TInt | TString, TString | TBool, TBool -> true
  | TName n1, TName n2 -> String.equal n1 n2
  | TParam p1, TParam p2 -> String.equal p1 p2
  | TVar v1, TVar v2 -> v1 = v2
  | TFun (Cons1 (p1, p_rest1), r1), TFun (Cons1 (p2, p_rest2), r2) ->
    eq_typ p1 p2 && eq_typ r1 r2 && List.length p_rest1 = List.length p_rest2 && List.for_all2 eq_typ p_rest1 p_rest2
  | TTuple (a1, b1), TTuple (a2, b2) | TSync(a1, b1), TSync(a2,b2) -> eq_typ a1 a2 && eq_typ b1 b2
  | TSignal t1, TSignal t2 | TLater t1, TLater t2 | TDelay t1, TDelay t2 -> eq_typ t1 t2
  | _ -> false

let pp_const out = function
  | CUnit -> Format.fprintf out "()"
  | CNever -> Format.fprintf out "@{<blue>never@}"
  | CInt i -> Format.fprintf out "@{<ligtgreen>%d@}" i
  | CString s -> Format.fprintf out "@{<orange>\"%s\"@}" s
  | CBool b -> Format.fprintf out "@{<blue>%b@}" b

let rec pp_pattern out = function
  | PWildcard -> Format.fprintf out "@{<lightcyan>_@}"
  | PVar (x, _) -> Format.fprintf out "@{<lightcyan>%s@}" x
  | PConst (c, _) -> pp_const out c
  | PTuple (p1, p2, _) -> Format.fprintf out "(%a, %a)" pp_pattern p1 pp_pattern p2
  | PSigCons (p1, p2, _) -> Format.fprintf out "(%a :: @{<lightcyan>%s@})" pp_pattern p1 (fst p2)
  | PCtor (name, args, _) -> 
    if List.length args = 0 then Format.fprintf out "@{<green>%s@}" (fst name)
    else Format.fprintf out "@{<green>%s@}(%a)" (fst name) (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out ", ") pp_pattern) args

let rec pp_case_branch out (p, b, _ : _ case_branch) =
  let open Format in
  fprintf out "@{<blue>|@} %a -> @[<hov 2>%a@]" pp_pattern p pp_expr b

and pp_expr out = 
  let open Format in
  function
  | EConst (c,_) -> pp_const out c
  | EVar (x, _) -> fprintf out "@{<lightcyan>%s@}" x
  | ECtor (name, args, _) ->
    if List.length args = 0 then fprintf out "@{<green>%s@}" (fst name)
    else
      fprintf out "%s(%a)" (fst name)
        (pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ") pp_expr) args
  | ELet ((x, _), e1, e2, _) ->
    fprintf out "@[<v 0>@{<magenta>let@} @{<lightcyan>%s@} = %a @{<magenta>in@}@,%a@]" x pp_expr e1 pp_expr e2
  | EFun (names, body, _) ->
    let params = List.map fst names in
    fprintf out "@[<v 2>@{<magenta>fun@} (@{<lightcyan>%s@}) ->@,%a@]" (String.concat ", " params) pp_expr body
  | EApp (f, args, _) ->
    fprintf out "@[<hov 2>%a(@[<hov>%a@])@]"
      pp_expr f
      (pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ") pp_expr) args
  | EUnary (op, e, _) -> 
    let op_str = string_of_unary_op op in
    fprintf out "@[<hov 2>@{<yellow>%s@}@ %a@]" op_str pp_expr e
  | EBinary (op, e1, e2, _) ->
    let op_str = string_of_binary_op op in
    fprintf out "@[<hov 2>(%a@ @{<yellow>%s@}@ %a)@]" pp_expr e1 op_str pp_expr e2
  | ETuple (e1, e2, _) -> fprintf out "@[<hov>(%a,@ %a)@]" pp_expr e1 pp_expr e2
  | EIfe (e1, e2, e3, _) ->
    fprintf out "@[<v 2>(@{<magenta>if@} %a@ @{<magenta>then@}@ %a@ @{<magenta>else@}@ %a)@]" pp_expr e1 pp_expr e2 pp_expr e3
  | ECase (e, cases, _) ->
    fprintf out "@[<v 0>(@{<magenta>match@} %a @{<magenta>with@}@,%a)@]"
      pp_expr e
      (pp_print_list ~pp_sep:(fun out () -> fprintf out "@,") pp_case_branch)
      cases
  | EAnno (e, typ, _) -> fprintf out "(%a : %a)" pp_expr e pp_typ typ
and string_of_binary_op = function 
  | SigCons -> "::" 
  | BSync -> "sync"
  | BOStar -> "(*)"
  | BLaterApp -> "|>"
  | Add -> "+"
  | Mul -> "*"
  | Sub -> "-"
  | Div -> "/"
  | Eq -> "=="
  | Lt -> "<"
  | Leq -> "<="
and string_of_unary_op = function
  | UWait -> "wait"
  | UWatch -> "watch"
  | UTail -> "tail"
  | UDelay -> "delay"
  | UProj i -> Printf.sprintf "proj_%d" i

and pp_typ fmt = 
  let open Format in 
  function 
  | TError -> fprintf fmt "@{<red>*Error-type*@}"
  | TBool -> fprintf fmt "@{<green>Bool@}"
  | TInt -> fprintf fmt "@{<green>Int@}"
  | TString -> fprintf fmt "@{<green>String@}"
  | TUnit -> fprintf fmt "@{<green>Unit@}"
  | TFun (Cons1(t1, []), t) -> fprintf fmt "(%a -> %a)" pp_typ t1 pp_typ t
  | TFun (Cons1(t1, ts), t) -> fprintf fmt "(%a -> %a -> %a)" pp_typ t1 (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " -> ") pp_typ) ts pp_typ t
  | TDelay t -> fprintf fmt "(@{<green>Delay@} %a)" pp_typ t
  | TLater t -> fprintf fmt "(@{<green>Later@} %a)" pp_typ t
  | TName n -> fprintf fmt "@{<green>%s@}" n
  | TParam n -> fprintf fmt "@{<green>%s@}" n
  | TSignal t -> fprintf fmt "(@{<green>Signal@} %a)" pp_typ t
  | TTuple (t1, t2) -> fprintf fmt ("(%a * %a)") pp_typ t1 pp_typ t2
  | TSync (t1, t2) -> fprintf fmt ("@{<green>Sync@}(%a, %a)") pp_typ t1 pp_typ t2
  | TOption t -> fprintf fmt "(@{<green>Option@} %a)" pp_typ t
  | TChan t -> fprintf fmt "(@{<green>Chan@} %a)" pp_typ t
  | TVar i -> fprintf fmt "@{<green>`weak%d@}" i

let eq_top_expr a b = match a,b with
  | TopLet (x1, e1, _), TopLet (x2, e2, _) -> eq_name x1 x2 && eq_expr e1 e2 

let pp_top_expr out =
  function
  | TopLet ((x, _), e, _) -> Format.fprintf out "@[<v 2>@{<magenta>let@} @{<lightcyan>%s@} =@,%a@]" x pp_expr e

let eq_program a b = 
  List.length a = List.length b && List.for_all2 eq_top_expr a b

let pp_program out (p: _ program) =
  let open Format in
  fprintf out "@[<v>%a@]"
    (pp_print_list ~pp_sep:(fun out () -> fprintf out "@.@.") pp_top_expr)
    p

let rec pp_typed_program out (p: typed program) =
  let open Format in
  fprintf out "@[<v>%a@]"
    (pp_print_list ~pp_sep:(fun out () -> fprintf out "@.@.") pp_typed_top_expr)
    p
and pp_typed_top_expr out =
  function
  | TopLet ((x, _), e, Ann_typed (_, t)) ->
    Format.fprintf out "@[<v 2>@{<magenta>let@} @{<lightcyan>%s@} : %a =@,%a@]" x pp_typ t pp_typed_expr e
and pp_typed_expr out = 
  let open Format in
  function
  | EConst (c, _) -> Format.fprintf out "%a" pp_const c
  | EVar (x, _) -> Format.fprintf out "@{<lightcyan>%s@}" x
  | ECtor (name, args, _) ->
    if List.length args = 0 then Format.fprintf out "@{<green>%s@}" (fst name)
    else
      Format.fprintf out "@{<green>%s@}(@[<hov>%a@])" (fst name)
        (pp_print_list ~pp_sep:(fun out () -> Format.fprintf out ",@ ") pp_typed_expr) args
  | ELet ((x, _), e1, e2, _) ->
    Format.fprintf out "@[<v 0>@{<magenta>let@} @{<lightcyan>%s@} = %a @{<magenta>in@}@,%a@]" x pp_typed_expr e1 pp_typed_expr e2
  | EFun (names, body, Ann_typed (_, t)) ->
    let params = List.map fst names in
    Format.fprintf out "@[<v 2>@{<magenta>fun@} (@{<lightcyan>%s@}) : %a @{<magenta>->@}@,%a@]" (String.concat ", " params) pp_typ t pp_typed_expr body
  | EApp (f, args, _) ->
    Format.fprintf out "@[<hov 2>%a(@[<hov>%a@])@]"
      pp_typed_expr f
      (pp_print_list ~pp_sep:(fun out () -> Format.fprintf out ",@ ") pp_typed_expr) args
  | EUnary (op, e, _) -> 
    let op_str = string_of_unary_op op in
    Format.fprintf out "@[<hov 2>@{<yellow>%s@}@ %a@]" op_str pp_typed_expr e
  | EBinary (op, e1, e2, _) ->
    let op_str = string_of_binary_op op in
    Format.fprintf out "@[<hov 2>(%a@ @{<yellow>%s@}@ %a)@]" pp_typed_expr e1 op_str pp_typed_expr e2
  | ETuple (e1, e2, _) -> Format.fprintf out "@[<hov>(%a,@ %a)@]" pp_typed_expr e1 pp_typed_expr e2
  | EIfe (e1, e2, e3, _) ->
    Format.fprintf out "@[<v 0>@{<magenta>if@} %a@,@{<magenta>then@} %a@,@{<magenta>else@} %a@]" pp_typed_expr e1 pp_typed_expr e2 pp_typed_expr e3
  | ECase (e, cases, _) ->
    Format.fprintf out "@[<v 0>@{<magenta>match@} %a @{<magenta>with@}@,%a@]"
      pp_typed_expr e
      (pp_print_list ~pp_sep:(fun out () -> Format.fprintf out "@,") pp_typed_case_branch)
      cases
  | EAnno (e, typ, _) -> Format.fprintf out "(@{<green>%a@} : %a)" pp_typed_expr e pp_typ typ
and pp_typed_case_branch out (p, b, _ : _ case_branch) =
  let open Format in
  fprintf out "@{<blue>|@} %a -> %a" pp_pattern p pp_typed_expr b