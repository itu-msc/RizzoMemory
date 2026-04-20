open! Ast_core

let pp_const out = function
  | CUnit -> Format.fprintf out "()"
  | CNever -> Format.fprintf out "@{<blue>never@}"
  | CInt i -> Format.fprintf out "@{<ligtgreen>%d@}" i
  | CString s -> Format.fprintf out "@{<orange>\"%s\"@}" s
  | CBool b -> Format.fprintf out "@{<blue>%b@}" b

let rec pp_pattern out = function
  | PWildcard _ -> Format.fprintf out "@{<lightcyan>_@}"
  | PVar (x, _) -> Format.fprintf out "@{<lightcyan>%s@}" x
  | PConst (c, _) -> pp_const out c
  | PTuple (p1, p2, _) -> Format.fprintf out "(%a, %a)" pp_pattern p1 pp_pattern p2
  | PSigCons (p1, p2, _) -> Format.fprintf out "(%a :: @{<lightcyan>%s@})" pp_pattern p1 (fst p2)
  | PStringCons (p1, p2, _) -> Format.fprintf out "(%a :: @{<lightcyan>%s@})" pp_pattern p1 (fst p2)
  | PCtor (name, args, _) ->
    if List.length args = 0 then Format.fprintf out "@{<green>%s@}" (fst name)
    else Format.fprintf out "@{<green>%s@}(%a)" (fst name) (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out ", ") pp_pattern) args

let rec pp_case_branch out (p, b, _ : _ case_branch) =
  let open Format in
  fprintf out "@{<blue>|@} %a -> @[<hov 2>%a@]" pp_pattern p pp_expr b

and pp_expr out =
  let open Format in
  function
  | EConst (c, _) -> pp_const out c
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

and pp_typ fmt =
  let open Format in
  function
  | TError -> fprintf fmt "@{<red>*Error-type*@}"
  | TBool -> fprintf fmt "@{<green>Bool@}"
  | TInt -> fprintf fmt "@{<green>Int@}"
  | TString -> fprintf fmt "@{<green>String@}"
  | TUnit -> fprintf fmt "@{<green>Unit@}"
  | TFun (Cons1 (t1, []), t) -> fprintf fmt "(%a -> %a)" pp_typ t1 pp_typ t
  | TFun (Cons1 (t1, ts), t) -> fprintf fmt "(%a -> %a -> %a)" pp_typ t1 (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " -> ") pp_typ) ts pp_typ t
  | TDelay t -> fprintf fmt "(@{<green>Delay@} %a)" pp_typ t
  | TLater t -> fprintf fmt "(@{<green>Later@} %a)" pp_typ t
  | TName n -> fprintf fmt "@{<green>%s@}" n
  | TParam n -> fprintf fmt "@{<green>%s@}" n
  | TSignal t -> fprintf fmt "(@{<green>Signal@} %a)" pp_typ t
  | TTuple (t1, t2) -> fprintf fmt "(%a * %a)" pp_typ t1 pp_typ t2
  | TChan t -> fprintf fmt "(@{<green>Chan@} %a)" pp_typ t
  | TVar i -> fprintf fmt "@{<green>`weak%d@}" i
  | TApp (t, ts) -> fprintf fmt "@{<green>%a@} @[<hov>%a@]" pp_typ t (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") pp_typ) ts

let pp_top_expr out = function
  | TopLet ((x, _), e, _) -> Format.fprintf out "@[<v 2>@{<magenta>let@} @{<lightcyan>%s@} =@,%a@]" x pp_expr e
  | TopTypeDef ((tname,_), params, ctors, _) ->
    Format.fprintf out "@[<v 2>@{<magenta>type@} @{<green>%s@}@{<lightcyan>%a@} =@,%a@]"
      tname
      (Format.pp_print_list ~pp_sep:Format.pp_print_nothing (fun out (param, _) -> Format.fprintf out " @{<lightcyan>%s@}" param))
      params
      (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out "@,") (fun out ((ctor_name,_), arg_types, _) ->
         Format.fprintf out "@{<green>%s@}(@[<hov>%a@])" ctor_name
           (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out ", ") pp_typ) arg_types))
      ctors

let pp_program out (p : _ program) =
  let open Format in
  fprintf out "@[<v>%a@]"
    (pp_print_list ~pp_sep:(fun out () -> fprintf out "@.@.") pp_top_expr)
    p

let rec pp_typed_program out (p : typed program) =
  let open Format in
  fprintf out "@[<v>%a@]"
    (pp_print_list ~pp_sep:(fun out () -> fprintf out "@.@.") pp_typed_top_expr)
    p

and pp_typed_top_expr out : typed top_expr -> unit = function
  | TopLet ((x, _), e, Ann_typed (_, t)) ->
    Format.fprintf out "@[<v 2>@{<magenta>let@} @{<lightcyan>%s@} : %a =@,%a@]" x pp_typ t pp_typed_expr e
  | TopLet ((x, _), e, _) ->
    Format.fprintf out "@[<v 2>@{<magenta>let@} @{<lightcyan>%s@} =@,%a@]" x pp_expr e
  | TopTypeDef _ as e -> 
    Format.fprintf out "%a" pp_top_expr e

and pp_typed_expr out : typed expr -> unit =
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
  | e -> pp_expr out e

and pp_typed_case_branch out ((p, b, _) : typed case_branch) =
  let open Format in
  fprintf out "@{<blue>|@} %a -> %a" pp_pattern p pp_typed_expr b