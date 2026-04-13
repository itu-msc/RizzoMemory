open! Refcount_core

let pp_primitive out = function
  | Var x -> Format.fprintf out "@{<lightcyan>%s@}" x
  | Const c -> Ast.pp_const out c

let pp_rexpr out =
  let comma_separated pp out = Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out ", ") pp out in
  function
  | RConst c -> pp_primitive out (Const c)
  | RCall (c, ys) ->
      Format.fprintf out "@{<green>%s@}(%a)" c (comma_separated pp_primitive) ys
  | RPartialApp (c, ys) ->
      Format.fprintf out "@{<magenta>pap@} @{<yellow>%s@}(%a)" c (comma_separated pp_primitive) ys
  | RVarApp (x, y) ->
      Format.fprintf out "@{<lightcyan>%s@} %a" x pp_primitive y
  | RCtor Ctor { tag; fields } ->
      Format.fprintf out "@{<green>Ctor%d@}(%a)" tag (comma_separated pp_primitive) fields
  | RCtor Signal { head; tail } ->
      Format.fprintf out "@{<green>Signal@}(%a, %a)" pp_primitive head pp_primitive tail
  | RProj (i, x) ->
      Format.fprintf out "@{<yellow>proj_%d@} @{<lightcyan>%s@}" i x
  | RReset x ->
      Format.fprintf out "@{<magenta>reset@} @{<lightcyan>%s@}" x
  | RReuse (x, Ctor { tag; fields }) ->
      Format.fprintf out "@{<magenta>reuse@} @{<lightcyan>%s@} @{<magenta>in@} @{<green>Ctor%d@}(%a)"
        x tag (comma_separated pp_primitive) fields
  | RReuse (x, Signal { head; tail }) ->
      Format.fprintf out "@{<magenta>reuse@} @{<lightcyan>%s@} @{<magenta>in@} @{<green>Signal@}(%a, %a)"
        x pp_primitive head pp_primitive tail

let rec pp_fnbody out = function
  | FnRet x -> Format.fprintf out "@{<magenta>ret@} %a" pp_primitive x
  | FnLet (y, r, f) ->
      Format.fprintf out "@[<v 0>@{<magenta>let@} @{<lightcyan>%s@} = %a @{<magenta>in@}@,%a@]" y pp_rexpr r pp_fnbody f
  | FnCase (x, fs) ->
      let pp_branch out { tag; body; _ } =
        match tag with
        | Some tag ->
            Format.fprintf out "@{<blue>|@} @{<yellow>#%d@} @[<hov 2>%a@]" tag pp_fnbody body
        | None ->
            Format.fprintf out "@{<blue>|@} @{<magenta>default@} @[<hov 2>%a@]" pp_fnbody body
      in
      Format.fprintf out "@[<v 0>@{<magenta>match@} @{<lightcyan>%s@} @{<magenta>with@}@,%a@]" x
        (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out "@,") pp_branch) fs
  | FnInc (x, f) -> Format.fprintf out "@[<v 0>@{<magenta>inc@} @{<lightcyan>%s@};@,%a@]" x pp_fnbody f
  | FnDec (x, f) -> Format.fprintf out "@[<v 0>@{<magenta>dec@} @{<lightcyan>%s@};@,%a@]" x pp_fnbody f

let pp_rcfun ?ownerships:(own = None) out (name, Fun (params, body)) =
  let pp_ownership out o =
    match o with
    | Owned -> Format.fprintf out "@{<blue>Owned@}"
    | Borrowed -> Format.fprintf out "@{<orange>Borrowed@}"
  in
  let pp_params_with_ownership out (own : parameter_ownership) =
    let own_list = StringMap.find_opt name own |> Option.value ~default:(List.map (fun _ -> Owned) params) in
    let params_with_ownership = List.combine params own_list in
    Format.fprintf out "%a"
      (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out ", ")
         (fun out (p, o) -> Format.fprintf out "@{<lightcyan>%s@}: %a" p pp_ownership o))
      params_with_ownership
  in
  Format.fprintf out "%a\n@{<magenta>fun@} @{<yellow>%s@}(%a) =@,  @[<v>%a@]"
    (Format.pp_print_option pp_params_with_ownership) own
    name
    (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out ", ") (fun out p -> Format.fprintf out "@{<lightcyan>%s@}" p))
    params
    pp_fnbody body

let[@warning "-32"] pp_ref_counted_program ?ownerships:(own = None) out (RefProg { functions; globals } : program) =
  let pp_named_fnbody out (name, body) = Format.fprintf out "@{<magenta>let@} @{<lightcyan>%s@} = %a" name pp_fnbody body in
  Format.fprintf out "%a%a"
    (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out "@\n\n") pp_named_fnbody) globals
    (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out "@\n\n") (pp_rcfun ~ownerships:own)) functions

let rec pp_fnbody out = function
  | FnRet x -> Format.fprintf out "@{<magenta>ret@} %a" pp_primitive x
  | FnLet (y, r, f) ->
      Format.fprintf out "@[<v 0>@{<magenta>let@} @{<lightcyan>%s@} = %a @{<magenta>in@}@,%a@]" y pp_rexpr r pp_fnbody f
  | FnCase (x, fs) ->
      let pp_branch out { tag; body; _ } =
        match tag with
        | Some tag ->
            Format.fprintf out "@{<blue>|@} @{<yellow>#%d@} @[<hov 2>%a@]" tag pp_fnbody body
        | None ->
            Format.fprintf out "@{<blue>|@} @{<magenta>default@} @[<hov 2>%a@]" pp_fnbody body
      in
      Format.fprintf out "@[<v 0>@{<magenta>match@} @{<lightcyan>%s@} @{<magenta>with@}@,%a@]" x
        (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out "@,") pp_branch) fs
  | FnInc (x, f) -> Format.fprintf out "@[<v 0>@{<magenta>inc@} @{<lightcyan>%s@};@,%a@]" x pp_fnbody f
  | FnDec (x, f) -> Format.fprintf out "@[<v 0>@{<magenta>dec@} @{<lightcyan>%s@};@,%a@]" x pp_fnbody f

let pp_rcfun ?ownerships:(own = None) out (name, Fun (params, body)) =
  let pp_ownership out o =
    match o with
    | Owned -> Format.fprintf out "@{<blue>Owned@}"
    | Borrowed -> Format.fprintf out "@{<orange>Borrowed@}"
  in
  let pp_params_with_ownership out (own : parameter_ownership) =
    let own_list = StringMap.find_opt name own |> Option.value ~default:(List.map (fun _ -> Owned) params) in
    let params_with_ownership = List.combine params own_list in
    Format.fprintf out "%a"
      (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out ", ")
         (fun out (p, o) -> Format.fprintf out "@{<lightcyan>%s@}: %a" p pp_ownership o))
      params_with_ownership
  in
  Format.fprintf out "%a\n@{<magenta>fun@} @{<yellow>%s@}(%a) =@,  @[<v>%a@]"
    (Format.pp_print_option pp_params_with_ownership) own
    name
    (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out ", ") (fun out p -> Format.fprintf out "@{<lightcyan>%s@}" p))
    params
    pp_fnbody body

let pp_ref_counted_program ?ownerships:(own = None) out (RefProg { functions; globals } : program) =
  let pp_named_fnbody out (name, body) = Format.fprintf out "@{<magenta>let@} @{<lightcyan>%s@} = %a" name pp_fnbody body in
  Format.fprintf out "%a%a"
    (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out "@\n\n") pp_named_fnbody) globals
    (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out "@\n\n") (pp_rcfun ~ownerships:own)) functions