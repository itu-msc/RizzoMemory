open! Rizzoc
open Rizzoc.RefCount

let () =
  parse_string "" |> ignore

let c = "c"
let y = "y"
let z = "z"

let () =
  let beta_vars:beta_env = StringMap.of_list [(y, Owned)] in
  let _ = beta_global := StringMap.of_list [(c, [Owned; Owned])] in
  let example = FnLet (z, (RCall (c, [y; y])), FnRet (z)) in
  Format.printf "Before: %a\n" pp_fnbody example;
  let with_rc = insert_rc example beta_vars in
  assert (with_rc = FnInc (y, FnLet (z, (RCall (c, [y; y])), FnRet (z))));
  Format.printf "Compiled to '%a'\n" pp_fnbody with_rc
  (* I was here! *)

let () =
let beta_vars:beta_env = StringMap.of_list [(y, Owned)] in
  let _ = beta_global := StringMap.of_list [(c, [Borrowed; Owned])] in
  let example = FnLet (z, (RCall (c, [y; y])), FnRet (z)) in
  let with_rc = insert_rc example beta_vars in
  Format.printf "Before:\n%a\n" pp_fnbody example;
  assert (with_rc = FnInc (y, FnLet (z, (RCall (c, [y; y])), FnDec (y, FnRet (z)))));
  Format.printf "After:\n%a\n" pp_fnbody with_rc
  (* I was here! *)