open! Rizzoc
open! Rizzoc.RefCount

let c = "c"
let y = "y"
let z = "z"

let ullrich_and_demoura_ex1 () =
  let beta_vars:beta_env = StringMap.of_list [(y, Owned)] in
  let beta = StringMap.of_list [(c, [Owned; Owned])] in
  let example = FnLet (z, (RCall (c, [y; y])), FnRet (z)) in
  Format.printf "Before: %a\n" pp_fnbody example;
  let with_rc = insert_rc example beta_vars beta in
  assert (with_rc = FnInc (y, FnLet (z, (RCall (c, [y; y])), FnRet (z))));
  Format.printf "Compiled to '%a'\n" pp_fnbody with_rc

let ullrich_and_demoura_ex2 () =
  let beta_vars:beta_env = StringMap.of_list [(y, Owned)] in
  let beta = StringMap.of_list [(c, [Borrowed; Owned])] in
  let example = FnLet (z, (RCall (c, [y; y])), FnRet (z)) in
  let with_rc = insert_rc example beta_vars beta in
  Format.printf "Before:\n%a\n" pp_fnbody example;
  assert (with_rc = FnInc (y, FnLet (z, (RCall (c, [y; y])), FnDec (y, FnRet (z)))));
  Format.printf "After:\n%a\n" pp_fnbody with_rc

(* Example program requiring fixpoint iteration, no reset/reuse. *)
let _example_prog : program =
  [
    ( "h",
      Fun
        ( [ "x" ],
          (* h x = let t = (x x) in ret t
             In your IR: RVarApp forces both arguments owned-required,
             so collectO(h_body) includes "x", thus beta(h)[0] becomes Owned.
          *)
          FnLet ("t", RVarApp ("x", "x"), FnRet "t") ) );

    ( "g",
      Fun
        ( [ "y" ],
          (* g y = let r = h(y) in ret r
             g will only learn y must be Owned AFTER beta(h)[0] flips to Owned.
          *)
          FnLet ("r", RCall ("h", [ "y" ]), FnRet "r") ) );

    ( "f",
      Fun
        ( [ "z" ],
          (* f z = let s = g(z) in ret s
             f will only learn z must be Owned AFTER beta(g)[0] flips to Owned.
          *)
          FnLet ("s", RCall ("g", [ "z" ]), FnRet "s") ) );
  ]

let print_parameter_ownerships beta = 
  let module StringMap = Map.Make(String) in
  StringMap.iter
    (fun fname ownerships ->
      let ownerships_str =
        ownerships
        |> List.map (function Owned -> "Owned" | Borrowed -> "Borrowed")
        |> String.concat "; "
      in
      Printf.printf "  %s: [%s]\n" fname ownerships_str)
    beta

let infer_borrow_status_example () =
  let beta = infer_all _example_prog in
  Printf.printf "\nInferred beta environment:\n";
  print_parameter_ownerships beta
