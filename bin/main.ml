open! Fr
open! Fr.Lambda1
open! Fr.Lambda1.Factory

let _map_program = 
  (*map: 
    \f -> \s -> match s with | SigCons(x, _) -> 
      let xs = Tail(s) in
      let hd = f x; 
      let map' = map f in
      let delayed_map = delay map' in
      let tl = (laterapp delayed_map) xs in
      SigCons(hd', tl')) *)
  let x = Var "x" in let xs = Var "xs" in
  let f = Var "f" in let s = Var "s" in let hd = Var "hd" in 
  let tl = Var "tl" in let map = Var "map" in let delay = Var "delay" in
  let laterapp = Var "laterapp" in let map' = Var "map'" in 
  let delayed_map = Var "delayed_map" in
  let borrow_env = VarMSet.of_key_list (get_names [ f; map; delay; laterapp ]) in
  borrow_env,
  llambda f (llambda s (
    lmatch s [ pctor "SigCons" [pvar x; pwildcard ()], 
      llet xs (lval (vctor "Tail" [s])) @@
      llet hd (lapp (lvar f) x) @@ 
      llet map' (lapp (lvar map) f) @@
      llet delayed_map (lapp (lvar delay) map') @@
      llet tl (lapp (lapp (lvar laterapp) delayed_map) xs) @@
      lval ( vctor "SigCons" [hd; tl] )
    ]))

let _reuse_program = 
  (* \y -> match y with 
           | Some(x) -> let x = Some(x) in Some(x)  
           | None -> None *)
  let x = Var "x" in let y = Var "y" in
  VarMSet.empty,
  llambda y (
    lmatch y [
      pctorv  "Some" [x], llet x (lval (vctor "Some" [x])) (lval (vctor "Some" [x]));
      pctor0 "None", lval (vctor0 "None")
    ])

let () = 
  let borrowed_env, program = _map_program in
  Format.printf "Original program:   %a\n\n" PP.pp_lexpr program;
  let o = Refcount.insert_dup_drop borrowed_env VarMSet.empty program in
  Format.printf "With dup and drop:  %a\n\n" PP.pp_lexpr o;
  let o' = Refcount.run_drop_guided_reuse o in
  Format.printf "After guided reuse: %a\n" PP.pp_lexpr o';
  ()
