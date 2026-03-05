
module M = Map.Make(String)

type builtin_info = {
  name: string;
  param_ownership: Refcount.ownership list option;
  projection_index: int option;
  typ : Ast.typ;
}

let mk name ?(ownership = None) ?(proj_idx = None) (t: Ast.typ) () : builtin_info = {
  name;
  param_ownership = ownership;
  projection_index = proj_idx;
  typ = t;
}

let builtins = [
  mk "start_event_loop" ~ownership:(Some [Refcount.Borrowed]) (TFun(Cons1(Ast.TUnit, []), TUnit)) ();
  mk "output_int_signal" ~ownership:(Some [Refcount.Owned]) (TFun (Cons1(TSignal TInt, []), TUnit)) ();
  mk "eq" ~ownership:(Some [Refcount.Borrowed; Refcount.Borrowed]) (TFun (Cons1(TParam "'a", [TParam "'a"]), TBool)) ();
  mk "lt" ~ownership:(Some [Refcount.Borrowed; Refcount.Borrowed]) (TFun (Cons1(TInt, [TInt]), TBool))  ();
  mk "leq" ~ownership:(Some [Refcount.Borrowed; Refcount.Borrowed]) (TFun (Cons1(TInt, [TInt]), TBool)) ();
  mk "add" ~ownership:(Some [Refcount.Borrowed; Refcount.Borrowed]) (TFun (Cons1(TInt, [TInt]), TInt)) ();
  mk "sub" ~ownership:(Some [Refcount.Borrowed; Refcount.Borrowed]) (TFun (Cons1(TInt, [TInt]), TInt)) ();
  mk "mul" ~ownership:(Some [Refcount.Borrowed; Refcount.Borrowed]) (TFun (Cons1(TInt, [TInt]), TInt)) ();
  mk "div" ~ownership:(Some [Refcount.Borrowed; Refcount.Borrowed]) (TFun (Cons1(TInt, [TInt]), TInt)) ();

  mk "head" ~proj_idx:(Some 0) (TFun (Cons1(TSignal (TParam "'a"), []), TParam "'a")) ();
  mk "fst" ~proj_idx: (Some 0) (TFun (Cons1(TTuple (TParam "'a", TParam "'b"), []), TParam "'a")) ();
  mk "snd" ~proj_idx: (Some 1) (TFun (Cons1(TTuple (TParam "'a", TParam "'b"), []), TParam "'b")) ();
  mk "left_elim"      ~proj_idx:(Some 0)  (TFun (Cons1(TSync (TParam "'a", TParam "'b"), []), TParam "'a")) (); 
  mk "right_elim"     ~proj_idx:(Some 0)  (TFun (Cons1(TSync (TParam "'a", TParam "'b"), []), TParam "'b")) ();
  mk "both_elim_fst"  ~proj_idx:(Some 0)  (TFun (Cons1(TSync (TParam "'a", TParam "'b"), []), TParam "'a")) ();
  mk "both_elim_snd"  ~proj_idx:(Some 1)  (TFun (Cons1(TSync (TParam "'a", TParam "'b"), []), TParam "'b")) ();

  mk "console" (TChan TInt) ()
]

let builtins_map = List.map (fun b -> b.name, b) builtins |> M.of_list

let builtins_ownerships_map = 
  builtins_map 
  |> M.filter (fun _ b -> Option.is_some b.param_ownership) 
  |> M.map (fun b -> Option.get b.param_ownership)

let get name = match M.find_opt name builtins_map with
  | Some b -> b
  | None -> failwith (Printf.sprintf "Builtin '%s' not found" name)

let is_builtin name = M.mem name builtins_map

let is_builtin_projection name = match M.find_opt name builtins_map with
  | Some { projection_index = Some _; _ } -> true
  | _ -> false
