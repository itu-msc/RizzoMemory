
module M = Map.Make(String)

type builtin_info = {
  name: string;
  param_ownership: Refcount.ownership list option;
  projection_index: int option;
  public: bool;
  typ : Ast.typ;
}

let mk name ?(ownership = None) ?(proj_idx = None) ?(public = true) (t: Ast.typ) () : builtin_info = {
  name;
  param_ownership = ownership;
  projection_index = proj_idx;
  public;
  typ = t;
}

let output_builtins = [
  mk "console_out_signal" ~ownership:(Some [Refcount.Owned]) (TFun (Cons1(TSignal (TParam "'a"), []), TUnit)) ();
  mk "quit_at" ~ownership:(Some [Refcount.Owned]) (TFun (Cons1(TLater (TParam "'a"), []), TUnit)) ();
  (* mk "output_string_signal" ~ownership:(Some [Refcount.Owned]) (TFun (Cons1(TSignal TString, []), TUnit)) (); *)
]

let builtins = [
  mk "start_event_loop" ~ownership:(Some [Refcount.Borrowed]) (TFun(Cons1(TUnit, []), TUnit)) ();
  mk "parse_int" ~ownership:(Some [Refcount.Borrowed]) (TFun (Cons1(TString, []), TOption TInt)) ();
  mk "not" ~ownership:(Some [Refcount.Borrowed]) (TFun (Cons1(TBool, []), TBool)) ();
  mk "eq" ~ownership:(Some [Refcount.Borrowed; Refcount.Borrowed]) (TFun (Cons1(TParam "'a", [TParam "'a"]), TBool)) ();
  mk "lt" ~ownership:(Some [Refcount.Borrowed; Refcount.Borrowed]) (TFun (Cons1(TInt, [TInt]), TBool))  ();
  mk "leq" ~ownership:(Some [Refcount.Borrowed; Refcount.Borrowed]) (TFun (Cons1(TInt, [TInt]), TBool)) ();
  mk "gt" ~ownership:(Some [Refcount.Borrowed; Refcount.Borrowed]) (TFun (Cons1(TInt, [TInt]), TBool)) ();
  mk "geq" ~ownership:(Some [Refcount.Borrowed; Refcount.Borrowed]) (TFun (Cons1(TInt, [TInt]), TBool)) ();
  mk "add" ~ownership:(Some [Refcount.Borrowed; Refcount.Borrowed]) (TFun (Cons1(TInt, [TInt]), TInt)) ();
  mk "sub" ~ownership:(Some [Refcount.Borrowed; Refcount.Borrowed]) (TFun (Cons1(TInt, [TInt]), TInt)) ();
  mk "mul" ~ownership:(Some [Refcount.Borrowed; Refcount.Borrowed]) (TFun (Cons1(TInt, [TInt]), TInt)) ();
  mk "div" ~ownership:(Some [Refcount.Borrowed; Refcount.Borrowed]) (TFun (Cons1(TInt, [TInt]), TInt)) ();
  mk "string_concat" ~public:false ~ownership:(Some [Refcount.Borrowed; Refcount.Borrowed]) (TFun (Cons1(TString, [TString]), TString)) ();
  mk "string_eq" ~public:false ~ownership:(Some [Refcount.Borrowed; Refcount.Borrowed]) (TFun (Cons1(TString, [TString]), TBool)) ();
  mk "string_is_empty" ~public:false ~ownership:(Some [Refcount.Borrowed]) (TFun (Cons1(TString, []), TBool)) ();
  mk "string_head" ~public:false ~ownership:(Some [Refcount.Borrowed]) (TFun (Cons1(TString, []), TString)) ();
  mk "string_tail" ~public:false ~ownership:(Some [Refcount.Borrowed]) (TFun (Cons1(TString, []), TString)) ();
  mk "string_of_int" ~public:true ~ownership: (Some [Refcount.Borrowed]) (TFun (Cons1(TInt, []), TString)) ();
  mk "match_fail" ~public:false ~ownership:(Some [Refcount.Borrowed]) (TFun (Cons1(TString, []), TParam "'a")) ();

  mk "head" ~proj_idx:(Some 0) (TFun (Cons1(TSignal (TParam "'a"), []), TParam "'a")) ();
  mk "fst" ~proj_idx: (Some 0) (TFun (Cons1(TTuple (TParam "'a", TParam "'b"), []), TParam "'a")) ();
  mk "snd" ~proj_idx: (Some 1) (TFun (Cons1(TTuple (TParam "'a", TParam "'b"), []), TParam "'b")) ();

  mk "console" (TChan TString) ();

] @ output_builtins

let builtins_map = List.map (fun b -> b.name, b) builtins |> M.of_list
let public_builtins = List.filter (fun ({ public; _ } : builtin_info) -> public) builtins

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
