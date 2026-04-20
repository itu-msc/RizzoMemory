
module Refcount_core = Refcount_private.Refcount_core
module M = Map.Make(String)

type builtin_info = {
  name: string;
  param_ownership: Refcount_core.ownership list option;
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

open Ast.Factory

let output_builtins = [
  mk "console_out_signal" ~ownership:(Some [Refcount_core.Owned]) (TFun (Cons1(TSignal (TParam "'a"), []), TUnit)) ();
  mk "quit_at" ~ownership:(Some [Refcount_core.Owned]) (TFun (Cons1(TLater (TParam "'a"), []), TUnit)) ();
  (* mk "output_string_signal" ~ownership:(Some [Refcount_core.Owned]) (TFun (Cons1(TSignal TString, []), TUnit)) (); *)
]

let builtins = [
  mk "start_event_loop" ~ownership:(Some [Refcount_core.Borrowed]) (TFun(Cons1(TUnit, []), TUnit)) ();
  mk "clock" ~ownership:(Some [Refcount_core.Borrowed]) (TFun (Cons1(TInt, []), TSignal TInt)) ();
  mk "parse_int" ~ownership:(Some [Refcount_core.Borrowed]) (TFun (Cons1(TString, []), TApp (TName "Option", [TInt]))) ();
  mk "not" ~ownership:(Some [Refcount_core.Borrowed]) (TFun (Cons1(TBool, []), TBool)) ();
  mk "mod" ~ownership:(Some [Refcount_core.Borrowed; Refcount_core.Borrowed]) (TFun (Cons1(TInt, [TInt]), TInt)) ();
  mk "eq" ~ownership:(Some [Refcount_core.Borrowed; Refcount_core.Borrowed]) (TFun (Cons1(TParam "'a", [TParam "'a"]), TBool)) ();
  mk "lt" ~ownership:(Some [Refcount_core.Borrowed; Refcount_core.Borrowed]) (TFun (Cons1(TInt, [TInt]), TBool))  ();
  mk "leq" ~ownership:(Some [Refcount_core.Borrowed; Refcount_core.Borrowed]) (TFun (Cons1(TInt, [TInt]), TBool)) ();
  mk "gt" ~ownership:(Some [Refcount_core.Borrowed; Refcount_core.Borrowed]) (TFun (Cons1(TInt, [TInt]), TBool)) ();
  mk "geq" ~ownership:(Some [Refcount_core.Borrowed; Refcount_core.Borrowed]) (TFun (Cons1(TInt, [TInt]), TBool)) ();
  mk "abs" ~ownership:(Some [Refcount_core.Borrowed]) (TFun (Cons1(TInt, []), TInt)) ();
  mk "min" ~ownership:(Some [Refcount_core.Borrowed; Refcount_core.Borrowed]) (TFun (Cons1(TInt, [TInt]), TInt)) ();
  mk "max" ~ownership:(Some [Refcount_core.Borrowed; Refcount_core.Borrowed]) (TFun (Cons1(TInt, [TInt]), TInt)) ();
  mk "clamp" ~ownership:(Some [Refcount_core.Borrowed; Refcount_core.Borrowed; Refcount_core.Borrowed]) (TFun (Cons1(TInt, [TInt; TInt]), TInt)) ();
  mk "add" ~ownership:(Some [Refcount_core.Borrowed; Refcount_core.Borrowed]) (TFun (Cons1(TInt, [TInt]), TInt)) ();
  mk "sub" ~ownership:(Some [Refcount_core.Borrowed; Refcount_core.Borrowed]) (TFun (Cons1(TInt, [TInt]), TInt)) ();
  mk "mul" ~ownership:(Some [Refcount_core.Borrowed; Refcount_core.Borrowed]) (TFun (Cons1(TInt, [TInt]), TInt)) ();
  mk "div" ~ownership:(Some [Refcount_core.Borrowed; Refcount_core.Borrowed]) (TFun (Cons1(TInt, [TInt]), TInt)) ();
  mk "string_contains" ~ownership:(Some [Refcount_core.Borrowed; Refcount_core.Borrowed]) (TFun (Cons1(TString, [TString]), TBool)) ();
  mk "string_starts_with" ~ownership:(Some [Refcount_core.Borrowed; Refcount_core.Borrowed]) (TFun (Cons1(TString, [TString]), TBool)) ();
  mk "string_ends_with" ~ownership:(Some [Refcount_core.Borrowed; Refcount_core.Borrowed]) (TFun (Cons1(TString, [TString]), TBool)) ();
  mk "string_concat" ~public:false ~ownership:(Some [Refcount_core.Borrowed; Refcount_core.Borrowed]) (TFun (Cons1(TString, [TString]), TString)) ();
  mk "string_eq" ~public:false ~ownership:(Some [Refcount_core.Borrowed; Refcount_core.Borrowed]) (TFun (Cons1(TString, [TString]), TBool)) ();
  mk "string_is_empty" ~public:false ~ownership:(Some [Refcount_core.Borrowed]) (TFun (Cons1(TString, []), TBool)) ();
  mk "string_head" ~public:false ~ownership:(Some [Refcount_core.Borrowed]) (TFun (Cons1(TString, []), TString)) ();
  mk "string_tail" ~public:false ~ownership:(Some [Refcount_core.Borrowed]) (TFun (Cons1(TString, []), TString)) ();
  mk "string_split" ~ownership:(Some [Refcount_core.Borrowed; Refcount_core.Borrowed]) (typ_fun [TString; TString] (typ_list TString)) ();
  mk "string_of_int" ~public:true ~ownership: (Some [Refcount_core.Borrowed]) (typ_fun [TInt] TString) ();
  mk "match_fail" ~public:false ~ownership:(Some [Refcount_core.Borrowed]) (typ_fun [TString] (typ_param "'a")) ();

  mk "head" ~proj_idx:(Some 0) (typ_fun1 (typ_signal (typ_param "'a")) (typ_param "'a")) ();
  mk "list_head" ~proj_idx:(Some 0) (typ_fun1 (typ_list (typ_param "'a")) (typ_param "'a")) ();
  mk "list_tail" ~proj_idx:(Some 1) (typ_fun1 (typ_list (typ_param "'a")) (typ_list (typ_param "'a"))) ();
  mk "list_is_empty" ~ownership:(Some [Refcount_core.Borrowed]) (typ_fun1 (typ_list (typ_param "'a")) TBool) ();
  mk "list_length" ~ownership:(Some [Refcount_core.Borrowed]) (typ_fun1 (typ_list (typ_param "'a")) TInt) ();
  mk "fst" ~proj_idx: (Some 0) (typ_fun1 (typ_tuple (typ_param "'a") (typ_param "'b")) (typ_param "'a")) ();
  mk "snd" ~proj_idx: (Some 1) (typ_fun1 (typ_tuple (typ_param "'a") (typ_param "'b")) (typ_param "'b")) ();

  mk "console" (TChan TString) ();

] @ output_builtins

let builtins_map = List.map (fun b -> b.name, b) builtins |> M.of_list
let public_builtins = List.filter (fun ({ public; _ } : builtin_info) -> public) builtins

let builtins_ownerships_map = 
  builtins
  |> List.filter_map (fun b -> Option.map (fun ownerships -> (b.name, ownerships)) b.param_ownership)
  |> Collections.StringMap.of_list

let builtins_projection_map =
  builtins
  |> List.filter_map (fun b -> Option.map (fun idx -> (b.name, idx)) b.projection_index)
  |> Collections.StringMap.of_list

let get name = match M.find_opt name builtins_map with
  | Some b -> b
  | None -> failwith (Printf.sprintf "Builtin '%s' not found" name)

let is_builtin name = M.mem name builtins_map

let is_builtin_projection name = match M.find_opt name builtins_map with
  | Some { projection_index = Some _; _ } -> true
  | _ -> false


let builtin_types = 
    [ ("Option", ["'a"], [
        ("Nothing", []);
        ("Just", [Ast.TParam "'a"]);
      ])
    ; ("List", ["'a"], [
        ("Nil", []);
        ("Cons", [TParam "'a"; TApp (TName "List", [TParam "'a"])])
      ])
    ; ("Sync", ["'a"; "'b"], [
        ("Left", [TParam "'a"]);
        ("Right", [TParam "'b"]);
        ("Both", [TParam "'a"; TParam "'b"])
      ])
    ]

(* consider if we can do this in a way that 
  makes the compiler complain if we dont have the entries*)
let ctor_mappings = 
  M.of_list [
    (*Laters*)
    ("never", 0);
    ("wait", 1);
    ("tail", 2);
    ("sync", 4);
    ("watch", 5);
    ("later_app", 6); 

    (*sync*)
    ("left", 0);
    ("right", 1);
    ("both", 2);

    (*delay*)
    ("delay", 0);
    ("ostar", 1);

    (*Tuples*)
    ("tuple", 0);

    (*Primitives*)
    ("unit", 0);
    ("int", 0);
    ("string", 0);
    ("true", 0);
    ("false", 1);
    ("sigcons", -1);
    (* ("nothing", 0);
    ("just", 1);
    ("nil", 2);
    ("cons", 3); *)
  ]

let ctor_tag_of name = match M.find_opt (String.lowercase_ascii name) ctor_mappings with
  | Some tag -> tag
  | None -> failwith (Printf.sprintf "Constructor '%s' not found in ctor_mappings" name)

