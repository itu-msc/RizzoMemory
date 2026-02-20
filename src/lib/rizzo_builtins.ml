
module M = Map.Make(String)

type builtin_info = {
  name: string;
  param_ownership: Refcount.ownership list option;
  projection_index: int option;
}

let mk name ?(ownership = None) ?(proj_idx = None) () : builtin_info = {
  name;
  param_ownership = ownership;
  projection_index = proj_idx;
} 

let builtins = [
  mk "start_event_loop" ~ownership:(Some [Refcount.Borrowed]) ();
  mk "output_int_signal" ~ownership:(Some [Refcount.Owned]) ();
  mk "eq" ~ownership:(Some [Refcount.Borrowed; Refcount.Borrowed]) ();
  mk "lt" ~ownership:(Some [Refcount.Borrowed; Refcount.Borrowed]) ();
  mk "leq" ~ownership:(Some [Refcount.Borrowed; Refcount.Borrowed]) ();
  mk "add" ~ownership:(Some [Refcount.Borrowed; Refcount.Borrowed]) ();
  mk "sub" ~ownership:(Some [Refcount.Borrowed; Refcount.Borrowed]) ();
  mk "mul" ~ownership:(Some [Refcount.Borrowed; Refcount.Borrowed]) ();
  mk "div" ~ownership:(Some [Refcount.Borrowed; Refcount.Borrowed]) ();

  mk "head" ~proj_idx:(Some 0) ();
  mk "fst" ~proj_idx:(Some 0) ();
  mk "snd" ~proj_idx:(Some 1) ();
  mk "left_elim" ~proj_idx:(Some 0) ();
  mk "right_elim" ~proj_idx:(Some 0) ();
  mk "both_elim_fst" ~proj_idx:(Some 0) ();
  mk "both_elim_snd" ~proj_idx:(Some 1) ();
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
