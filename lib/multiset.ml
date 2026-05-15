module Make(Ord: Map.OrderedType) = struct
  module Map = Map.Make(Ord)

  type t = int Map.t

  let empty : t = Map.empty
  let add_many x n m  : t = 
    Map.find_opt x m 
    |> Option.value ~default:0
    |> fun current -> Map.add x (current + n) m

  let add x m  : t = 
    Map.find_opt x m 
    |> Option.value ~default:0
    |> fun current -> Map.add x (current + 1) m
  
  let find x (m : t) = 
    Map.find x m

  let mem x (m : t) = 
    Map.find_opt x m 
    |> Option.map (fun n -> n > 0)
    |> Option.value ~default:false

  let pop_opt x (m : t) = 
    match Map.find_opt x m with
    | None -> None
    | Some n when n <= 0 -> None
    | Some n -> Some (Map.add x (n - 1) m)
    
  let pop x m = 
    match pop_opt x m with
    | None -> m
      (* failwith "pop: element not found or count is zero" *)
    | Some m' -> m'

  let pop_all x m = 
    match Map.find_opt x m with
    | None -> 0,m
    | Some n -> n, Map.add x 0 m

  let fold f acc (m : t) = Map.fold (fun k v acc -> f k v acc) m acc

  (** regular fold but for an try 'x |-> n' we perform [f] n times *)
  let fold_n f acc (m : t) = Map.fold (fun k n acc -> 
    let rec apply_n acc n = 
      if n <= 0 then acc
      else apply_n (f k acc) (n - 1)
    in
    if n <= 0 then acc else apply_n acc n
  ) m acc

  let union m1 m2 = 
    Map.fold (fun k n acc -> add_many k n acc) m1 m2
  
  let of_list lst = List.fold_left (fun acc (x, n) -> add_many x n acc) empty lst
  let of_key_list lst = List.fold_left (fun acc x -> add x acc) empty lst

  let pp pp_elem out m = 
    let elements = Map.bindings m |> List.filter (fun (_, n) -> n > 0) in
    let pp_binding out (k, n) = Format.fprintf out "%a: %d" pp_elem k n in
    Format.fprintf out "{%a}" (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out ", ") pp_binding) elements
end