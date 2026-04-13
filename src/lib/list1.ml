open! Ast

let length (Cons1 (_, rest)) = 1 + List.length rest

let map f (Cons1 (x, rest)) = Cons1 (f x, List.map f rest)

let fold_left f acc (Cons1 (x, rest)) = List.fold_left f (f acc x) rest

let fold_left2 f acc (Cons1 (x1, rest1)) (Cons1 (x2, rest2)) =
  let acc = f acc x1 x2 in
  List.fold_left2 f acc rest1 rest2

let of_list = function
  | [] -> failwith "Cannot convert empty list to list1"
  | x :: rest -> Cons1 (x, rest)