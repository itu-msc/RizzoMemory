val length : 'a Ast.list1 -> int
val map : ('a -> 'b) -> 'a Ast.list1 -> 'b Ast.list1
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b Ast.list1 -> 'a
val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b Ast.list1 -> 'c Ast.list1 -> 'a
val of_list : 'a list -> 'a Ast.list1