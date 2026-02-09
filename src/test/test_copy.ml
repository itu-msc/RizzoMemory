open Ast

let test_copy_prop_removes_unused_let () =
  (* y = x; z = 3 + y*)
  let e = ELet("y", EVar "x", ELet("z", EBinop (EInt 3, Add, EVar "y"), EVar "z")) in

let tests_copy_propagation = [
	"removes let", `Quick, test_copy_prop_removes_unused_let
]