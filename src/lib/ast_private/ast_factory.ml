open! Ast_core

let typ_option t = TApp (TName "Option", [t])
let typ_list t = TApp (TName "List", [t])
let typ_sync a b = TApp (TName "Sync", [a; b])

let typ_fun params ret = TFun (Cons1 (List.hd params, List.tl params), ret)
let typ_fun1 p ret = TFun (Cons1 (p, []), ret)
let typ_param n = TParam n

let typ_tuple t1 t2 = TTuple (t1, t2, [])
let typ_tuple_many t1 t2 ts = TTuple (t1, t2, ts)

let typ_signal t = TSignal t
let typ_later t = TLater t
let typ_delay t = TDelay t
let typ_chan t = TChan t