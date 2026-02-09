let new_name_cnt = ref 0 
let new_name_reset () = new_name_cnt := 0
let new_name s = 
  incr new_name_cnt; Printf.sprintf "%s%d" s !new_name_cnt 
let new_var () = new_name "var"