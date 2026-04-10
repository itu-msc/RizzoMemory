open Refcount

let standard_indent = 4

let make_fun_decl ?ending:(e = " {\n") name  = 
  Printf.sprintf "rz_box_t %s(size_t _, rz_box_t* args)%s" name e 
  (* TODO: see if we can remove the num_args (the first param) from the method signature *)

let rec collect_string_consts (RefProg{functions; globals}: program) = 
  List.concat_map (fun (_, (Fun (_, b))) -> collect_string_consts_fn b) functions
  @ List.concat_map (fun (_, body) -> collect_string_consts_fn body) globals
and collect_string_consts_fn (fn:Refcount.fn_body) = match fn with
  | FnRet x -> collect_primitive_string_const x 
    |> Option.map (fun a -> [a])
    |> Option.value ~default:[]
  | FnLet (_, e, f) -> (collect_string_consts_fn f) @ (collect_string_consts_expr e)
  | FnCase (_, cases) -> List.concat_map (fun { body; _ } -> collect_string_consts_fn body) cases
  | FnDec (_, rest) | FnInc (_, rest)  -> collect_string_consts_fn rest
and collect_string_consts_expr rexpr = match rexpr with
  | RConst c -> collect_primitive_string_const (Const c) 
    |> Option.map (fun a -> [a])
    |> Option.value ~default:[]
  | RCall (_, args) -> List.filter_map collect_primitive_string_const args
  | RCtor Ctor {tag = _; fields } -> List.filter_map collect_primitive_string_const fields
  | RPartialApp (_, args) -> List.filter_map collect_primitive_string_const args
  | RVarApp (_, arg) -> collect_primitive_string_const arg
    |> Option.map (fun a -> [a])
    |> Option.value ~default:[]
  | RCtor Signal {head; tail} -> List.filter_map collect_primitive_string_const [head; tail]
  | RReuse (_, Signal {head;tail}) -> List.filter_map collect_primitive_string_const [head; tail]
  | RReuse (_, Ctor {fields; _}) -> List.filter_map collect_primitive_string_const fields
  | RProj _ | RReset _ -> []
and collect_primitive_string_const p = match p with
  | Const (CString x as c) -> Some (c, (x, Utilities.new_name "rz_string_lit"))
  | _ -> None


let emit_c_code (RefProg{functions; _} as p:program) (filename:string) =
  let module M = Map.Make(String) in
  let builtin_arity_map =
    Rizzo_builtins.builtins
    |> List.filter_map (fun ({ name; param_ownership; _ } : Rizzo_builtins.builtin_info) ->
        Option.map (fun ownerships -> (name, List.length ownerships)) param_ownership)
    |> M.of_list
  in
  let arity_map =
    M.of_list (List.map (fun (name, Fun (params, _)) -> (name, List.length params)) functions)
    |> M.union (fun key _ _ -> failwith (Printf.sprintf "Duplicate function name %s" key)) builtin_arity_map
  in
  
  let mangle s =
    if String.contains s '\''
    then (^) "_rizz_" (String.map (function '\'' -> '_' | c -> c) s)
    else (^) "rizz_" s in

  let builtin_c_name name =
    if M.mem name builtin_arity_map then Printf.sprintf "rz_builtin_%s" name 
    else (mangle name)
  in
  let out_file = open_out filename in
  let write ?(indent = 0) out = output_string out_file ((String.make indent ' ') ^ out) in

  let string_consts = Utilities.new_name_reset (); collect_string_consts p in

  let rec emit_program (RefProg{functions; globals}:program) : unit = 
    write "#include \"rizzo.h\"\n";
    write "\n";

    write (Printf.sprintf "static rz_box_t %s = rz_make_int(RZ_CHANNEL_CONSOLE_IN);\n" (mangle "console"));

    string_consts 
    |> List.iter (fun (_, (str_lit, name)) -> write @@ Printf.sprintf "static char* %s = %S;\n" name str_lit);
    write "\n";

    (* forward declare functions *)
    List.iter (fun (name, _) -> write @@ make_fun_decl ~ending:";\n" (mangle name)) functions;
    write "\n";

    let needs_init = declare_globals globals in
    if List.length needs_init > 0 then write ~indent:standard_indent "\n";

    List.iter emit_fn functions;
    let entry_name = mangle "entry" in
    match List.assoc_opt "entry" functions with
    | Some _ -> write ("int main() {\n"
                ^ "    rz_init_rizzo();\n");
                init_globals needs_init;
                write (
                  Printf.sprintf 
                  "    rz_box_t res = rz_call(%s, 1, (rz_box_t[]){rz_make_int(0)});\n" entry_name (* TODO: notice, should we have this line here?*)
                ^ "    printf(\"result: \"); rz_debug_print_box(res); printf(\"\\n\"); \n"
                ^ "    return 0;\n}\n")
    | None -> failwith "No entry point found"
  and declare_globals (globals: (string * Refcount.fn_body) list) = 
    List.filter_map (fun (name, body) -> 
      match body with
      | FnRet p -> write (Printf.sprintf "static rz_box_t %s = %s;\n" (mangle name) (emit_primitive p)); None
      | _ -> write (Printf.sprintf "static rz_box_t %s;\n" (mangle name)); Some (name, body)
    ) globals
  and init_globals (globals: (string* Refcount.fn_body) list) : unit = 
    List.iter (fun (name, body) -> 
      emit_fn_body ~return_to:(Some name) 4 body; write "\n";
    ) globals
  and emit_fn (name, Fun (params, body)) : unit = 
    write (make_fun_decl (mangle name));
    List.iteri (fun i param ->
      write ~indent:standard_indent (Printf.sprintf "rz_box_t %s = args[%d];\n" (mangle param) i)
    ) params;
    emit_fn_body standard_indent body;
    write "}\n\n"
  and emit_fn_body ?return_to:(return_to = None) indent fn = 
    let emit_fn_body = emit_fn_body ~return_to in
    match fn with
    | FnRet x -> 
      (match return_to with
      | None -> write ~indent (Printf.sprintf "return %s;\n" (emit_primitive x))
      | Some var -> write ~indent (Printf.sprintf "%s = %s;\n" (mangle var) (emit_primitive x)))
    | FnLet (var, e, body) ->
      write ~indent (Printf.sprintf "rz_box_t %s = " (mangle var)); emit_rexpr e; write ";\n";
      emit_fn_body indent body
    | FnCase (scrutinee, branches) ->
      write ~indent (Printf.sprintf "switch (rz_object_tag(rz_unbox_ptr(%s))) {\n" (mangle scrutinee));
      let indent_inner = indent + standard_indent in
      let default_branch, tagged_branches =
        List.fold_left
          (fun (default_branch, tagged_branches) { tag; body; _ } ->
            match tag with
            | Some tag -> (default_branch, (tag, body) :: tagged_branches)
            | None ->
                ((match default_branch with
                  | Some _ -> default_branch
                  | None -> Some body), tagged_branches))
          (None, []) branches
      in
      List.rev tagged_branches
      |> List.iter (fun (tag, branch_fn) -> 
        write ~indent (Printf.sprintf "case %d: {\n" tag);
        emit_fn_body indent_inner branch_fn;
        write ~indent:indent_inner "break;\n";
        write ~indent "}\n";
      );
      write ~indent "default: {\n";
      (match default_branch with
      | Some branch_fn ->
          emit_fn_body indent_inner branch_fn;
          write ~indent:indent_inner "break;\n"
      | None ->
          write ~indent:indent_inner "fprintf(stderr, \"Rizzo Runtime error at (%s, %d): unexpected tag %d\", __FILE__, __LINE__,";
          write (Printf.sprintf "rz_object_tag(rz_unbox_ptr(%s)));\n" (mangle scrutinee));
          write ~indent:indent_inner "exit(1);\n");
      write ~indent "}}\n"
    | FnDec (x, f) -> 
      if Option.is_none (int_of_string_opt x) then
        write ~indent (Printf.sprintf "rz_refcount_dec_box(%s);\n" (mangle x));
      emit_fn_body indent f
    | FnInc (x,f) -> 
      if Option.is_none (int_of_string_opt x) then
        write ~indent (Printf.sprintf "rz_refcount_inc_box(%s);\n" (mangle x));
      emit_fn_body indent f
  and emit_rexpr e = 
    (* it may be that we are referencing a 'constant'/'global' function, then we have to emit a lift *)
    
    let s = match e with
    | RConst c -> emit_primitive (Const c)
    | RCall ("eq", [p1; p2]) -> Printf.sprintf "rz_eq(%s, %s)" (emit_primitive p1) (emit_primitive p2)
    | RCall ("start_event_loop", _) -> "rz_start_event_loop()"
    | RCall ("console_out_signal", [signal]) -> 
      Printf.sprintf "rz_call(rz_register_output_signal, 1, (rz_box_t[]){%s})" (emit_primitive signal)
    | RCall (f, args) -> 
      Printf.sprintf "rz_call(%s, %d, (rz_box_t[]){%s})" (builtin_c_name f) (List.length args) (mk_args_string args)
    | RCtor Ctor { tag; fields = [] } -> 
      Printf.sprintf "rz_make_ptr(rz_ctor_var(%d, %d))" tag 0
    | RCtor Ctor { tag; fields } -> 
      Printf.sprintf "rz_make_ptr(rz_ctor_var(%d, %d, %s))" tag (List.length fields) (mk_args_string fields)
    | RVarApp (f, x) -> 
      Printf.sprintf "rz_apply1(rz_unbox_ptr(%s), %s)" (mangle f) (emit_primitive x)
    | RPartialApp (f, args) -> (match M.find_opt f arity_map with
        | None -> failwith (Printf.sprintf "Function %s not found in arity map" f)
        | Some arity -> 
          let num_args = List.length args in
          let args = mk_args_string args in
          Printf.sprintf "rz_lift_c_fun(%s, %d, (rz_box_t[]){%s}, %d)" (builtin_c_name f) arity args num_args
      )
    | RProj (i, x) -> Printf.sprintf "rz_object_get_field(rz_unbox_ptr(%s), %d)" (mangle x) i
    | RCtor Signal {head; tail} -> Printf.sprintf "rz_make_ptr_sig(rz_signal_ctor(%s, %s))" (emit_primitive head) (emit_primitive tail)
    | RReset (n) -> Printf.sprintf "rz_make_ptr(rz_reset_object(rz_unbox_ptr(%s)))" (mangle n)
    | RReuse (n, Ctor {tag; fields}) -> 
      Printf.sprintf "rz_make_ptr(rz_reuse_object(rz_unbox_ptr(%s), %d, %d, (rz_box_t[]){%s}))" (mangle n) tag (List.length fields) (mk_args_string fields)
    | RReuse (n, Signal {head; tail}) -> 
      Printf.sprintf "rz_make_ptr_sig(rz_reuse_signal(rz_unbox_ptr(%s), %s, %s))" (mangle n) (emit_primitive head) (emit_primitive tail)
    in write s
  and emit_primitive = function
    | Var x -> as_possible_function_access x []
    | Const CInt i   -> Printf.sprintf "rz_make_int(%d)" i
    | Const CBool true  -> "rz_make_ptr(rz_bool_ctor(true))"
    | Const CBool false -> "rz_make_ptr(rz_bool_ctor(false))"
    | Const CNever   -> "RZ_NEVER"
    | Const (CString _ as c) -> 
      let (_, var_name) = List.assoc c string_consts in
      Printf.sprintf "rz_make_str_lit(%s)" var_name
    | Const Ast.CUnit -> Printf.sprintf "rz_make_int(0)"
  and as_possible_function_access name args =
    match M.find_opt name arity_map with
    | None -> mangle name  (* was just a regular name - output as such *)
    | Some arity -> (* was the name of a function - output as such? *)
      match args with
      | [] -> Printf.sprintf "rz_lift_c_fun(%s, %d, NULL, 0)" (builtin_c_name name) arity
      | _ ->
        let num_args = List.length args in
        let args = mk_args_string args in
        Printf.sprintf "rz_lift_c_fun(%s, %d, (rz_box_t[]){%s}, %d)" (builtin_c_name name) arity args num_args
  and mk_args_string args = 
    args
    |> List.map (function 
        | Const _ as e -> emit_primitive e
        | Var arg -> as_possible_function_access arg [])
    |> String.concat ", "
      
  in
  Fun.protect
    ~finally:(fun () -> close_out out_file)
    (fun () -> emit_program p; flush out_file)
  