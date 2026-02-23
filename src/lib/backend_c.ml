open Refcount

let make_fun_decl ?ending:(e = " {\n") name  = 
  Printf.sprintf "rz_box_t %s(size_t num_args, rz_box_t* args)%s" name e


let rec collect_string_consts (p: Refcount.program) = 
  List.concat_map (fun (_, (Fun (_, b))) -> collect_string_consts_fn b) p
and collect_string_consts_fn (fn:Refcount.fn_body) = match fn with
  | FnRet x -> collect_primitive_string_const x 
    |> Option.map (fun a -> [a])
    |> Option.value ~default:[]
  | FnLet (_, e, f) -> (collect_string_consts_fn f) @ (collect_string_consts_expr e)
  | FnCase (_, cases) -> List.concat_map (Fun.compose collect_string_consts_fn snd) cases
  | FnDec _ | FnInc _  -> []
and collect_string_consts_expr rexpr = match rexpr with
  | RConst _ -> []
  | RCall (_, args) -> List.filter_map collect_primitive_string_const args
  | RCtor Ctor {tag = _; fields } -> List.filter_map collect_primitive_string_const fields
  | RPartialApp (_, args) -> List.filter_map collect_primitive_string_const args
  | RVarApp (_, arg) -> collect_primitive_string_const arg
    |> Option.map (fun a -> [a])
    |> Option.value ~default:[]
  | RCtor Signal {head; tail} -> List.filter_map collect_primitive_string_const [head; tail]
  | RProj _ | RReset _ | RReuse _  -> []
and collect_primitive_string_const p = match p with
  | Const (CString x as c) -> Some (c, (x, Utilities.new_name "rz_string_lit"))
  | _ -> None


let emit_c_code (p:program) (filename:string) =
  let module M = Map.Make(String) in
  let arity_map = M.of_list (List.map (fun (name, Fun (params, _)) -> (name, List.length params)) p) in
  let out_file = open_out filename in
  let write = output_string out_file in

  let string_consts = Utilities.new_name_reset (); collect_string_consts p in

  let rec emit_program (p: program) : unit = 
    write "#include \"rizzo.h\"\n";
    write "\n";
    string_consts 
    |> List.iter (fun (_, (str_lit, name)) -> write @@ Printf.sprintf "static char* %s = %S;\n" name str_lit);
    write "\n";

    List.iter emit_fn p;
    match List.assoc_opt "entry" p with
    | Some _ -> write ("int main() {\n"
                ^ "    rz_init_rizzo();\n"
                ^ "    rz_box_t res = rz_call(entry, 1, (rz_box_t[]){rz_make_int(42)});\n"
                ^ "    printf(\"result: \"); rz_debug_print_box(res); printf(\"\\n\"); \n"
                ^ "    return 0;\n}\n")
    | None -> failwith "No entry point found"
  and emit_fn (name, Fun (params, body)) : unit = 
    write (make_fun_decl name);
    write ("(void)num_args;\n");
    List.iteri (fun i param -> 
      write (Printf.sprintf "rz_box_t %s = args[%d];\n" param i)
    ) params;
    emit_fn_body body;
    write "}\n\n"
  and emit_fn_body fn = match fn with
    | FnRet x -> write (Printf.sprintf "return %s;\n" (emit_primitive x))
    | FnLet (var, e, body) -> 
      write (Printf.sprintf "rz_box_t %s = " var); emit_rexpr e; write ";\n";
      emit_fn_body body
    | FnCase (scrutinee, branches) ->
      write (Printf.sprintf "switch (rz_object_tag(rz_unbox_ptr(%s))) {\n" scrutinee);
      List.map snd branches 
      |> List.iteri (fun tag branch_fn -> 
        write (Printf.sprintf "case %d: {\n" tag);
        emit_fn_body branch_fn;
        write "break;\n}\n";
      );
      write "default: {\n";
      write "fprintf(stderr, \"Rizzo Runtime error at (%s, %d): unexpected tag %d\", __FILE__, __LINE__,";
      write (Printf.sprintf "rz_object_tag(rz_unbox_ptr(%s)));\n" scrutinee);
      write "exit(1);\n}\n";
      write "}\n"
    | FnDec (x, f) -> 
      if Option.is_none (int_of_string_opt x) then
        write (Printf.sprintf "rz_refcount_dec_box(%s);\n" x);
      emit_fn_body f
    | FnInc (x,f) -> 
      if Option.is_none (int_of_string_opt x) then
        write (Printf.sprintf "rz_refcount_inc_box(%s);\n" x);
      emit_fn_body f
  and emit_rexpr e = 
    (* it may be that we are referencing a 'constant'/'global' function, then we have to emit a lift *)
    let mk_args_string args = args 
      |> List.map (function | Const _ as e -> emit_primitive e
        | Var arg -> match M.find_opt arg arity_map with
          | None -> arg
          | Some arity -> Printf.sprintf "rz_lift_c_fun(%s, %d, (rz_box_t[]){}, 0)" arg arity) 
      |> String.concat ", "
    in
    let s = match e with
    | RConst c -> emit_primitive (Const c)
    | RCall ("eq", [p1; p2]) -> Printf.sprintf "rz_eq(%s, %s)" (emit_primitive p1) (emit_primitive p2)
    | RCall ("start_event_loop", _) -> "rz_start_event_loop()"
    | RCall ("output_int_signal", [signal]) -> 
      Printf.sprintf "rz_call(rz_register_output_signal, 1, (rz_box_t[]){%s})" (emit_primitive signal)
    | RCall (f, args) -> 
      Printf.sprintf "rz_call(%s, %d, (rz_box_t[]){%s})" f (List.length args) (mk_args_string args)
    | RCtor Ctor { tag; fields = [] } -> 
      Printf.sprintf "rz_make_ptr(rz_ctor_var(%d, %d))" tag 0
    | RCtor Ctor { tag; fields } -> 
      Printf.sprintf "rz_make_ptr(rz_ctor_var(%d, %d, %s))" tag (List.length fields) (mk_args_string fields)
    | RVarApp (f, x) -> 
      Printf.sprintf "rz_apply1(rz_unbox_ptr(%s), %s)" f (emit_primitive x)
    | RPartialApp (f, args) -> (match M.find_opt f arity_map with
        | None -> failwith (Printf.sprintf "Function %s not found in arity map" f)
        | Some arity -> 
          let num_args = List.length args in
          let args = mk_args_string args in
          Printf.sprintf "rz_lift_c_fun(%s, %d, (rz_box_t[]){%s}, %d)" f arity args num_args
      )
    | RProj (i, x) -> Printf.sprintf "rz_object_get_field(rz_unbox_ptr(%s), %d)" x i
    | RCtor Signal {head; tail} -> Printf.sprintf "rz_make_ptr_sig(rz_signal_ctor(%s, %s))" (emit_primitive head) (emit_primitive tail)
    | RReset (n) -> Printf.sprintf "rz_make_ptr(rz_reset_object(rz_unbox_ptr(%s)))" n
    | RReuse (n, Ctor {tag; fields}) -> 
      Printf.sprintf "rz_make_ptr(rz_reuse_object(rz_unbox_ptr(%s), %d, %d, (rz_box_t[]){%s}))" n tag (List.length fields) (mk_args_string fields)
    | RReuse (n, Signal {head; tail}) -> 
      Printf.sprintf "rz_make_ptr_sig(rz_reuse_signal(rz_unbox_ptr(%s), %s, %s))" n (emit_primitive head) (emit_primitive tail)
    in write s
  and emit_primitive = function
    | Var x -> x
    | Const CInt i   -> Printf.sprintf "rz_make_int(%d)" i
    | Const CBool b  -> Printf.sprintf "rz_make_ptr(rz_bool_ctor(%b))" b
    | Const CNever   -> "RZ_NEVER"
    | Const (CString _ as c) -> 
      let (_, var_name) = List.assoc c string_consts in
      Printf.sprintf "rz_make_str_lit(%s)" var_name
    | Const Ast.CUnit -> Printf.sprintf "rz_make_int(0)"
  in emit_program p
  