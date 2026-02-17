open Refcount

let make_fun_decl ?ending:(e = " {\n") name  = 
  Printf.sprintf "rz_box_t %s(rz_function_t* fun_context)%s" name e

let emit_c_code (p:program) (filename:string) =
  let module M = Map.Make(String) in
  let arity_map = M.of_list (List.map (fun (name, Fun (params, _)) -> (name, List.length params)) p) in
  let out_file = open_out filename in
  let write = output_string out_file in
  let rec emit_program (p: program) : unit = 
    write "#include \"rizzo.h\"\n";
    write "\n";
    List.iter emit_fn p;
    match List.assoc_opt "entry" p with
    | Some _ -> write ("int main() {\n"
                ^ "    rz_init_rizzo();\n"
                ^ "    rz_box_t res = rz_call(entry, (rz_box_t[]){rz_make_int(42)}, 1);\n"
                ^ "    printf(\"result: %d\\n\", res.as.i32);\n"
                ^ "    return 0;\n}\n")
    | None -> failwith "No entry point found"
  and emit_fn (name, Fun (params, body)) : unit = 
    write (make_fun_decl name);
    write ("rz_box_t* args = ARGS_OF_BOXED(fun_context);\n");
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
      branches 
      |> List.iteri (fun tag branch_fn -> 
        write (Printf.sprintf "case %d:\n" tag);
        emit_fn_body branch_fn;
        write "break;\n";
      );
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
    | RCall ("eq", [p1; p2]) -> Printf.sprintf "rz_eq(%s, %s)" (emit_primitive p1) (emit_primitive p2)
    | RCall ("start_event_loop", _) -> "rz_start_event_loop()"
    | RCall ("output_int_signal", [signal]) -> 
      Printf.sprintf "rz_call(rz_register_output_signal, (rz_box_t[]){%s}, 1)" (emit_primitive signal)
    | RCall (f, args) -> 
      Printf.sprintf "rz_call(%s, (rz_box_t[]){%s}, %d)" f (mk_args_string args) (List.length args)
    | RCtor (tag, []) -> 
      Printf.sprintf "rz_make_ptr(rz_ctor_var(%d, %d))" tag 0
    | RCtor (tag, args) -> 
      Printf.sprintf "rz_make_ptr(rz_ctor_var(%d, %d, %s))" tag (List.length args) (mk_args_string args)
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
    | RSignal {head; tail} -> Printf.sprintf "rz_make_ptr_sig(rz_signal_ctor(%s, %s))" (emit_primitive head) (emit_primitive tail)
    in write s
  and emit_primitive = function
    | Refcount.Var x -> x
    | Refcount.Const CInt i   -> Printf.sprintf "rz_make_int(%d)" i
    | Refcount.Const CBool b  -> Printf.sprintf "rz_make_ptr(rz_bool_ctor(%b))" b
    | Refcount.Const CNever   -> "RZ_NEVER"
    | _ -> failwith "emit_primitive: todo"
  in emit_program p