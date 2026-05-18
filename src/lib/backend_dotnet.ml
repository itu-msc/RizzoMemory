module Refcount_core = Refcount_private.Refcount_core
open Refcount_core

let standard_indent = 4
let assembly_name = "RizzoProgram"

let csharp_string_literal s =
  let buffer = Buffer.create (String.length s + 2) in
  Buffer.add_char buffer '"';
  String.iter
    (fun ch ->
      match ch with
      | '"' -> Buffer.add_string buffer "\\\""
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '\n' -> Buffer.add_string buffer "\\n"
      | '\r' -> Buffer.add_string buffer "\\r"
      | '\t' -> Buffer.add_string buffer "\\t"
      | c ->
          let code = Char.code c in
          if code < 32 || code > 126
          then Buffer.add_string buffer (Printf.sprintf "\\u%04x" code)
          else Buffer.add_char buffer c)
    s;
  Buffer.add_char buffer '"';
  Buffer.contents buffer

let mangle s =
  let map_char = function
    | '\'' -> '_'
    | c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' -> c
    | _ -> '_'
  in
  "rizz_" ^ String.map map_char s

let ensure_dir path =
  let rec go path =
    if path = "" || path = "." then ()
    else if Sys.file_exists path then (
      if not (Sys.is_directory path) then
        failwith (Printf.sprintf "Expected directory path, got file: %s" path))
    else (
      go (Filename.dirname path);
      Unix.mkdir path 0o755)
  in
  go path

let copy_file source target =
  let in_chan = open_in_bin source in
  let out_chan = open_out_bin target in
  Fun.protect
    ~finally:(fun () ->
      close_in_noerr in_chan;
      close_out_noerr out_chan)
    (fun () ->
      let buffer = Bytes.create 65536 in
      let rec loop () =
        let read = input in_chan buffer 0 (Bytes.length buffer) in
        if read > 0 then (
          output out_chan buffer 0 read;
          loop ())
      in
      loop ())

let runtime_installed_relative_parts = [".."; "lib"; "rizzoc"; "runtime_dotnet"]
let runtime_dev_relative_parts = [".."; ".."; ".."; ".."; "src"; "runtime_dotnet"]

let join_path base parts = List.fold_left Filename.concat base parts

let realpath path =
  try Unix.realpath path with Unix.Unix_error (_, _, _) -> path

let runtime_candidates () =
  let executable_dir = Filename.dirname (realpath Sys.executable_name) in
  [ join_path executable_dir runtime_dev_relative_parts;
    join_path executable_dir runtime_installed_relative_parts;
    "src/runtime_dotnet";
  ]
  |> List.map realpath

let resolve_runtime_root () =
  match List.find_opt (fun path -> Sys.file_exists path && Sys.is_directory path) (runtime_candidates ()) with
  | Some root -> root
  | None ->
      failwith
        (Printf.sprintf "Could not locate .NET runtime directory. Looked in: %s"
           (String.concat ", " (runtime_candidates ())))

let emit_project_file project_dir =
  let path = Filename.concat project_dir (assembly_name ^ ".csproj") in
  let out_file = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out out_file)
    (fun () ->
      output_string out_file
        (Printf.sprintf
           {|<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>%s</AssemblyName>
    <ImplicitUsings>enable</ImplicitUsings>
    <Nullable>disable</Nullable>
    <NoWarn>$(NoWarn);0162</NoWarn>
  </PropertyGroup>
</Project>
|}
           assembly_name))

let emit_csharp_code (RefProg { functions; _ } as p : program) filename =
  let module M = Map.Make (String) in
  let builtin_arity_map =
    Rizzo_builtins.builtins
    |> List.filter_map
         (fun ({ name; param_ownership; _ } : Rizzo_builtins.builtin_info) ->
           Option.map (fun ownerships -> (name, List.length ownerships)) param_ownership)
    |> M.of_list
  in
  let arity_map =
    M.of_list (List.map (fun (name, Fun (params, _)) -> (name, List.length params)) functions)
    |> M.union (fun key _ _ -> failwith (Printf.sprintf "Duplicate function name %s" key)) builtin_arity_map
  in
  let is_builtin name = M.mem name builtin_arity_map in
  let out_file = open_out filename in
  let write ?(indent = 0) out = output_string out_file ((String.make indent ' ') ^ out) in
  let rec emit_program (RefProg { functions; globals } : program) =
    write "using System;\n\n";
    write "public static class RizzoProgram\n";
    write "{\n";
    write ~indent:standard_indent (Printf.sprintf "static RzBox %s;\n" (mangle "console"));
    write ~indent:standard_indent (Printf.sprintf "static RzBox %s;\n" (mangle "keyboard"));
    List.iter
      (fun (name, _) -> write ~indent:standard_indent (Printf.sprintf "static RzBox %s;\n" (mangle name)))
      globals;
    write "\n";
    List.iter emit_fn functions;
    write ~indent:standard_indent "public static int Main(string[] args)\n";
    write ~indent:standard_indent "{\n";
    write ~indent:(standard_indent * 2) "Rz.InitRizzo();\n";
    write ~indent:(standard_indent * 2) (Printf.sprintf "%s = Rz.Int(Rz.ChannelConsoleIn);\n" (mangle "console"));
    write ~indent:(standard_indent * 2) (Printf.sprintf "%s = Rz.Int(Rz.ChannelKeyboardIn);\n" (mangle "keyboard"));
    List.iter
      (fun (name, body) ->
        emit_fn_body ~return_to:(Some name) (standard_indent * 2) body;
        write "\n")
      globals;
    (match List.assoc_opt "entry" functions with
    | Some _ ->
        write ~indent:(standard_indent * 2)
          (Printf.sprintf "RzBox res = %s(new RzBox[] { Rz.Unit });\n" (mangle "entry"));
        write ~indent:(standard_indent * 2) "return (int)Rz.UnboxInt(res);\n"
    | None -> failwith "No entry point found");
    write ~indent:standard_indent "}\n";
    write "}\n"
  and emit_fn (name, Fun (params, body)) =
    write ~indent:standard_indent (Printf.sprintf "static RzBox %s(RzBox[] args)\n" (mangle name));
    write ~indent:standard_indent "{\n";
    List.iteri
      (fun i param -> write ~indent:(standard_indent * 2) (Printf.sprintf "RzBox %s = args[%d];\n" (mangle param) i))
      params;
    emit_fn_body (standard_indent * 2) body;
    write ~indent:standard_indent "}\n\n"
  and emit_fn_body ?return_to:(return_to = None) indent fn =
    let emit_fn_body = emit_fn_body ~return_to in
    match fn with
    | FnRet x ->
        (match return_to with
        | None -> write ~indent (Printf.sprintf "return %s;\n" (emit_primitive x))
        | Some var -> write ~indent (Printf.sprintf "%s = %s;\n" (mangle var) (emit_primitive x)))
    | FnLet (var, e, body) ->
        write ~indent (Printf.sprintf "RzBox %s = %s;\n" (mangle var) (emit_rexpr e));
        emit_fn_body indent body
    | FnCase (scrutinee, branches) ->
        write ~indent (Printf.sprintf "switch (Rz.ObjectTag(Rz.UnboxPtr(%s)))\n" (mangle scrutinee));
        write ~indent "{\n";
        let default_branch, tagged_branches =
          List.fold_left
            (fun (default_branch, tagged_branches) { tag; body; _ } ->
              match tag with
              | Some tag -> (default_branch, (tag, body) :: tagged_branches)
              | None ->
                  ((match default_branch with Some _ -> default_branch | None -> Some body), tagged_branches))
            (None, []) branches
        in
        List.rev tagged_branches
        |> List.iter
             (fun (tag, branch_fn) ->
               write ~indent:(indent + standard_indent) (Printf.sprintf "case %d:\n" tag);
               write ~indent:(indent + standard_indent) "{\n";
               emit_fn_body (indent + (standard_indent * 2)) branch_fn;
               write ~indent:(indent + (standard_indent * 2)) "break;\n";
               write ~indent:(indent + standard_indent) "}\n");
        write ~indent:(indent + standard_indent) "default:\n";
        write ~indent:(indent + standard_indent) "{\n";
        (match default_branch with
        | Some branch_fn ->
            emit_fn_body (indent + (standard_indent * 2)) branch_fn;
            write ~indent:(indent + (standard_indent * 2)) "break;\n"
        | None ->
            write ~indent:(indent + (standard_indent * 2))
              (Printf.sprintf "Rz.Fail($\"Runtime error: unexpected tag {Rz.ObjectTag(Rz.UnboxPtr(%s))}\");\n" (mangle scrutinee));
            write ~indent:(indent + (standard_indent * 2)) "throw Rz.RuntimeFailure(\"unreachable\");\n");
        write ~indent:(indent + standard_indent) "}\n";
        write ~indent "}\n"
    | FnDec (x, f) ->
        if Option.is_none (int_of_string_opt x) then
          write ~indent (Printf.sprintf "Rz.RefcountDecBox(%s);\n" (mangle x));
        emit_fn_body indent f
    | FnInc (x, f) ->
        if Option.is_none (int_of_string_opt x) then
          write ~indent (Printf.sprintf "Rz.RefcountIncBox(%s);\n" (mangle x));
        emit_fn_body indent f
  and emit_rexpr = function
    | RConst c -> emit_primitive (Const c)
    | RCall ("eq", [p1; p2]) -> Printf.sprintf "Rz.Eq(%s, %s)" (emit_primitive p1) (emit_primitive p2)
    | RCall ("start_event_loop", _) -> "Rz.StartEventLoop()"
    | RCall ("console_out_signal", [signal]) ->
        Printf.sprintf "Rz.RegisterOutputSignal(new RzBox[] { %s })" (emit_primitive signal)
    | RCall (f, args) when is_builtin f ->
        Printf.sprintf "Rz.CallBuiltin(%s, %s)" (csharp_string_literal f) (mk_args_array args)
    | RCall (f, args) -> Printf.sprintf "Rz.Call(%s, %s)" (mangle f) (mk_args_array args)
    | RCtor Ctor { tag; fields } ->
        Printf.sprintf "Rz.Ptr(Rz.Ctor(%d, %s))" tag (mk_args_array fields)
    | RVarApp (f, x) -> Printf.sprintf "Rz.Apply1(Rz.UnboxPtr(%s), %s)" (mangle f) (emit_primitive x)
    | RPartialApp (f, args) ->
        (match M.find_opt f arity_map with
        | None -> failwith (Printf.sprintf "Function %s not found in arity map" f)
        | Some arity ->
            if is_builtin f then
              Printf.sprintf "Rz.LiftBuiltin(%s, %d, %s)" (csharp_string_literal f) arity (mk_args_array args)
            else Printf.sprintf "Rz.LiftFun(%s, %d, %s)" (mangle f) arity (mk_args_array args))
    | RProj (i, x) -> Printf.sprintf "Rz.ObjectGetField(Rz.UnboxPtr(%s), %d)" (mangle x) i
    | RCtor Signal { head; tail } ->
        Printf.sprintf "Rz.PtrSig(Rz.SignalCtor(%s, %s))" (emit_primitive head) (emit_primitive tail)
    | RReset n -> Printf.sprintf "Rz.Ptr(Rz.ResetObject(Rz.UnboxPtr(%s)))" (mangle n)
    | RReuse (n, Ctor { tag; fields }) ->
        Printf.sprintf "Rz.Ptr(Rz.ReuseObject(Rz.UnboxPtr(%s), %d, %s))" (mangle n) tag (mk_args_array fields)
    | RReuse (n, Signal { head; tail }) ->
        Printf.sprintf "Rz.PtrSig(Rz.ReuseSignal(Rz.UnboxPtr(%s), %s, %s))" (mangle n) (emit_primitive head)
          (emit_primitive tail)
  and emit_primitive = function
    | Var x -> as_possible_function_access x []
    | Const CInt i -> Printf.sprintf "Rz.Int(%dL)" i
    | Const CBool true -> "Rz.Ptr(Rz.BoolCtor(true))"
    | Const CBool false -> "Rz.Ptr(Rz.BoolCtor(false))"
    | Const CNever -> "Rz.Never"
    | Const (CString s) -> Printf.sprintf "Rz.StrLit(%s)" (csharp_string_literal s)
    | Const Ast.CUnit -> "Rz.Unit"
  and as_possible_function_access name args =
    match M.find_opt name arity_map with
    | None -> mangle name
    | Some arity ->
        if is_builtin name then
          Printf.sprintf "Rz.LiftBuiltin(%s, %d, %s)" (csharp_string_literal name) arity (mk_args_array args)
        else Printf.sprintf "Rz.LiftFun(%s, %d, %s)" (mangle name) arity (mk_args_array args)
  and mk_args_array args =
    let args =
      args
      |> List.map (function Const _ as e -> emit_primitive e | Var arg -> as_possible_function_access arg [])
      |> String.concat ", "
    in
    Printf.sprintf "new RzBox[] { %s }" args
  in
  Fun.protect ~finally:(fun () -> close_out out_file) (fun () -> emit_program p; flush out_file)

let emit_project program project_dir =
  ensure_dir project_dir;
  emit_project_file project_dir;
  emit_csharp_code program (Filename.concat project_dir "Program.cs");
  let runtime_root = resolve_runtime_root () in
  copy_file (Filename.concat runtime_root "RizzoRuntime.cs") (Filename.concat project_dir "RizzoRuntime.cs")
