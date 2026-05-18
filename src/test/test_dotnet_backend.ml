let dotnet_available () = Rizzoc.command_exists "dotnet"

let normalize_line line =
  if String.ends_with ~suffix:"\r" line
  then String.sub line 0 (String.length line - 1)
  else line

let read_all_lines_from_file path =
  let in_chan = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in in_chan)
    (fun () ->
      let rec go acc =
        match input_line in_chan with
        | line -> go (normalize_line line :: acc)
        | exception End_of_file -> List.rev acc
      in
      go [])

let read_all_lines in_chan =
  let rec go acc =
    match input_line in_chan with
    | line -> go (normalize_line line :: acc)
    | exception End_of_file -> List.rev acc
  in
  go []

let remove_tree root =
  let rec go path =
    if Sys.file_exists path then
      if Sys.is_directory path then (
        Sys.readdir path
        |> Array.iter (fun entry -> go (Filename.concat path entry));
        Unix.rmdir path)
      else Sys.remove path
  in
  go root

let with_temp_project prefix f =
  let marker = Filename.temp_file prefix ".marker" in
  Sys.remove marker;
  let project_dir = marker ^ ".dotnet" in
  Fun.protect ~finally:(fun () -> remove_tree project_dir) (fun () -> f project_dir)

let publish_project project_dir =
  let publish_dir = Filename.concat project_dir "publish" in
  let command =
    Rizzoc.dotnet_to_shell_command
      (Rizzoc.generated_dotnet_publish_invocation ~project_dir ~publish_dir ())
  in
  let status = Sys.command command in
  if status <> 0 then Alcotest.failf ".NET publish failed with status %d. Command: %s" status command;
  Filename.concat publish_dir
    (if Sys.win32 then Rizzoc.dotnet_assembly_name ^ ".exe" else Rizzoc.dotnet_assembly_name)

let run_executable ?(input = "") executable =
  if Sys.win32 then
    let stdin_file = Filename.temp_file "dotnet-input" ".txt" in
    let stdout_file = Filename.temp_file "dotnet-stdout" ".txt" in
    let stderr_file = Filename.temp_file "dotnet-stderr" ".txt" in
    let script_file = Filename.temp_file "dotnet-run" ".cmd" in
    Fun.protect
      ~finally:(fun () ->
        List.iter
          (fun path -> if Sys.file_exists path then Sys.remove path)
          [stdin_file; stdout_file; stderr_file; script_file])
      (fun () ->
        let stdin_chan = open_out stdin_file in
        output_string stdin_chan input;
        if input <> "" && not (String.ends_with ~suffix:"\n" input) then output_char stdin_chan '\n';
        close_out stdin_chan;
        let script_chan = open_out script_file in
        Printf.fprintf script_chan "%s < %s > %s 2> %s\n"
          (Filename.quote executable)
          (Filename.quote stdin_file)
          (Filename.quote stdout_file)
          (Filename.quote stderr_file);
        close_out script_chan;
        let exit_code = Sys.command (Printf.sprintf "cmd /d /c %s" (Filename.quote script_file)) in
        let outputs = read_all_lines_from_file stdout_file in
        let errors = read_all_lines_from_file stderr_file in
        outputs, errors, exit_code)
  else
    let in_chan, out_chan, err_chan =
      Unix.open_process_args_full executable [| executable |] (Unix.environment ())
    in
    output_string out_chan input;
    if input <> "" && not (String.ends_with ~suffix:"\n" input) then output_char out_chan '\n';
    close_out out_chan;
    let outputs = read_all_lines in_chan in
    let errors = read_all_lines err_chan in
    let status = Unix.close_process_full (in_chan, out_chan, err_chan) in
    let exit_code =
      match status with
      | Unix.WEXITED code -> code
      | Unix.WSIGNALED signal -> 128 + signal
      | Unix.WSTOPPED signal -> 128 + signal
    in
    outputs, errors, exit_code

let compile_dotnet program project_dir =
  Rizzoc.compile_from_string ~backend:Rizzoc.Dotnet program project_dir

let contains_substring ~text ~substring =
  let text_length = String.length text in
  let substring_length = String.length substring in
  let rec loop index =
    if index + substring_length > text_length then false
    else if String.sub text index substring_length = substring then true
    else loop (index + 1)
  in
  substring_length = 0 || loop 0

let test_emits_csharp_for_core_rc_shapes () =
  with_temp_project "dotnet-shape" (fun project_dir ->
      let program =
        {|
          fun entry x =
            let p = (1, "ok") in
            let a = fst p in
            let total = a + 2 in
            let _o = console_out_signal (string_of_int total :: never) in
            start_event_loop ()
        |}
      in
      compile_dotnet program project_dir;
      let source_file = Filename.concat project_dir "Program.cs" in
      let source = In_channel.with_open_text source_file In_channel.input_all in
      Alcotest.(check bool) "emits constructor" true
        (contains_substring ~text:source ~substring:"Rz.Ptr(Rz.Ctor");
      Alcotest.(check bool) "emits builtin call" true
        (contains_substring ~text:source ~substring:"Rz.CallBuiltin(\"add\"");
      Alcotest.(check bool) "emits projection" true
        (contains_substring ~text:source ~substring:"Rz.ObjectGetField"))

let test_dotnet_constant_signal_runs () =
  if not (dotnet_available ()) then ()
  else
    with_temp_project "dotnet-run-const" (fun project_dir ->
        let program =
          {|
            fun entry x =
              let _o = console_out_signal (string_of_int (1 + 2) :: never) in
              start_event_loop ()
          |}
        in
        compile_dotnet program project_dir;
        let executable = publish_project project_dir in
        let outputs, errors, exit_code = run_executable executable in
        Alcotest.(check (list string)) "stderr" [] errors;
        Alcotest.(check int) "exit code" 0 exit_code;
        Alcotest.(check bool) "prints computed value" true (List.mem "3" outputs))

let test_dotnet_console_signal_runs () =
  if not (dotnet_available ()) then ()
  else
    with_temp_project "dotnet-run-console" (fun project_dir ->
        let program =
          {|
            fun entry x =
              let console_sig = mk_sig (wait console) in
              let quit_sig = filterL (fun x -> x == "quit") console_sig in
              let _o = console_out_signal ("" :: console_sig) in
              let _x = quit_at quit_sig in
              start_event_loop ()
          |}
        in
        let source_file = Filename.temp_file "dotnet-console" ".rizz" in
        Fun.protect
          ~finally:(fun () -> if Sys.file_exists source_file then Sys.remove source_file)
          (fun () ->
            let out = open_out source_file in
            output_string out program;
            close_out out;
            Rizzoc.compile_from_files ~backend:Rizzoc.Dotnet [source_file] project_dir;
            let executable = publish_project project_dir in
            let outputs, errors, exit_code = run_executable ~input:"quit" executable in
            Alcotest.(check (list string)) "stderr" [] errors;
            Alcotest.(check int) "exit code" 0 exit_code;
            Alcotest.(check bool) "echoes console input" true (List.mem "quit" outputs)))

let tests =
  [ "emits C# for core RC shapes", `Quick, test_emits_csharp_for_core_rc_shapes;
    "dotnet constant signal runs", `Quick, test_dotnet_constant_signal_runs;
    "dotnet console signal runs", `Quick, test_dotnet_console_signal_runs;
  ]
