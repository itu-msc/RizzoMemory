let run_console_program ?(delay_s = 0.) ~program ~input () =
  let normalize_line line =
    if String.ends_with ~suffix:"\r" line
    then String.sub line 0 (String.length line - 1)
    else line
  in
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
  in
  let read_all_lines in_chan =
    let rec go acc =
      match input_line in_chan with
      | line -> go (normalize_line line :: acc)
      | exception End_of_file -> List.rev acc
    in
    go []
  in
  let input_file = Filename.temp_file "test" ".rizz" in
  let input_channel = open_out input_file in
  output_string input_channel program;
  close_out input_channel;
  let output_file = Filename.temp_file "test" ".c" in
  let binary_file = Filename.temp_file "test" ".exe" in
  let original_cwd = Sys.getcwd () in
  try
    Fun.protect
      ~finally:(fun () ->
        Sys.chdir original_cwd;
        if Sys.file_exists input_file
        then Sys.remove input_file)
      (fun () ->
        Rizzoc.compile_from_file input_file output_file;
        Sys.chdir "../../../..";
        let command =
          Rizzoc.to_shell_command
            (Rizzoc.generated_c_compiler_invocation ~input_file:output_file
               ~output_file:binary_file ())
        in
        let status = Sys.command command in
        if status <> 0
        then
          let dir = Sys.getcwd () in
          Alcotest.failf "C compile failed with status %d.\nCommand: %s,\nat dir: %s" status command dir
        else
          if Sys.win32 then
            let stdin_file = Filename.temp_file "test-input" ".txt" in
            let stdout_file = Filename.temp_file "test-stdout" ".txt" in
            let stderr_file = Filename.temp_file "test-stderr" ".txt" in
            let script_file = Filename.temp_file "test-run" ".cmd" in
            Fun.protect
              ~finally:(fun () ->
                List.iter
                  (fun path -> if Sys.file_exists path then Sys.remove path)
                  [stdin_file; stdout_file; stderr_file; script_file])
              (fun () ->
                let stdin_chan = open_out stdin_file in
                output_string stdin_chan (Printf.sprintf "%s\n" input);
                close_out stdin_chan;
                let script_chan = open_out script_file in
                Printf.fprintf script_chan "%s < %s > %s 2> %s\n"
                    (Filename.quote binary_file)
                    (Filename.quote stdin_file)
                    (Filename.quote stdout_file)
                  (Filename.quote stderr_file);
                close_out script_chan;
                let run_command =
                  Printf.sprintf "cmd /d /c %s" (Filename.quote script_file)
                in
                let exit_code = Sys.command run_command in
                let outputs = read_all_lines_from_file stdout_file in
                let errors = read_all_lines_from_file stderr_file in
                if errors <> []
                then Alcotest.failf "Program stderr was not empty: [%s]" (String.concat "; " errors)
                else outputs, Unix.WEXITED exit_code)
          else
          let (in_chan, out_chan, err_chan) =
            Unix.open_process_args_full binary_file [| binary_file |] (Unix.environment ())
          in
          output_string out_chan (Printf.sprintf "%s\n" input);
          flush out_chan;
          if delay_s > 0.
          then Unix.sleepf delay_s;
          close_out out_chan;
          let outputs = read_all_lines in_chan in
          let errors = read_all_lines err_chan in
          let process_status = Unix.close_process_full (in_chan, out_chan, err_chan) in
          if errors <> []
          then Alcotest.failf "Program stderr was not empty: [%s]" (String.concat "; " errors)
          else
            outputs, process_status)
  with exn ->
    Alcotest.failf "Compilation failed with exception: %s" (Printexc.to_string exn)

let test_simple_console_identity () =
  let program = 
    {|
      fun entry x = 
        let my_console_sig = mk_sig (wait console) in
        let my_console = console_out_signal ("" :: my_console_sig) in
        let q = quit_at (my_console_sig) in
        start_event_loop ()
    |}
  in
  let expected_output = "Hello world!" in
  let outputs, process_status = run_console_program ~program ~input:expected_output () in
  Alcotest.(check string) "initial output" "" (List.hd outputs);
  Alcotest.(check bool) "console output appears" true (List.mem expected_output outputs);
  Alcotest.(check int) "process exit code" 0
    (match process_status with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED signal -> Alcotest.failf "Process was terminated by signal %d" signal
    | Unix.WSTOPPED signal -> Alcotest.failf "Process was stopped by signal %d" signal)

let test_issue_filterl_variant_outputs_quit () =
  let program =
    {|
      fun entry x =
        let console_sig = mk_sig (wait console) in
        let quit_sig = filterL (fun x -> x == "quit") console_sig in
        let _o = console_out_signal ("" :: quit_sig) in
        let _x = quit_at quit_sig in
        start_event_loop ()
    |}
  in
  let outputs, process_status = run_console_program ~delay_s:1.0 ~program ~input:"quit" () in
  if not (List.mem "quit" outputs)
  then Alcotest.failf "quit signal appears: captured outputs were [%s]" (String.concat "; " outputs);
  Alcotest.(check int) "process exit code" 0
    (match process_status with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED signal -> Alcotest.failf "Process was terminated by signal %d" signal
    | Unix.WSTOPPED signal -> Alcotest.failf "Process was stopped by signal %d" signal)

let test_issue_filterl_variant_outputs_quit_after_non_match () =
  let program =
    {|
      fun entry x =
        let console_sig = mk_sig (wait console) in
        let quit_sig = filterL (fun x -> x == "quit") console_sig in
        let _o = console_out_signal ("" :: quit_sig) in
        let _x = quit_at quit_sig in
        start_event_loop ()
    |}
  in
  let outputs, process_status = run_console_program ~delay_s:1.0 ~program ~input:"ls\nquit" () in
  if not (List.mem "quit" outputs)
  then Alcotest.failf "quit after non-match appears: captured outputs were [%s]" (String.concat "; " outputs);
  Alcotest.(check int) "process exit code" 0
    (match process_status with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED signal -> Alcotest.failf "Process was terminated by signal %d" signal
    | Unix.WSTOPPED signal -> Alcotest.failf "Process was stopped by signal %d" signal)

let test_issue_map_l_variant_outputs_hello_world () =
  let program =
    {|
      fun entry x =
        let console_sig = mk_sig (wait console) in
        let quit_sig = map_l (fun x -> "Hello world") console_sig in
        let _o = console_out_signal ("" :: quit_sig) in
        let _x = quit_at quit_sig in
        start_event_loop ()
    |}
  in
  let outputs, process_status = run_console_program ~delay_s:1.0 ~program ~input:"quit" () in
  Alcotest.(check bool) "mapped output appears" true (List.mem "Hello world" outputs);
  Alcotest.(check int) "process exit code" 0
    (match process_status with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED signal -> Alcotest.failf "Process was terminated by signal %d" signal
    | Unix.WSTOPPED signal -> Alcotest.failf "Process was stopped by signal %d" signal)

let test_clock_signal_outputs_ticks () =
  let progam =
    {|
      fun entry x =
        let ticks = clock(20) in
        let out = console_out_signal(ticks) in
        let q = quit_at(tail ticks) in
        start_event_loop ()
    |}
  in
  let normalize_line line =
    if String.ends_with ~suffix:"\r" line
    then String.sub line 0 (String.length line - 1)
    else line
  in
  let output_file = Filename.temp_file "clock" ".c" in
  let binary_file = Filename.temp_file "clock" ".exe" in
  let original_cwd = Sys.getcwd () in
  try
    Fun.protect
      ~finally:(fun () -> Sys.chdir original_cwd)
      (fun () ->
        Rizzoc.compile_from_string progam output_file;
        Sys.chdir "../../../..";
        let command =
          Rizzoc.to_shell_command
            (Rizzoc.generated_c_compiler_invocation ~input_file:output_file
               ~output_file:binary_file ())
        in
        let status = Sys.command command in
        if status <> 0
        then
          let dir = Sys.getcwd () in
          Alcotest.failf "C compile failed with status %d.\nCommand: %s,\nat dir: %s" status command dir
        else
          let (in_chan, out_chan) = Unix.open_process binary_file in
          close_out out_chan;
          let rec read_all acc =
            match input_line in_chan with
            | line -> read_all (normalize_line line :: acc)
            | exception End_of_file -> List.rev acc
          in
          let outputs = read_all [] in
          close_in in_chan;
          let process_status = Unix.close_process (in_chan, out_chan) in
          Alcotest.(check bool) "initial clock output appears" true (List.mem "0" outputs);
          Alcotest.(check bool) "first clock tick appears" true (List.mem "20" outputs);
          Alcotest.(check int) "process exit code" 0
            (match process_status with
            | Unix.WEXITED code -> code
            | Unix.WSIGNALED signal -> Alcotest.failf "Process was terminated by signal %d" signal
            | Unix.WSTOPPED signal -> Alcotest.failf "Process was stopped by signal %d" signal))
  with exn ->
    Alcotest.failf "Clock compilation failed with exception: %s" (Printexc.to_string exn)

let end_to_end_tests = [
  "Inputing on the consile outputs the same thing", `Quick, test_simple_console_identity;
  "Issue filterL variant outputs quit", `Quick, test_issue_filterl_variant_outputs_quit;
  "Issue filterL variant outputs quit after non-match", `Quick, test_issue_filterl_variant_outputs_quit_after_non_match;
  "Issue map_l variant outputs Hello world", `Quick, test_issue_map_l_variant_outputs_hello_world;
  "Clock signal outputs ticks", `Quick, test_clock_signal_outputs_ticks;
]
