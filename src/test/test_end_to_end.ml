let run_console_program ?(delay_s = 0.) ?(debug_malloc = false) ~program ~input () =
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
          let command =
            Rizzoc.to_shell_command
              (Rizzoc.generated_c_compiler_invocation ~input_file:output_file
                 ~output_file:binary_file ~debug_malloc ())
          in
          if debug_malloc then command ^ " -D__RZ_DEBUG_INFO" else command
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

let contains_substring ~text ~substring =
  let text_length = String.length text in
  let substring_length = String.length substring in
  let rec loop index =
    if index + substring_length > text_length then false
    else if String.sub text index substring_length = substring then true
    else loop (index + 1)
  in
  substring_length = 0 || loop 0

let compile_program_binary ~prefix ~program =
  let output_file = Filename.temp_file prefix ".c" in
  let binary_file = Filename.temp_file prefix ".exe" in
  let original_cwd = Sys.getcwd () in
  let cleanup () =
    List.iter (fun path -> if Sys.file_exists path then Sys.remove path) [output_file; binary_file]
  in
  try
    Rizzoc.compile_from_string program output_file;
    Sys.chdir "../../../..";
    let command =
      Rizzoc.to_shell_command
        (Rizzoc.generated_c_compiler_invocation ~input_file:output_file
           ~output_file:binary_file ())
    in
    let status = Sys.command command in
    Sys.chdir original_cwd;
    if status <> 0
    then (
      cleanup ();
      Alcotest.failf "C compile failed with status %d. Command: %s" status command);
    binary_file, cleanup
  with exn ->
    Sys.chdir original_cwd;
    cleanup ();
    Alcotest.failf "Compilation failed with exception: %s" (Printexc.to_string exn)

let read_available_lines ?(timeout_s = 2.0) in_chan =
  let fd = Unix.descr_of_in_channel in_chan in
  let deadline = Unix.gettimeofday () +. timeout_s in
  let normalize_line line =
    if String.ends_with ~suffix:"\r" line
    then String.sub line 0 (String.length line - 1)
    else line
  in
  let rec go acc =
    let remaining = deadline -. Unix.gettimeofday () in
    if remaining <= 0. then List.rev acc
    else
      let ready, _, _ = Unix.select [fd] [] [] remaining in
      if ready = [] then List.rev acc
      else
        match input_line in_chan with
        | line -> go (normalize_line line :: acc)
        | exception End_of_file -> List.rev acc
  in
  go []

let get_free_tcp_port () =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close sock)
    (fun () ->
      Unix.setsockopt sock Unix.SO_REUSEADDR true;
      Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      match Unix.getsockname sock with
      | Unix.ADDR_INET (_, port) -> port
      | _ -> Alcotest.fail "expected inet socket")

let test_random_int_outputs_value_in_range () =
  let program =
    {|
      fun entry x =
        let n = random_int 10 in
        let _o = console_out_signal (string_of_int n :: never) in
        start_event_loop ()
    |}
  in
  let outputs, process_status = run_console_program ~program ~input:"trigger" () in
  let maybe_value =
    List.find_map
      (fun line ->
        match int_of_string_opt line with
        | Some n when n >= 0 && n < 10 -> Some n
        | _ -> None)
      outputs
  in
  Alcotest.(check bool) "random output in range" true (Option.is_some maybe_value);
  Alcotest.(check int) "process exit code" 0
    (match process_status with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED signal -> Alcotest.failf "Process was terminated by signal %d" signal
    | Unix.WSTOPPED signal -> Alcotest.failf "Process was stopped by signal %d" signal)

let test_random_int_rejects_non_positive_bound () =
  if Sys.win32 then ()
  else
    let program =
      {|
        fun entry x =
          let n = random_int 0 in
          let _o = console_out_signal (string_of_int n :: never) in
          start_event_loop ()
      |}
    in
    let binary_file, cleanup = compile_program_binary ~prefix:"random-invalid" ~program in
    Fun.protect
      ~finally:cleanup
      (fun () ->
        let in_chan, out_chan, err_chan =
          Unix.open_process_args_full binary_file [| binary_file |] (Unix.environment ())
        in
        close_out out_chan;
        let _outputs = read_available_lines ~timeout_s:0.1 in_chan in
        let errors = read_available_lines err_chan in
        let process_status = Unix.close_process_full (in_chan, out_chan, err_chan) in
        Alcotest.(check bool) "error mentions positive upper bound" true
          (List.exists (fun line -> contains_substring ~text:line ~substring:"positive upper bound") errors);
        match process_status with
        | Unix.WEXITED code -> Alcotest.(check bool) "process exits nonzero" true (code <> 0)
        | Unix.WSIGNALED _ -> ()
        | Unix.WSTOPPED signal -> Alcotest.failf "Process was stopped by signal %d" signal)

let test_port_output_loops_into_port_input () =
  let port = get_free_tcp_port () in
  let program =
    Printf.sprintf
      {|
        fun entry x =
          let port_sig = port_input %d in
          let displayed = map (fun x -> "port: " + x + "; Random number:" + string_of_int (random_int 100)) port_sig in
          let _send = port_out_signal %d ("hello from tcp" :: never) in
          let _out = console_out_signal displayed in
          let _q = quit_at (tail port_sig) in
          start_event_loop ()
      |}
      port
      port
  in
  let outputs, process_status = run_console_program ~delay_s:1.0 ~program ~input:"" () in
  Alcotest.(check bool) "tcp loopback output appears" true
    (List.exists
       (fun line ->
         contains_substring ~text:line ~substring:"port: hello from tcp; Random number:")
       outputs);
  Alcotest.(check int) "process exit code" 0
    (match process_status with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED signal -> Alcotest.failf "Process was terminated by signal %d" signal
    | Unix.WSTOPPED signal -> Alcotest.failf "Process was stopped by signal %d" signal)

let test_console_signal_input_string_is_freed () =
  let program =
    {|
      fun entry x =
        let console_sig = mk_sig (wait console) in
        let _echo = console_out_signal ("" :: console_sig) in
        start_event_loop ()
    |}
  in
  let outputs, process_status =
    run_console_program ~debug_malloc:true ~program ~input:"hello\nworld" ()
  in
  Alcotest.(check bool) "console echo includes first input" true (List.mem "hello" outputs);
  Alcotest.(check bool) "console echo includes second input" true (List.mem "world" outputs);
  Alcotest.(check bool) "first input is freed" true
    (List.exists
       (fun line -> contains_substring ~text:line ~substring:"Freeing string: hello")
       outputs);
  Alcotest.(check int) "process exit code" 0
    (match process_status with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED signal -> Alcotest.failf "Process was terminated by signal %d" signal
    | Unix.WSTOPPED signal -> Alcotest.failf "Process was stopped by signal %d" signal)

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

let test_runtime_mod_and_string_helpers () =
  let program =
    {|
      fun entry x =
        let result =
          if string_contains "functional reactive programming" "reactive" then
            if string_starts_with "hello" "he" then
              if string_ends_with "hello" "lo" then
                if 9 % 4 == 1 then "ok" else "bad"
              else "bad"
            else "bad"
          else "bad"
        in
        let _o = console_out_signal (result :: never) in
        start_event_loop ()
    |}
  in
  let outputs, process_status = run_console_program ~program ~input:"trigger" () in
  Alcotest.(check bool) "computed output appears" true (List.mem "ok" outputs);
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

let test_ostar_constructor_maps_console_signal () =
  let program =
    {|
      fun mapp f (x :: xs) : ('a -> 'b) -> Signal 'a -> Signal 'b =
          (f x) :: (laterapp (ostar (delay mapp) (delay f)) xs)


      fun entry x =
          let console_sig = mk_sig (wait console) in
          let quit_sig = mapp (fun x -> "Hello world") |> console_sig in
          let _o = console_out_signal ("" :: quit_sig) in
          let _x = quit_at quit_sig in
          start_event_loop ()
    |}
  in
  let outputs, process_status = run_console_program ~delay_s:1.0 ~program ~input:"quit" () in
  Alcotest.(check bool) "mapped console output appears" true (List.mem "Hello world" outputs);
  Alcotest.(check int) "process exit code" 0
    (match process_status with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED signal -> Alcotest.failf "Process was terminated by signal %d" signal
    | Unix.WSTOPPED signal -> Alcotest.failf "Process was stopped by signal %d" signal)

let test_console_out_signal_l_registers_when_later_ticks () =
  let program =
    {|
      fun entry x =
        let console_sig = mk_sig (wait console) in
        let quit_sig = filter_l (fun command -> command == "quit") console_sig in
        let _o = console_out_signal_l quit_sig in
        start_event_loop ()
    |}
  in
  let outputs, process_status = run_console_program ~delay_s:1.0 ~program ~input:"ls\nquit" () in
  let signal_outputs = List.filter (fun line -> not (String.starts_with ~prefix:"result: " line)) outputs in
  Alcotest.(check (list string)) "deferred output starts when later resolves" ["quit"] signal_outputs;
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

let test_string_split_and_list_length_output_count () =
  let program =
    {|
      fun entry x =
        let parts = string_split "a,b,,c," "," in
        let count = list_length parts in
        let _o = console_out_signal (string_of_int count :: never) in
        start_event_loop ()
    |}
  in
  let outputs, process_status = run_console_program ~program ~input:"trigger" () in
  Alcotest.(check bool) "split count output appears" true (List.mem "5" outputs);
  Alcotest.(check int) "process exit code" 0
    (match process_status with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED signal -> Alcotest.failf "Process was terminated by signal %d" signal
    | Unix.WSTOPPED signal -> Alcotest.failf "Process was stopped by signal %d" signal)

let test_list_pattern_match_outputs_head () =
  let program =
    {|
      fun entry x =
        let nums = [7, 8] in
        let result =
          match nums with
          | head :: rest -> string_of_int head
          | [] -> "empty"
        in
        let _o = console_out_signal (result :: never) in
        start_event_loop ()
    |}
  in
  let outputs, process_status = run_console_program ~program ~input:"trigger" () in
  Alcotest.(check bool) "list pattern output appears" true (List.mem "7" outputs);
  Alcotest.(check int) "process exit code" 0
    (match process_status with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED signal -> Alcotest.failf "Process was terminated by signal %d" signal
    | Unix.WSTOPPED signal -> Alcotest.failf "Process was stopped by signal %d" signal)

let end_to_end_tests = [
  "Inputing on the consile outputs the same thing", `Quick, test_simple_console_identity;
  "random_int outputs value in range", `Quick, test_random_int_outputs_value_in_range;
  "random_int rejects non-positive bound", `Quick, test_random_int_rejects_non_positive_bound;
  "port output loops into port input", `Quick, test_port_output_loops_into_port_input;
  "Issue filterL variant outputs quit", `Quick, test_issue_filterl_variant_outputs_quit;
  "Issue filterL variant outputs quit after non-match", `Quick, test_issue_filterl_variant_outputs_quit_after_non_match;
  "Issue map_l variant outputs Hello world", `Quick, test_issue_map_l_variant_outputs_hello_world;
  "ostar constructor maps console signal", `Quick, test_ostar_constructor_maps_console_signal;
  "console_out_signal_l registers when later ticks", `Quick, test_console_out_signal_l_registers_when_later_ticks;
  "runtime mod and string helpers", `Quick, test_runtime_mod_and_string_helpers;
  "string_split and list_length output count", `Quick, test_string_split_and_list_length_output_count;
  "list pattern match outputs head", `Quick, test_list_pattern_match_outputs_head;
  "Console signal input string is freed", `Quick, test_console_signal_input_string_is_freed;
  "Clock signal outputs ticks", `Quick, test_clock_signal_outputs_ticks;
]
