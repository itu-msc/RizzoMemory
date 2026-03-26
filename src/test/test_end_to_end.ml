let test_simple_console_identity () =
  let progam = 
    {|
      fun mk_sig later = 
        (fun a -> a :: mk_sig later) |> later

      fun entry x = 
        let my_console_sig = mk_sig (wait console) in
        let my_console = console_out_signal ("" :: my_console_sig) in
        let q = quit_at (my_console_sig) in
        start_event_loop ()
    |} 
  in
  let expected_output = "Hello world!" in
  let normalize_line line =
    if String.ends_with ~suffix:"\r" line
    then String.sub line 0 (String.length line - 1)
    else line
  in
  (* let tempdir = Filename.temp_dir in *)
  let output_file = Filename.temp_file "test" ".c" in
  let binary_file = Filename.temp_file "test" ".exe" in
  let original_cwd = Sys.getcwd () in
  try
    Fun.protect
      ~finally:(fun () -> Sys.chdir original_cwd)
      (fun () ->
        Rizzoc.compile_from_string progam output_file;
        Sys.chdir "../../../..";
        let command = Printf.sprintf "clang -m64 -Isrc/runtime -o %s %s" binary_file output_file in
        let status = Sys.command command in
        if status <> 0
        then
          let dir = Sys.getcwd () in
          Alcotest.failf "C compile failed with status %d.\nCommand: %s,\nat dir: %s" status command dir
        else
          let (in_chan, out_chan) = Unix.open_process binary_file in
          output_string out_chan (Printf.sprintf "%s\n" expected_output);
          flush out_chan;
          close_out out_chan;
          let rec read_all acc =
            match input_line in_chan with
            | line -> read_all (normalize_line line :: acc)
            | exception End_of_file -> List.rev acc
          in
          let outputs = read_all [] in
          close_in in_chan;
          let process_status = Unix.close_process (in_chan, out_chan) in
          Alcotest.(check string) "initial output" "" (List.hd outputs);
          Alcotest.(check bool) "console output appears" true (List.mem expected_output outputs);
          Alcotest.(check int) "process exit code" 0
            (match process_status with
            | Unix.WEXITED code -> code
            | Unix.WSIGNALED signal -> Alcotest.failf "Process was terminated by signal %d" signal
            | Unix.WSTOPPED signal -> Alcotest.failf "Process was stopped by signal %d" signal))
  with exn ->
    Alcotest.failf "Compilation failed with exception: %s" (Printexc.to_string exn)

let end_to_end_tests = [
  "Inputing on the consile outputs the same thing", `Quick, test_simple_console_identity;
]
