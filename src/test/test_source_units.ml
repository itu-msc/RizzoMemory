let write_temp_file ~prefix ~suffix (contents : string) =
  let path = Filename.temp_file prefix suffix in
  let oc = open_out path in
  output_string oc contents;
  close_out oc;
  path

let remove_if_exists path =
  if Sys.file_exists path then
    Sys.remove path

let with_temp_file ~prefix ~suffix contents f =
  let path = write_temp_file ~prefix ~suffix contents in
  Fun.protect
    ~finally:(fun () -> remove_if_exists path)
    (fun () -> f path)

let with_temp_output prefix f =
  let output_path = Filename.temp_file prefix ".c" in
  Fun.protect
    ~finally:(fun () -> remove_if_exists output_path)
    (fun () -> f output_path)

let test_compile_from_file_includes_signal_prelude () =
  let program =
    "fun entry x : Int -> Int =\n"
    ^ "  head (map (fun y -> y) (x :: never))\n"
  in
  with_temp_file ~prefix:"prelude-user" ~suffix:".rizz" program (fun input_path ->
      with_temp_output "prelude-output" (fun output_path ->
          Rizzoc.compile_from_file input_path output_path;
          Alcotest.(check bool) "generated C file exists" true (Sys.file_exists output_path)))

let test_compile_from_files_respects_left_to_right_order () =
  let first_file = "fun double x : Int -> Int = x + x\n" in
  let second_file = "fun entry x : Int -> Int = double x\n" in
  with_temp_file ~prefix:"rizzo-a" ~suffix:".rizz" first_file (fun file_a ->
      with_temp_file ~prefix:"rizzo-b" ~suffix:".rizz" second_file (fun file_b ->
          with_temp_output "ordered-files" (fun output_path ->
              Rizzoc.compile_from_files [file_a; file_b] output_path;
              Alcotest.(check bool) "ordered compile produced output" true (Sys.file_exists output_path))))

let test_compile_from_files_rejects_reverse_order_dependencies () =
  let first_file = "fun entry x : Int -> Int = double x\n" in
  let second_file = "fun double x : Int -> Int = x + x\n" in
  with_temp_file ~prefix:"rizzo-a" ~suffix:".rizz" first_file (fun file_a ->
      with_temp_file ~prefix:"rizzo-b" ~suffix:".rizz" second_file (fun file_b ->
          with_temp_output "reverse-ordered-files" (fun output_path ->
              match
                try
                  Rizzoc.compile_from_files [file_a; file_b] output_path;
                  None
                with
                | Failure msg -> Some msg
              with
              | Some msg -> Alcotest.(check bool) "reverse order fails" true (String.length msg > 0)
              | None -> Alcotest.fail "expected compile_from_files to reject reverse-order dependency")))

let test_compile_from_file_rejects_prelude_name_collision () =
  let program =
    "fun map f s = s\n"
    ^ "fun entry x = x\n"
  in
  with_temp_file ~prefix:"prelude-collision" ~suffix:".rizz" program (fun input_path ->
      with_temp_output "prelude-collision" (fun output_path ->
          match
            try
              Rizzoc.compile_from_file input_path output_path;
              None
            with
            | Failure msg -> Some msg
          with
          | Some msg -> Alcotest.(check string) "duplicate definition error" "Validation failed" msg
          | None -> Alcotest.fail "expected compile_from_file to reject prelude name collision"))

let test_compile_from_file_accepts_overridden_stdlib_file () =
  let custom_stdlib = "fun helper x : Int -> Int = x + x\n" in
  let program = "fun entry x : Int -> Int = helper x\n" in
  with_temp_file ~prefix:"custom-stdlib" ~suffix:".rizz" custom_stdlib (fun stdlib_path ->
      with_temp_file ~prefix:"custom-user" ~suffix: ".rizz" program (fun input_path ->
          with_temp_output "custom-stdlib-output" (fun output_path ->
              Rizzoc.compile_from_file ~stdlib_path input_path output_path;
              Alcotest.(check bool) "custom stdlib override compiled" true (Sys.file_exists output_path))))

let test_compile_from_file_accepts_included_file () =
  let include_program = "fun helper x : Int -> Int = x + x\n" in
  let user_program = "fun entry x : Int -> Int = helper x\n" in
  with_temp_file ~prefix:"include-helper" ~suffix:".rizz" include_program (fun include_path ->
      with_temp_file ~prefix:"include-user" ~suffix:".rizz" user_program (fun input_path ->
          with_temp_output "include-output" (fun output_path ->
              Rizzoc.compile_from_file ~include_paths:[include_path] input_path output_path;
              Alcotest.(check bool) "include path compiled" true (Sys.file_exists output_path))))

let tests =
  [ "compile_from_file includes signal prelude", `Quick, test_compile_from_file_includes_signal_prelude;
    "compile_from_files respects left-to-right order", `Quick, test_compile_from_files_respects_left_to_right_order;
    "compile_from_files rejects reverse-order dependency", `Quick, test_compile_from_files_rejects_reverse_order_dependencies;
    "compile_from_file rejects prelude collision", `Quick, test_compile_from_file_rejects_prelude_name_collision;
    "compile_from_file accepts overridden stdlib file", `Quick, test_compile_from_file_accepts_overridden_stdlib_file;
    "compile_from_file accepts included file", `Quick, test_compile_from_file_accepts_included_file;
  ]
