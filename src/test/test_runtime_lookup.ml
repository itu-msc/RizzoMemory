let remove_file_if_exists path =
  if Sys.file_exists path && not (Sys.is_directory path) then
    Sys.remove path

let remove_dir_if_exists path =
  if Sys.file_exists path && Sys.is_directory path then
    Unix.rmdir path

let rec remove_tree_if_exists path =
  if Sys.file_exists path then
    if Sys.is_directory path then (
      Sys.readdir path
      |> Array.iter (fun name -> remove_tree_if_exists (Filename.concat path name));
      Unix.rmdir path
    ) else
      Sys.remove path

let with_temp_dir prefix f =
  let path = Filename.temp_file prefix ".dir" in
  remove_file_if_exists path;
  Unix.mkdir path 0o755;
  Fun.protect
    ~finally:(fun () -> remove_tree_if_exists path)
    (fun () -> f path)

let mkdir path =
  if not (Sys.file_exists path) then
    Unix.mkdir path 0o755

let test_resolve_default_runtime_root_prefers_installed_layout () =
  with_temp_dir "runtime-root" (fun install_root ->
      let bin_dir = Filename.concat install_root "bin" in
      let lib_dir = Filename.concat install_root "lib" in
      let package_dir = Filename.concat lib_dir "rizzoc" in
      let runtime_dir = Filename.concat package_dir "runtime" in
      mkdir bin_dir;
      mkdir lib_dir;
      mkdir package_dir;
      mkdir runtime_dir;
      let executable_path = Filename.concat bin_dir "rizzoc" in
      let resolved = Rizzoc.resolve_default_runtime_root ~executable_path () in
      Alcotest.(check string)
        "installed runtime root"
        (Unix.realpath runtime_dir)
        resolved)

let tests =
  [ "resolve_default_runtime_root finds installed runtime layout", `Quick,
    test_resolve_default_runtime_root_prefers_installed_layout;
  ]