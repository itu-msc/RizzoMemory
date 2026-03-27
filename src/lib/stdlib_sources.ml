type source_text = {
  filename: string;
  text: string;
}

let manifest_filename = "manifest.txt"

let installed_stdlib_relative_parts = [".."; "lib"; "rizzoc"; "stdlib"]
let dev_stdlib_relative_parts = [".."; ".."; ".."; ".."; "src"; "stdlib"]

let path_exists path =
    Sys.file_exists path

let is_directory path =
    try Sys.is_directory path with Sys_error _ -> false

let join_path base parts =
    List.fold_left Filename.concat base parts

let realpath path =
    try Unix.realpath path with Unix.Unix_error (_, _, _) -> path

let executable_path () =
    realpath Sys.executable_name

let canonical_path path =
    if path_exists path then
        realpath path
    else
        path

let dedupe_preserving_order paths =
    let seen = Hashtbl.create 8 in
    List.filter
        (fun path ->
            if Hashtbl.mem seen path then
                false
            else (
                Hashtbl.add seen path ();
                true))
        paths

let candidate_stdlib_roots ?executable_path:maybe_executable_path () =
    let resolved_executable_path = Option.value maybe_executable_path ~default:(executable_path ()) in
    let executable_dir = Filename.dirname resolved_executable_path in
    [ join_path executable_dir dev_stdlib_relative_parts;
        join_path executable_dir installed_stdlib_relative_parts;
    ]
    |> List.map realpath
    |> dedupe_preserving_order

let resolve_default_stdlib_root ?executable_path () =
    match List.find_opt is_directory (candidate_stdlib_roots ?executable_path ()) with
    | Some root -> root
    | None ->
            let tried = String.concat ", " (candidate_stdlib_roots ?executable_path ()) in
            failwith (Printf.sprintf "Could not locate stdlib directory. Looked in: %s" tried)

let ends_with ~(suffix : string) (text : string) =
    let text_len = String.length text in
    let suffix_len = String.length suffix in
    text_len >= suffix_len && String.sub text (text_len - suffix_len) suffix_len = suffix

let read_all_text path =
    let ic = open_in_bin path in
    Fun.protect
        ~finally:(fun () -> close_in ic)
        (fun () ->
            let len = in_channel_length ic in
            really_input_string ic len)

let read_manifest_entries manifest_path =
    let ic = open_in manifest_path in
    Fun.protect
        ~finally:(fun () -> close_in ic)
        (fun () ->
            let rec loop acc =
                match input_line ic with
                | line ->
                        let entry = String.trim line in
                        if entry = "" || String.length entry > 0 && entry.[0] = '#'
                        then loop acc
                        else loop (entry :: acc)
                | exception End_of_file -> List.rev acc
            in
            loop [])

let list_directory_rizz_files root =
    Sys.readdir root
    |> Array.to_list
    |> List.filter (fun name -> ends_with ~suffix:".rizz" name)
    |> List.sort String.compare

let resolve_stdlib_file_paths root =
    let manifest_path = Filename.concat root manifest_filename in
    let entries =
        if path_exists manifest_path then
            read_manifest_entries manifest_path
        else
            list_directory_rizz_files root
    in
    List.map
        (fun entry ->
            let path =
                if Filename.is_relative entry then Filename.concat root entry else entry
            in
            if path_exists path then
                realpath path
            else
                failwith (Printf.sprintf "Stdlib entry not found: %s" path))
        entries

let resolve_source_file_paths path =
    let canonical = canonical_path path in
    if is_directory canonical then
        resolve_stdlib_file_paths canonical
    else if path_exists canonical then
        [canonical]
    else
        failwith (Printf.sprintf "Path not found: %s" path)

let filter_excluded_paths excluded_paths paths =
    let excluded = Hashtbl.create 8 in
    List.iter (fun path -> Hashtbl.replace excluded (canonical_path path) ()) excluded_paths;
    List.filter (fun path -> not (Hashtbl.mem excluded (canonical_path path))) paths

let source_text_of_file path =
    { filename = path; text = read_all_text path }

let source_texts_of_path path =
    resolve_source_file_paths path
    |> List.map source_text_of_file

let default_preludes ?executable_path ?stdlib_path ?(exclude_paths = []) () =
    let source_path =
        match stdlib_path with
        | Some path -> path
        | None -> resolve_default_stdlib_root ?executable_path ()
    in
    resolve_source_file_paths source_path
    |> filter_excluded_paths exclude_paths
    |> List.map source_text_of_file
