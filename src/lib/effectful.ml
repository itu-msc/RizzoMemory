module StringSet = Set.Make(String)

let default_effectful_names =
  Rizzo_builtins.builtins
  |> List.filter_map (fun ({ name; _ } : Rizzo_builtins.builtin_info) ->
         if String.starts_with ~prefix:"output_" name then Some name else None)
  |> StringSet.of_list

let custom_effectful_names = ref StringSet.empty

let reset_custom () =
  custom_effectful_names := StringSet.empty

let mark_effectful (name : string) =
  custom_effectful_names := StringSet.add name !custom_effectful_names

let is_effectful (name : string) =
  StringSet.mem name default_effectful_names || StringSet.mem name !custom_effectful_names
