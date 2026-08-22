type t = { file : string; source_directories : string list }

let file_name = "elm.json"
let default_source_directory = "src"
let an_array_of_strings = "an array of strings"

let bad_field ~file ~field expected =
  Reporting.Error.raise_project (Bad_field { file; field; expected })

let assumed root =
  let src = Files.Relative.of_string default_source_directory in
  let folder =
    if Files.Dir.is_directory root src then default_source_directory
    else Filename.current_dir_name
  in
  { file = Files.Dir.shown root; source_directories = [ folder ] }

let directory_of ~file ~field = function
  | `String text -> text
  | _ -> bad_field ~file ~field an_array_of_strings

let directories ~file rows =
  let field = "source-directories" in
  match List.assoc_opt field rows with
  | None -> Reporting.Error.raise_project (Missing_field { file; field })
  | Some (`List []) -> bad_field ~file ~field "at least one folder"
  | Some (`List items) -> List.map (directory_of ~file ~field) items
  | Some _ -> bad_field ~file ~field an_array_of_strings

let flattened problem =
  String.map (function '\n' | '\r' | '\t' -> ' ' | letter -> letter) problem

let rows_of ~file content =
  match Yojson.Safe.from_string content with
  | `Assoc rows -> rows
  | _ ->
      Reporting.Error.raise_project
        (Bad_json { file; problem = "I was expecting a JSON object here." })
  | exception Yojson.Json_error problem ->
      Reporting.Error.raise_project
        (Bad_json { file; problem = flattened problem })

let decoded ~file content =
  let field = "type" in
  let rows = rows_of ~file content in
  match List.assoc_opt field rows with
  | None -> Reporting.Error.raise_project (Missing_field { file; field })
  | Some (`String "package") ->
      { file; source_directories = [ default_source_directory ] }
  | Some (`String "application") ->
      { file; source_directories = directories ~file rows }
  | Some _ -> bad_field ~file ~field {|"application" or "package"|}

let of_folder root =
  let path = Files.Relative.of_string file_name in
  if Files.Dir.is_file root path then
    decoded
      ~file:(Files.Dir.shown (Files.Dir.at root path))
      (Files.Dir.load root path)
  else assumed root
