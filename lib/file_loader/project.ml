type t = { file : string; source_directories : string list }

let file_name = "elm.json"
let default_source_directory = "src"
let an_array_of_strings = "an array of strings"
let refuse problem = Reporting.Error.raise_project problem

let assumed root =
  let folder =
    if Paths.is_directory Eio.Path.(root / default_source_directory) then
      default_source_directory
    else Paths.here
  in
  {
    file = Paths.displayed ~default:Paths.here root;
    source_directories = [ folder ];
  }

let directory_of ~file ~field = function
  | `String text -> text
  | _ -> refuse (Bad_field { file; field; expected = an_array_of_strings })

let source_directories ~file rows =
  let field = "source-directories" in
  match List.assoc_opt field rows with
  | None -> refuse (Missing_field { file; field })
  | Some (`List []) ->
      refuse (Bad_field { file; field; expected = "at least one folder" })
  | Some (`List items) -> List.map (directory_of ~file ~field) items
  | Some _ -> refuse (Bad_field { file; field; expected = an_array_of_strings })

let flattened problem =
  String.map (function '\n' | '\r' | '\t' -> ' ' | letter -> letter) problem

let rows_of ~file content =
  match Yojson.Safe.from_string content with
  | `Assoc rows -> rows
  | _ ->
      refuse (Bad_json { file; problem = "I was expecting a JSON object here." })
  | exception Yojson.Json_error problem ->
      refuse (Bad_json { file; problem = flattened problem })

let decoded ~file content =
  let field = "type" in
  let rows = rows_of ~file content in
  match List.assoc_opt field rows with
  | None -> refuse (Missing_field { file; field })
  | Some (`String "package") ->
      { file; source_directories = [ default_source_directory ] }
  | Some (`String "application") ->
      { file; source_directories = source_directories ~file rows }
  | Some _ ->
      refuse
        (Bad_field { file; field; expected = {|"application" or "package"|} })

let of_folder root =
  let path = Eio.Path.(root / file_name) in
  if Paths.is_file path then
    decoded ~file:(Paths.displayed ~default:file_name path) (Eio.Path.load path)
  else assumed root
