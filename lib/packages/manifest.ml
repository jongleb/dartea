type t = {
  file : string;
  source_directories : string list;
  dependencies : (string * Range.t) list;
}

let file_name = "dartea.json"
let path = Files.Relative.of_string file_name
let field = "dependencies"
let default_source_directory = "src"

let wanted ~file (name, held) = (name, Json.range ~file ~field held)

let depended ~file rows =
  match List.assoc_opt field rows with
  | None -> []
  | Some (`Assoc packages) -> List.map (wanted ~file) packages
  | Some _ -> Json.bad ~file ~field "an object of packages and versions"

let folders ~file rows =
  let field = "source-directories" in
  let expected = "an array of strings" in
  match List.assoc_opt field rows with
  | None -> [ default_source_directory ]
  | Some (`List []) -> Json.bad ~file ~field "at least one folder"
  | Some (`List items) -> Json.strings ~file ~field expected items
  | Some _ -> Json.bad ~file ~field expected

let decoded ~file content =
  let rows = Json.rows_of ~file content in
  {
    file;
    source_directories = folders ~file rows;
    dependencies = depended ~file rows;
  }

let of_folder root = Json.loaded root path decoded
