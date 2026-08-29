type t = {
  file : string;
  source_directories : string list;
  dependencies : (string * Version.Range.t) list;
}

let file_name = "dartea.json"
let path = Fpath.v file_name
let folders = "source-directories"
let field = "dependencies"
let default_source_directory = "src"
let an_array_of_strings = "an array of strings"

let source_directories =
  Json.nonempty "at least one folder"
    (Json.items an_array_of_strings (Json.text an_array_of_strings))

let dependencies = Json.pairs "an object of packages and versions" Json.range

let decoded ~file content =
  let rows = Json.rows_of ~file content in
  {
    file;
    source_directories =
      Json.optional ~file rows folders
        ~default:[ default_source_directory ]
        source_directories;
    dependencies = Json.optional ~file rows field ~default:[] dependencies;
  }

let of_folder root = Json.loaded root path decoded
