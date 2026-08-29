open Packages

type t = {
  file : string;
  source_directories : string list;
  dependencies : Pick.t list;
}

let file_name = "elm.json"
let path = Files.Relative.of_string file_name
let default_source_directory = "src"
let an_array_of_strings = "an array of strings"
let depends = "dependencies"

let assumed root =
  let src = Files.Relative.of_string default_source_directory in
  let folder =
    if Files.Dir.is_directory root src then default_source_directory
    else Filename.current_dir_name
  in
  {
    file = Files.Dir.shown root;
    source_directories = [ folder ];
    dependencies = [];
  }

let directories ~file rows =
  let field = "source-directories" in
  match List.assoc_opt field rows with
  | None -> Json.missing ~file ~field
  | Some (`List []) -> Json.bad ~file ~field "at least one folder"
  | Some (`List items) -> Json.strings ~file ~field an_array_of_strings items
  | Some _ -> Json.bad ~file ~field an_array_of_strings

let picked ~file (package, held) =
  { Pick.package; version = Json.version ~file ~field:depends held }

let grouped ~file (_, held) =
  match held with
  | `Assoc packages -> List.map (picked ~file) packages
  | _ -> Json.bad ~file ~field:depends "direct and indirect"

let depended ~file rows =
  match List.assoc_opt depends rows with
  | None -> []
  | Some (`Assoc parts) -> List.concat_map (grouped ~file) parts
  | Some _ -> Json.bad ~file ~field:depends "an object"

let decoded ~file content =
  let field = "type" in
  let rows = Json.rows_of ~file content in
  match List.assoc_opt field rows with
  | None -> Json.missing ~file ~field
  | Some (`String "package") ->
      {
        file;
        source_directories = [ default_source_directory ];
        dependencies = [];
      }
  | Some (`String "application") ->
      {
        file;
        source_directories = directories ~file rows;
        dependencies = depended ~file rows;
      }
  | Some _ -> Json.bad ~file ~field {|"application" or "package"|}

let of_elm_json root =
  match Json.loaded root path decoded with
  | Some outline -> outline
  | None -> assumed root

let from_lock ~file root =
  match Lock.of_folder root with
  | Some (lock : Lock.t) -> lock.packages
  | None ->
      Reporting.Error.raise_project
        (Missing_lock { file; lock = Lock.file_name })

let locked root (manifest : Manifest.t) =
  match manifest.dependencies with
  | [] -> []
  | _ :: _ -> from_lock ~file:manifest.file root

let of_manifest root (manifest : Manifest.t) =
  {
    file = manifest.file;
    source_directories = manifest.source_directories;
    dependencies = locked root manifest;
  }

let of_folder root =
  match Manifest.of_folder root with
  | Some manifest -> of_manifest root manifest
  | None -> of_elm_json root
