open Packages

type t = {
  file : string;
  source_directories : string list;
  dependencies : Pick.t list;
}

let file_name = "elm.json"
let path = Fpath.v file_name
let kind = "type"
let depends = "dependencies"
let a_kind = {|"application" or "package"|}

let dependencies =
  Json.pairs "an object" (Json.pairs "direct and indirect" Json.version)

let assumed root =
  let folder =
    if Files.is_directory root (Fpath.v Manifest.default_source_directory) then
      Manifest.default_source_directory
    else Filename.current_dir_name
  in
  { file = Files.shown root; source_directories = [ folder ]; dependencies = [] }

let application ~file rows =
  let grouped = Json.optional ~file rows depends ~default:[] dependencies in
  {
    file;
    source_directories =
      Json.required ~file rows Manifest.folders Manifest.source_directories;
    dependencies =
      List.concat_map (fun (_, picked) -> List.map Pick.of_pair picked) grouped;
  }

let decoded ~file content =
  let rows = Json.rows_of ~file content in
  match Json.required ~file rows kind (Json.text a_kind) with
  | "package" ->
      {
        file;
        source_directories = [ Manifest.default_source_directory ];
        dependencies = [];
      }
  | "application" -> application ~file rows
  | _ -> Json.bad ~file ~field:kind a_kind

let of_elm_json root =
  match Json.loaded root path decoded with
  | Some outline -> outline
  | None -> assumed root

let from_lock ~file root =
  match Lock.of_folder root with
  | Some (lock : Lock.t) -> lock.packages
  | None ->
      Diagnostic.Failure.raise_project
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
