open Packages

type t = {
  file : string;
  source_directories : string list;
  dependencies : Pick.t list;
}

let file_name = "elm.json"
let path = Fpath.v file_name
type header = { type_ : string [@key "type"] } [@@deriving of_yojson { strict = false }]

type application = {
  source_directories : string Non_empty_list.t;
      [@key "source-directories"] [@of_yojson Json.nonempty "folder names"]
  dependencies : (string * (string * Version.t) list) list;
      [@default []] [@of_yojson Json.pairs (Json.pairs Version.of_yojson)]
}
[@@deriving of_yojson { strict = false }]

let default root =
  let folder =
    if Files.is_directory root (Fpath.v Manifest.default_source_directory) then
      Manifest.default_source_directory
    else Filename.current_dir_name
  in
  { file = Files.shown root; source_directories = [ folder ]; dependencies = [] }

let decode ~file content =
  match (Json.decode ~file header_of_yojson content).type_ with
  | "package" ->
      {
        file;
        source_directories = [ Manifest.default_source_directory ];
        dependencies = [];
      }
  | "application" ->
      let read = Json.decode ~file application_of_yojson content in
      {
        file;
        source_directories = Non_empty_list.to_list read.source_directories;
        dependencies =
          List.concat_map
            (fun (_, picked) -> List.map Pick.of_pair picked)
            read.dependencies;
      }
  | _ ->
      Diagnostic.Failure.raise_project
        (Bad_value
           { file; expected = {|"application" or "package" as the type|} })

let of_elm_json root =
  match Json.load root path decode with
  | Some outline -> outline
  | None -> default root

let from_lock ~file root =
  match Lock.of_folder root with
  | Some (lock : Lock.t) -> lock.packages
  | None ->
      Diagnostic.Failure.raise_project
        (Missing_lock { file; lock = Lock.file_name })

let picks_of_lock root (manifest : Manifest.t) =
  match manifest.dependencies with
  | [] -> []
  | _ :: _ -> from_lock ~file:manifest.file root

let of_manifest root (manifest : Manifest.t) =
  {
    file = manifest.file;
    source_directories = Manifest.source_directories_of manifest;
    dependencies = picks_of_lock root manifest;
  }

let of_folder root =
  match Manifest.of_folder root with
  | Some manifest -> of_manifest root manifest
  | None -> of_elm_json root
