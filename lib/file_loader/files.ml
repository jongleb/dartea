module Elm_file = struct
  type t = { path : string; name : string; content : string }

  let dotted path =
    Filename.remove_extension path
    |> String.split_on_char '/' |> String.concat "."

  let of_path ~path content = { path; name = dotted path; content }
end

let extension = ".elm"

let rec elm_files folder prefix =
  Eio.Path.read_dir (Paths.at folder prefix)
  |> List.sort String.compare
  |> List.concat_map (fun entry ->
         let path = Paths.joined prefix entry in
         if Paths.hidden entry then []
         else if Paths.is_directory (Paths.at folder path) then
           elm_files folder path
         else if String.ends_with ~suffix:extension entry then [ path ]
         else [])

let loaded ~folder ~directory path =
  {
    Elm_file.path = Paths.joined directory path;
    name = Elm_file.dotted path;
    content = Eio.Path.load (Paths.at folder path);
  }

let inside root ~file directory =
  let folder = Paths.at root directory in
  if Paths.is_directory folder then
    List.map (loaded ~folder ~directory) (elm_files folder Paths.here)
  else
    Reporting.Error.raise_project
      (Missing_source_directory { file; folder = directory })

let by_name (one : Elm_file.t) (other : Elm_file.t) =
  match String.compare one.name other.name with
  | 0 -> String.compare one.path other.path
  | ordering -> ordering

let one_per_name sources =
  let rec scanned = function
    | (one : Elm_file.t) :: ((other : Elm_file.t) :: _ as rest) ->
        if String.equal one.name other.name then
          Reporting.Error.raise_project
            (Duplicate_module
               { name = one.name; one = one.path; other = other.path })
        else scanned rest
    | [ _ ] | [] -> ()
  in
  scanned (List.sort by_name sources)

let gathered root =
  let project = Project.of_folder root in
  match
    List.concat_map (inside root ~file:project.file) project.source_directories
  with
  | [] ->
      Reporting.Error.raise_project
        (No_sources { folder = Paths.displayed ~default:Paths.here root })
  | sources ->
      one_per_name sources;
      sources

let load (root_dir : _ Eio.Path.t) =
  match gathered root_dir with
  | sources -> Ok sources
  | exception Reporting.Error.Found error -> Error error
