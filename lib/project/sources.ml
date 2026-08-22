let elm_files ~root ~directory =
  let folder = Files.Dir.at root directory in
  Files.Dir.under ~suffix:Elm_file.extension folder
  |> List.map (fun path ->
         Elm_file.under ~directory ~path (Files.Dir.load folder path))

let inside root ~file written =
  let directory = Files.Relative.of_string written in
  if Files.Dir.is_directory root directory then elm_files ~root ~directory
  else
    Reporting.Error.raise_project
      (Missing_source_directory { file; folder = written })

let by_name (one : Elm_file.t) (other : Elm_file.t) =
  match String.compare one.name other.name with
  | 0 -> String.compare one.path other.path
  | ordering -> ordering

let one_per_name sources =
  let rec scanned = function
    | (one : Elm_file.t) :: (other :: _ as rest) ->
        if String.equal one.name other.name then
          Reporting.Error.raise_project
            (Duplicate_module
               { name = one.name; one = one.path; other = other.path })
        else scanned rest
    | [ _ ] | [] -> ()
  in
  scanned (List.sort by_name sources)

let gathered root =
  if not (Files.Dir.is_directory root Files.Relative.root) then
    Reporting.Error.raise_project
      (Unknown_folder { folder = Files.Dir.shown root });
  let outline = Outline.of_folder root in
  match
    List.concat_map (inside root ~file:outline.file) outline.source_directories
  with
  | [] ->
      Reporting.Error.raise_project
        (No_sources { folder = Files.Dir.shown root })
  | sources ->
      one_per_name sources;
      sources

let load (root : _ Eio.Path.t) =
  match gathered root with
  | sources -> Ok sources
  | exception Reporting.Error.Found error -> Error error
