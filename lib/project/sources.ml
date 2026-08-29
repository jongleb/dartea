open Packages

let elm_files ~origin ~root ~directory =
  let folder = Files.Dir.into root directory in
  Files.Dir.under ~suffix:Elm_file.extension folder
  |> List.map (fun path ->
         Elm_file.under ~origin ~directory ~path (Files.Dir.load folder path))

let inside root ~file written =
  let directory = Files.Relative.of_string written in
  if Files.Dir.is_directory root directory then
    elm_files ~origin:Elm_file.Written ~root ~directory
  else
    Diagnostic.Failure.raise_project
      (Missing_source_directory { file; folder = written })

let package root ~file ~provided (pick : Pick.t) =
  if List.mem pick.package provided then []
  else
    let directory = Vendor.inside pick in
    if Files.Dir.is_directory root directory then
      elm_files ~origin:Elm_file.Package ~root ~directory
    else
      Diagnostic.Failure.raise_project
        (Missing_package
           {
             file;
             package = pick.package;
             version = Version.show pick.version;
             looked = Files.Relative.shown directory;
           })

let by_name (one : Elm_file.t) (other : Elm_file.t) =
  match String.compare one.name other.name with
  | 0 -> String.compare one.path other.path
  | ordering -> ordering

let one_per_name sources =
  let rec scanned = function
    | (one : Elm_file.t) :: (other :: _ as rest) ->
        if String.equal one.name other.name then
          Diagnostic.Failure.raise_project
            (Duplicate_module
               { name = one.name; one = one.path; other = other.path })
        else scanned rest
    | [ _ ] | [] -> ()
  in
  scanned (List.sort by_name sources)

type t = Elm_file.t list

let files sources = sources

let checked sources =
  one_per_name sources;
  sources

let gathered ~provided root =
  if not (Files.Dir.is_directory root Files.Relative.root) then
    Diagnostic.Failure.raise_project
      (Unknown_folder { folder = Files.Dir.shown root });
  let outline = Outline.of_folder root in
  let file = outline.file in
  let packages =
    List.concat_map (package root ~file ~provided) outline.dependencies
  in
  let own = List.concat_map (inside root ~file) outline.source_directories in
  match packages @ own with
  | [] ->
      Diagnostic.Failure.raise_project
        (No_sources { folder = Files.Dir.shown root })
  | sources -> checked sources

let of_list = checked

let load ~provided root =
  match gathered ~provided root with
  | sources -> Ok sources
  | exception Diagnostic.Failure.Found failure -> Error failure
