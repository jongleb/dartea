open Packages

let elm_files ~origin ~root ~directory =
  let folder = Fpath.append root directory in
  Files.under ~suffix:Elm_file.extension folder
  |> List.map (fun path ->
         Elm_file.under ~origin ~directory ~path (Files.load folder path))

let inside root ~file written =
  let directory = Fpath.v written in
  if Files.is_directory root directory then
    elm_files ~origin:Elm_file.Written ~root ~directory
  else
    Diagnostic.Failure.raise_project
      (Missing_source_directory { file; folder = written })

let package root ~file ~provided (pick : Pick.t) =
  if List.mem pick.package provided then []
  else
    let directory = Pick.sources pick in
    if Files.is_directory root directory then
      elm_files ~origin:Elm_file.Package ~root ~directory
    else
      Diagnostic.Failure.raise_project
        (Missing_package
           {
             file;
             package = pick.package;
             version = Version.show pick.version;
             looked = Fpath.to_string directory;
           })

let by_name (one : Elm_file.t) (other : Elm_file.t) =
  match String.compare one.name other.name with
  | 0 -> String.compare one.path other.path
  | ordering -> ordering

let one_per_name sources =
  let rec scan = function
    | (one : Elm_file.t) :: (other :: _ as rest) ->
        if String.equal one.name other.name then
          Diagnostic.Failure.raise_project
            (Duplicate_module
               { name = one.name; one = one.path; other = other.path })
        else scan rest
    | [ _ ] | [] -> ()
  in
  scan (List.sort by_name sources)

type t = Elm_file.t list

let files sources = sources

let check sources =
  one_per_name sources;
  sources

let gather ~provided root =
  if not (Files.is_directory root (Fpath.v Filename.current_dir_name)) then
    Diagnostic.Failure.raise_project
      (Unknown_folder { folder = Files.shown root });
  let outline = Outline.of_folder root in
  let file = outline.file in
  let packages =
    List.concat_map (package root ~file ~provided) outline.dependencies
  in
  let own = List.concat_map (inside root ~file) outline.source_directories in
  match packages @ own with
  | [] ->
      Diagnostic.Failure.raise_project
        (No_sources { folder = Files.shown root })
  | sources -> check sources

let of_list = check

let load ~provided root =
  match gather ~provided root with
  | sources -> Ok sources
  | exception Diagnostic.Failure.Found failure -> Error failure
