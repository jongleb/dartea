module By_file = Map.Make (String)

type t = Source.t By_file.t

let of_list files =
  List.fold_left
    (fun collected (file, content) ->
      By_file.add file (Source.of_string ~file content) collected)
    By_file.empty files

let empty = By_file.empty

let source_of sources (region : Data.Region.t) =
  match By_file.find_opt region.file sources with
  | Some source -> source
  | None -> Source.of_string ~file:region.file ""

let report sources (error : Error.t) =
  Report.of_error (source_of sources error.region) error

let warning sources (found : Warning.t) =
  Report.of_warning (source_of sources found.region) found
