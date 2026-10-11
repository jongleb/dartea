type listing = {
  name : string;
  version : Version.t;
  summary : string;
  license : string;
  exposed_modules : string Non_empty_list.t;
      [@key "exposed-modules"] [@of_yojson Json.nonempty "module names"]
}
[@@deriving of_yojson { strict = false }]

type kind =
  | Application of { source_directories : string Non_empty_list.t }
  | Package of listing

type t = {
  file : string;
  kind : kind;
  dependencies : (string * Version.Range.t) list;
}

let file_name = "dartea.json"
let path = Fpath.v file_name
let default_source_directory = "src"

type header = {
  type_ : string; [@key "type"] [@default "application"]
  dependencies : (string * Version.Range.t) list;
      [@default []] [@of_yojson Json.pairs Version.Range.of_yojson]
}
[@@deriving of_yojson { strict = false }]

type application = {
  source_directories : string Non_empty_list.t;
      [@key "source-directories"]
      [@default Non_empty_list.(default_source_directory :: [])]
      [@of_yojson Json.nonempty "folder names"]
}
[@@deriving of_yojson { strict = false }]

let decode ~file content =
  let header = Json.decode ~file header_of_yojson content in
  let kind =
    match header.type_ with
    | "application" ->
        Application
          {
            source_directories =
              (Json.decode ~file application_of_yojson content)
                .source_directories;
          }
    | "package" -> Package (Json.decode ~file listing_of_yojson content)
    | _ ->
        Diagnostic.Failure.raise_project
          (Bad_value
             { file; expected = {|"application" or "package" as the type|} })
  in
  { file; kind; dependencies = header.dependencies }

let source_directories_of (manifest : t) =
  match manifest.kind with
  | Application { source_directories } ->
      Non_empty_list.to_list source_directories
  | Package _ -> [ default_source_directory ]

let of_folder root = Json.load root path decode

let save_dependencies root dependencies =
  let written = Json.object_of Version.Range.to_yojson dependencies in
  let rows =
    match Yojson.Safe.from_string (Files.load root path) with
    | `Assoc rows -> rows
    | _ -> []
  in
  let rows =
    if List.mem_assoc "dependencies" rows then
      List.map
        (fun (key, value) ->
          if String.equal key "dependencies" then (key, written) else (key, value))
        rows
    else rows @ [ ("dependencies", written) ]
  in
  Files.save root path (Yojson.Safe.pretty_to_string (`Assoc rows) ^ "\n")
