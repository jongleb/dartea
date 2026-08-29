let a_version = "a version like `1.2.0`"
let a_range = "a version like `1.2.0` or a range like `^1.2.0`"

let bad ~file ~field expected =
  Reporting.Error.raise_project (Bad_field { file; field; expected })

let missing ~file ~field =
  Reporting.Error.raise_project (Missing_field { file; field })

let flattened problem =
  String.map (function '\n' | '\r' | '\t' -> ' ' | letter -> letter) problem

let rows_of ~file content =
  match Yojson.Safe.from_string content with
  | `Assoc rows -> rows
  | _ ->
      Reporting.Error.raise_project
        (Bad_json { file; problem = "I was expecting a JSON object here." })
  | exception Yojson.Json_error problem ->
      Reporting.Error.raise_project
        (Bad_json { file; problem = flattened problem })

let text_of ~file ~field expected = function
  | `String written -> written
  | _ -> bad ~file ~field expected

let strings ~file ~field expected items =
  List.map (text_of ~file ~field expected) items

let version ~file ~field held =
  match Version.of_string (text_of ~file ~field a_version held) with
  | Some version -> version
  | None -> bad ~file ~field a_version

let range ~file ~field held =
  match Range.of_string (text_of ~file ~field a_range held) with
  | Some range -> range
  | None -> bad ~file ~field a_range

let loaded root path decoded =
  if Files.Dir.is_file root path then
    Some (decoded ~file:(Files.Dir.at root path) (Files.Dir.load root path))
  else None
