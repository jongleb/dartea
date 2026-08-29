type rows = (string * Yojson.Safe.t) list
type 'a decoder = file:string -> field:string -> Yojson.Safe.t -> 'a

let a_version = "a version like `1.2.0`"
let a_range = "a version like `1.2.0` or a range like `^1.2.0`"

let bad ~file ~field expected =
  Diagnostic.Failure.raise_project (Bad_field { file; field; expected })

let missing ~file ~field =
  Diagnostic.Failure.raise_project (Missing_field { file; field })

let flattened problem =
  String.map (function '\n' | '\r' | '\t' -> ' ' | letter -> letter) problem

let rows_of ~file content : rows =
  match Yojson.Safe.from_string content with
  | `Assoc rows -> rows
  | _ ->
      Diagnostic.Failure.raise_project
        (Bad_json { file; problem = "I was expecting a JSON object here." })
  | exception Yojson.Json_error problem ->
      Diagnostic.Failure.raise_project
        (Bad_json { file; problem = flattened problem })

let text expected : string decoder =
 fun ~file ~field -> function
  | `String written -> written
  | _ -> bad ~file ~field expected

let parsed expected of_string : 'a decoder =
 fun ~file ~field held ->
  match of_string (text expected ~file ~field held) with
  | Some value -> value
  | None -> bad ~file ~field expected

let version : Version.t decoder = parsed a_version Version.of_string
let range : Version.Range.t decoder = parsed a_range Version.Range.of_string

let items expected (item : 'a decoder) : 'a list decoder =
 fun ~file ~field -> function
  | `List held -> List.map (item ~file ~field) held
  | _ -> bad ~file ~field expected

let pairs expected (item : 'a decoder) : (string * 'a) list decoder =
 fun ~file ~field -> function
  | `Assoc held ->
      List.map (fun (name, value) -> (name, item ~file ~field value)) held
  | _ -> bad ~file ~field expected

let nonempty expected (listed : 'a list decoder) : 'a list decoder =
 fun ~file ~field held ->
  match listed ~file ~field held with
  | [] -> bad ~file ~field expected
  | found -> found

let required ~file (rows : rows) field (decoder : 'a decoder) =
  match List.assoc_opt field rows with
  | Some held -> decoder ~file ~field held
  | None -> missing ~file ~field

let optional ~file (rows : rows) field ~default (decoder : 'a decoder) =
  match List.assoc_opt field rows with
  | Some held -> decoder ~file ~field held
  | None -> default

let loaded root path decoded =
  if Files.is_file root path then
    Some (decoded ~file:(Files.at root path) (Files.load root path))
  else None
