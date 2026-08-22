type t =
  | No_sources of { folder : string }
  | Bad_json of { file : string; problem : string }
  | Missing_field of { file : string; field : string }
  | Bad_field of { file : string; field : string; expected : string }
  | Missing_source_directory of { file : string; folder : string }
  | Duplicate_module of { name : string; one : string; other : string }
  | Unknown_entry of { path : string }
  | No_entry of { module_name : string; declaration : string }
[@@deriving show]

let file_of = function
  | No_sources { folder } -> folder
  | Bad_json { file; _ }
  | Missing_field { file; _ }
  | Bad_field { file; _ }
  | Missing_source_directory { file; _ } ->
      file
  | Duplicate_module { one; _ } -> one
  | Unknown_entry { path } -> path
  | No_entry { module_name; _ } -> module_name
