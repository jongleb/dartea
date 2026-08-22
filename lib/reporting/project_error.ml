type t =
  | Unknown_folder of { folder : string }
  | No_sources of { folder : string }
  | Bad_json of { file : string; problem : string }
  | Missing_field of { file : string; field : string }
  | Bad_field of { file : string; field : string; expected : string }
  | Missing_source_directory of { file : string; folder : string }
  | Duplicate_module of { name : string; one : string; other : string }
  | Unknown_entry of { path : string }
  | No_entry of { module_name : string; declaration : string }
  | Entry_not_exposed of {
      delivery : string;
      module_name : string;
      declaration : string;
    }
  | Bad_entry of {
      delivery : string;
      module_name : string;
      declaration : string;
      expected : string;
      found : string;
    }
  | Unknown_delivery of { name : string; known : string list }
  | Delivery_needs_entry of { delivery : string }
[@@deriving show]

let file_of = function
  | Unknown_folder { folder } | No_sources { folder } -> folder
  | Bad_json { file; _ }
  | Missing_field { file; _ }
  | Bad_field { file; _ }
  | Missing_source_directory { file; _ } ->
      file
  | Duplicate_module { one; _ } -> one
  | Unknown_entry { path } -> path
  | No_entry { module_name; _ }
  | Entry_not_exposed { module_name; _ }
  | Bad_entry { module_name; _ } ->
      module_name
  | Unknown_delivery { name; _ } -> name
  | Delivery_needs_entry { delivery } -> delivery
