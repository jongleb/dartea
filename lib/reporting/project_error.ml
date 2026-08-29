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
  | Delivery_needs_entry of { delivery : string }
  | Missing_lock of { file : string; lock : string }
  | Missing_manifest of { file : string }
  | Offline of { file : string; url : string; problem : string }
  | Unknown_package of { file : string; package : string; asked_by : string }
  | No_version of {
      file : string;
      package : string;
      asked : (string * string) list;
    }
  | Bad_range of {
      file : string;
      package : string;
      version : string;
      dependency : string;
      range : string;
    }
  | Bad_tarball of {
      file : string;
      package : string;
      version : string;
      problem : string;
    }
  | Missing_package of {
      file : string;
      package : string;
      version : string;
      looked : string;
    }
  | Bad_flags of {
      delivery : string;
      module_name : string;
      declaration : string;
      found : string;
    }
[@@deriving show]

let file_of = function
  | Unknown_folder { folder } | No_sources { folder } -> folder
  | Bad_json { file; _ }
  | Missing_field { file; _ }
  | Bad_field { file; _ }
  | Missing_source_directory { file; _ }
  | Missing_lock { file; _ }
  | Missing_manifest { file }
  | Offline { file; _ }
  | Unknown_package { file; _ }
  | No_version { file; _ }
  | Bad_range { file; _ }
  | Bad_tarball { file; _ }
  | Missing_package { file; _ } ->
      file
  | Duplicate_module { one; _ } -> one
  | Unknown_entry { path } -> path
  | No_entry { module_name; _ }
  | Entry_not_exposed { module_name; _ }
  | Bad_entry { module_name; _ }
  | Bad_flags { module_name; _ } ->
      module_name
  | Delivery_needs_entry { delivery } -> delivery
