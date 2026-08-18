type prefix = No_prefix | Unknown_prefix of string | Known_prefix of string
[@@deriving show]

type t =
  | Unknown_module of { qualifier : string; near : string list }
  | Not_exposed of { module_name : string; name : string; near : string list }
  | Ctors_not_exposed of { module_name : string; type_name : string }
  | Ambiguous of { name : string; modules : string list }
  | Unknown_kernel of { module_name : string; exported_name : string }
  | Kernel_needs_annotation of { name : string }
  | Kernel_arity_mismatch of { declared : int; kernel : int }
  | Duplicate_declaration of { name : string }
  | Duplicate_binder of { name : string }
  | Unbound_value of {
      name : Data.Name.t;
      prefix : prefix;
      near : string list;
    }
  | Unknown_constructor of {
      name : Data.Name.t;
      prefix : prefix;
      near : string list;
    }
  | Unknown_type of { name : Data.Name.t; prefix : prefix; near : string list }
  | Import_cycle of { modules : string list }
  | Recursive_value of { names : string list }
[@@deriving show]
