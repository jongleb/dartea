type t =
  | Type_alias of Typealias.t
  | Type_dec of Typedecl.t
  | Top_declaration of Declaration.t  (** fixme: rename it *)
  | Import of Import_thing.t
  | ModuleName of string Data.Located.t
  | Export of Exposing.t
  | Port of Port_thing.t
[@@deriving show]
