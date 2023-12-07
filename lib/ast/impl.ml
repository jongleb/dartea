type t =
  | Type_alias of Typealias.t
  | Type_dec of Typedecl.t
  | Top_declaration of Declaration.t  (** fixme: rename it *)
[@@deriving show]
