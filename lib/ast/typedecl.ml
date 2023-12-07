type type_ctor = { id : string; data : Typedef.t list } [@@deriving show]

type t = { name : string; ctors : type_ctor list; params : string list }
[@@deriving show]
