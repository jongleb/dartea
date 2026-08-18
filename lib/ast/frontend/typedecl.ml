type type_ctor = { id : string Data.Located.t; data : Typedef.Impl.t list }
[@@deriving show]

type t = {
  name : string Data.Located.t;
  ctors : type_ctor list;
  params : string list;
}
[@@deriving show]
