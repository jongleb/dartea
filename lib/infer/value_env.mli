type t

val empty : t
val primitives : t
val find : Data.Name.t -> t -> Typed.Type.scheme option
val bind : Data.Name.t -> Typed.Type.scheme -> t -> t
val bind_one : string -> Typed.Type.t -> t -> t
val shadow : by:t -> t -> t
val binders_of_both : region:Data.Region.t -> t -> t -> t
val zonk : t -> t
val names : t -> Data.Name.t list
val schemes : t -> Typed.Type.scheme list
val equal : (Typed.Type.scheme -> Typed.Type.scheme -> bool) -> t -> t -> bool
