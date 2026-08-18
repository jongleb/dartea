type t

val naming : unit -> t
val alone : t -> Typed.Type.t -> Doc.t
val within : t -> Typed.Type.t -> string
val of_type : Typed.Type.t -> string

val comparison :
  t ->
  found:Typed.Type.t ->
  expected:Typed.Type.t ->
  Doc.t * Doc.t * Hint.t list
