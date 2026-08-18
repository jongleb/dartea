type t

val fail : ('a, unit, string, 'b) format4 -> 'a
val naming : unit -> t
val within : t -> Typed.Type.t -> string
val of_type : Typed.Type.t -> string
