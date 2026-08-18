type 'a t

type 'a state =
  | Unbound of { level : int; constrained : Data.Constraint.t option }
  | Linked of 'a

val fresh : Data.Constraint.t option -> 'a t
val state : 'a t -> 'a state
val constraint_of : 'a t -> Data.Constraint.t option
val link : 'a t -> 'a -> unit
val constrain : 'a t -> Data.Constraint.t -> unit
val lower_to : from:'a t -> 'a t -> unit
val enter_level : unit -> unit
val leave_level : unit -> unit
val current_level : unit -> int
val identity : 'a t -> int
val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val hash_fold_t :
  (Base.Hash.state -> 'a -> Base.Hash.state) ->
  Base.Hash.state ->
  'a t ->
  Base.Hash.state

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
