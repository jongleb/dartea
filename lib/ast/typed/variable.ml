type 'a state =
  | Unbound of { level : int; constrained : Data.Constraint.t option }
  | Linked of 'a

type 'a t = { identity : int; mutable state : 'a state }

let born = ref 0
let depth = ref 0
let enter_level () = incr depth
let leave_level () = decr depth
let current_level () = !depth

let fresh constrained =
  incr born;
  { identity = !born; state = Unbound { level = !depth; constrained } }

let state variable = variable.state

let constraint_of variable =
  match variable.state with
  | Unbound carried -> carried.constrained
  | Linked _ -> None

let link variable target = variable.state <- Linked target

let constrain variable carried =
  match variable.state with
  | Unbound held -> variable.state <- Unbound { held with constrained = Some carried }
  | Linked _ -> ()

let lower_to ~from variable =
  match (from.state, variable.state) with
  | Unbound source, Unbound held when held.level > source.level ->
      variable.state <- Unbound { held with level = source.level }
  | (Unbound _ | Linked _), (Unbound _ | Linked _) -> ()

let identity variable = variable.identity
let compare _ left right = Stdlib.Int.compare left.identity right.identity
let equal _ left right = Stdlib.Int.equal left.identity right.identity
let hash_fold_t _ state variable = Base.Hash.fold_int state variable.identity

let pp inner formatter variable =
  match variable.state with
  | Unbound { level; constrained = None } ->
      Format.fprintf formatter "'%d@%d" variable.identity level
  | Unbound { level; constrained = Some carried } ->
      Format.fprintf formatter "'%d@%d:%s" variable.identity level
        (Data.Constraint.name carried)
  | Linked target -> inner formatter target
