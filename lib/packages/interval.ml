type t = { least : Version.t; below : Version.t }

let upto least below = { least; below }

let of_range = function
  | Range.Exactly version -> upto version (Version.next_patch version)
  | Range.Upward version -> upto version (Version.next_major version)

let holds bounds version =
  Version.compare version bounds.least >= 0
  && Version.compare version bounds.below < 0

let later one other = if Version.compare one other >= 0 then one else other
let earlier one other = if Version.compare one other <= 0 then one else other

let holding bounds =
  if Version.compare bounds.least bounds.below < 0 then Some bounds else None

let meet one other =
  holding (upto (later one.least other.least) (earlier one.below other.below))
