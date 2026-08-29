type t = { major : int; minor : int; patch : int }

let parts version = [ version.major; version.minor; version.patch ]
let show version = String.concat "." (List.map string_of_int (parts version))
let compare one other = List.compare Int.compare (parts one) (parts other)
let next_major version = { major = version.major + 1; minor = 0; patch = 0 }
let next_patch version = { version with patch = version.patch + 1 }

let digits written =
  String.length written > 0
  && String.for_all (function '0' .. '9' -> true | _ -> false) written

let number written =
  if digits written then int_of_string_opt written else None

let of_string written =
  match List.map number (String.split_on_char '.' written) with
  | [ Some major; Some minor; Some patch ] -> Some { major; minor; patch }
  | _ -> None

let later one other = if compare one other >= 0 then one else other
let earlier one other = if compare one other <= 0 then one else other

module Range = struct
  type nonrec t = Exactly of t | Upward of t

  let upward = "^"

  let show = function
    | Exactly version -> show version
    | Upward version -> upward ^ show version

  let of_string written =
    match Data.Text.after_prefix ~prefix:upward written with
    | Some after -> Option.map (fun version -> Upward version) (of_string after)
    | None -> Option.map (fun version -> Exactly version) (of_string written)
end

module Interval = struct
  type nonrec t = { least : t; below : t }

  let upto least below = { least; below }

  let of_range = function
    | Range.Exactly version -> upto version (next_patch version)
    | Range.Upward version -> upto version (next_major version)

  let holds bounds version =
    compare version bounds.least >= 0 && compare version bounds.below < 0

  let holding bounds =
    if compare bounds.least bounds.below < 0 then Some bounds else None

  let meet one other =
    holding
      (upto (later one.least other.least) (earlier one.below other.below))
end
