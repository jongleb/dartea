type problem =
  | Missing_patterns of { unhandled : Typed.Pattern.t list }
  | Redundant_pattern of { index : int }
[@@deriving show]

type t = { region : Data.Region.t; problem : problem } [@@deriving show]

let missing_patterns ~region unhandled =
  { region; problem = Missing_patterns { unhandled } }

let redundant_pattern ~region index =
  { region; problem = Redundant_pattern { index } }
