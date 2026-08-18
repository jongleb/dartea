type problem =
  | Syntax of Syntax_error.t
  | Name of Name_error.t
  | Type of Type_error.t
[@@deriving show]

type t = { region : Data.Region.t; problem : problem } [@@deriving show]

exception Found of t

let syntax ~region problem = { region; problem = Syntax problem }
let name ~region problem = { region; problem = Name problem }
let type_ ~region problem = { region; problem = Type problem }
let raise_type ~region problem = raise (Found (type_ ~region problem))
let raise_name ~region problem = raise (Found (name ~region problem))
