type t = { region : Data.Region.t; problem : Project_error.t } [@@deriving show]

exception Found of t

let about problem =
  let file = Project_error.file_of problem in
  { region = { Data.Region.nowhere with file }; problem }

let raise_project problem = raise (Found (about problem))
let raise_project_at ~region problem = raise (Found { region; problem })
