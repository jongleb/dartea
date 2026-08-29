type t =
  | Compare [@rename "compare"]
  | Minimum [@rename "min"]
  | Maximum [@rename "max"]
[@@deriving show, enumerate, to_string]

let written_as exported_name =
  Name.global ~module_name:"Basics" ~exported_name

let origin method_ = written_as (to_string method_)

let by_origin =
  Hashtbl.of_seq (Seq.map (fun method_ -> (origin method_, method_)) (List.to_seq all))
let referred_to_by name = Hashtbl.find_opt by_origin name

type ordering_result = { less : Name.t; equal : Name.t; greater : Name.t }

let ordering_result =
  {
    less = written_as "LT";
    equal = written_as "EQ";
    greater = written_as "GT";
  }
