type t = Compare | Minimum | Maximum [@@deriving show, enumerate]

let written_as exported_name =
  Name.global ~module_name:"Basics" ~exported_name

let origin = function
  | Compare -> written_as "compare"
  | Minimum -> written_as "min"
  | Maximum -> written_as "max"

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
