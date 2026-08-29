type t =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Integer_divide
  | Power
  | Append
  | Equal
  | Not_equal
  | Less
  | Less_or_equal
  | Greater
  | Greater_or_equal
  | Conjunction
  | Disjunction
[@@deriving show, enumerate]

let lexeme = function
  | Add -> "+"
  | Subtract -> "-"
  | Multiply -> "*"
  | Divide -> "/"
  | Integer_divide -> "//"
  | Power -> "^"
  | Append -> "++"
  | Equal -> "=="
  | Not_equal -> "/="
  | Less -> "<"
  | Less_or_equal -> "<="
  | Greater -> ">"
  | Greater_or_equal -> ">="
  | Conjunction -> "&&"
  | Disjunction -> "||"

let by_lexeme =
  Hashtbl.of_seq
    (Seq.map (fun operator -> (lexeme operator, operator)) (List.to_seq all))

let of_lexeme written = Hashtbl.find_opt by_lexeme written

let referred_to_by (name : Name.t) =
  match name with
  | Name.Local written -> of_lexeme written
  | Name.Global _ -> None
