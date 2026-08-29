type t =
  | Add [@rename "+"]
  | Subtract [@rename "-"]
  | Multiply [@rename "*"]
  | Divide [@rename "/"]
  | Integer_divide [@rename "//"]
  | Power [@rename "^"]
  | Append [@rename "++"]
  | Equal [@rename "=="]
  | Not_equal [@rename "/="]
  | Less [@rename "<"]
  | Less_or_equal [@rename "<="]
  | Greater [@rename ">"]
  | Greater_or_equal [@rename ">="]
  | Conjunction [@rename "&&"]
  | Disjunction [@rename "||"]
[@@deriving show, enumerate, to_string]

let lexeme = to_string

let of_lexeme written =
  List.find_opt (fun operator -> String.equal (lexeme operator) written) all

let referred_to_by (name : Name.t) =
  match name with
  | Name.Local written -> of_lexeme written
  | Name.Global _ -> None
