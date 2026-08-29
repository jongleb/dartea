type t =
  | Number [@rename "number"]
  | Comparable [@rename "comparable"]
  | Appendable [@rename "appendable"]
  | Comp_appendable [@rename "compappend"]
[@@deriving show, equal, enumerate, to_string]

let name = to_string

let rec only_digits written index =
  if index >= String.length written then true
  else
    match written.[index] with
    | '0' .. '9' -> only_digits written (index + 1)
    | _ -> false

let names written candidate =
  let prefix = name candidate in
  String.starts_with ~prefix written && only_digits written (String.length prefix)

let of_written written = List.find_opt (names written) all

let combined left right =
  match (left, right) with
  | Number, Number -> Some Number
  | Number, Comparable | Comparable, Number -> Some Number
  | Comparable, Comparable -> Some Comparable
  | Appendable, Appendable -> Some Appendable
  | Comparable, Appendable | Appendable, Comparable -> Some Comp_appendable
  | Comp_appendable, (Comparable | Appendable | Comp_appendable)
  | (Comparable | Appendable), Comp_appendable ->
      Some Comp_appendable
  | Number, (Appendable | Comp_appendable)
  | (Appendable | Comp_appendable), Number ->
      None
