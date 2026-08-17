type t = Number | Comparable | Appendable | Comp_appendable
[@@deriving show, enumerate]

let name = function
  | Number -> "number"
  | Comparable -> "comparable"
  | Appendable -> "appendable"
  | Comp_appendable -> "compappend"

let generated = '#'

let rec only_digits written index =
  if index >= String.length written then true
  else
    match written.[index] with
    | '0' .. '9' -> only_digits written (index + 1)
    | _ -> false

let numbered written index =
  if index < String.length written && Char.equal written.[index] generated then
    only_digits written (index + 1)
  else only_digits written index

let names written candidate =
  let prefix = name candidate in
  String.starts_with ~prefix written
  && numbered written (String.length prefix)

let of_variable written = List.find_opt (names written) all

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
