type colour =
  | Red [@rename "\027[31m"]
  | Yellow [@rename "\027[33m"]
  | Cyan [@rename "\027[36m"]
  | Green [@rename "\027[32m"]
[@@deriving to_string]

type t =
  | Empty
  | Text of string
  | Coloured of colour * t
  | Beside of t list
  | Above of t list
  | Indent of int * t
  | Paragraph of string

let empty = Empty
let text written = Text written
let words written = Paragraph written
let beside parts = Beside parts
let above parts = Above parts
let indent width inner = Indent (width, inner)
let red inner = Coloured (Red, inner)
let yellow inner = Coloured (Yellow, inner)
let cyan inner = Coloured (Cyan, inner)
let green inner = Coloured (Green, inner)
let blank = Text ""
let stack parts =
  match parts with
  | [] -> Empty
  | first :: rest ->
      Above (first :: List.concat_map (fun part -> [ blank; part ]) rest)

let escape = string_of_colour

let reset = "\027[0m"

let wrapped ~width written =
  let words = String.split_on_char ' ' written |> List.filter (( <> ) "") in
  let lines, last =
    List.fold_left
      (fun (lines, current) word ->
        match current with
        | "" -> (lines, word)
        | _ when String.length current + 1 + String.length word <= width ->
            (lines, current ^ " " ^ word)
        | _ -> (current :: lines, word))
      ([], "") words
  in
  let completed = if String.equal last "" then lines else last :: lines in
  List.rev completed

let glued left right =
  match (left, right) with
  | [], lines | lines, [] -> lines
  | _ ->
      let start = List.filteri (fun index _ -> index < List.length left - 1) left in
      let joined = List.nth left (List.length left - 1) ^ List.hd right in
      start @ (joined :: List.tl right)

let rec lines_of ~width ~colours doc =
  match doc with
  | Empty -> []
  | Text written -> [ written ]
  | Paragraph written -> wrapped ~width written
  | Coloured (colour, inner) ->
      if colours then
        List.map
          (fun line -> escape colour ^ line ^ reset)
          (lines_of ~width ~colours inner)
      else lines_of ~width ~colours inner
  | Beside parts ->
      List.fold_left
        (fun collected part -> glued collected (lines_of ~width ~colours part))
        [] parts
  | Above parts ->
      List.concat_map (fun part -> lines_of ~width ~colours part) parts
  | Indent (by, inner) ->
      List.map
        (fun line -> if String.equal line "" then line else String.make by ' ' ^ line)
        (lines_of ~width:(width - by) ~colours inner)

let to_string ~colours doc =
  String.concat "\n" (lines_of ~width:80 ~colours doc)
