type t = kind Data.Located.t [@@deriving show]

and kind =
  | P_anything
  | P_var of string
  | P_record of string list
  | P_alias of (t * string)
  | P_unit
  | P_tuple of t list
  | P_list of t list
  | P_cons of (t * t)
  | P_chr of string
  | P_str of string
  | P_int of int
  | P_ctor of (Data.Name.t * t list)
[@@deriving show]

let rec of_frontend (pattern : Frontend.Pattern.t) : t =
  let same kind = Data.Located.at pattern.region kind in
  match pattern.thing with
  | Frontend.Pattern.P_anything -> same P_anything
  | P_tuple items -> same (P_tuple (List.map of_frontend items))
  | P_list items -> same (P_list (List.map of_frontend items))
  | P_cons (head, tail) -> same (P_cons (of_frontend head, of_frontend tail))
  | P_chr letter -> same (P_chr letter)
  | P_str text -> same (P_str text)
  | P_int value -> same (P_int value)
  | P_ctor (ctor, arguments) ->
      same (P_ctor (Data.Name.of_dotted ctor, List.map of_frontend arguments))
  | P_alias (inner, name) -> same (P_alias (of_frontend inner, name))
  | P_unit -> same P_unit
  | P_var name -> same (P_var name)
  | P_record fields -> same (P_record fields)
