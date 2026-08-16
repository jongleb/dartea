open Ppx_compare_lib.Builtin
open Base.Export

type t = { typ : Type.t; pattern : kind }
[@@deriving show, compare, equal, hash]

and kind =
  | P_T_anything
  | P_T_var of string
  | P_T_record of string list
  | P_T_alias of (t * string)
  | P_T_unit
  | P_T_tuple of t list
  | P_T_list of t list
  | P_T_cons of (t * t)
  | P_T_chr of string
  | P_T_str of string
  | P_T_int of int
  | P_T_ctor of (Data.Name.t * t list)
[@@deriving show, compare, equal, hash]

let rec substitute bindings p =
  let inner = substitute bindings in
  let pattern =
    match p.pattern with
    | P_T_tuple items -> P_T_tuple (List.map inner items)
    | P_T_list items -> P_T_list (List.map inner items)
    | P_T_cons (head, tail) -> P_T_cons (inner head, inner tail)
    | P_T_ctor (name, arguments) -> P_T_ctor (name, List.map inner arguments)
    | P_T_alias (aliased, name) -> P_T_alias (inner aliased, name)
    | ( P_T_anything | P_T_var _ | P_T_record _ | P_T_unit | P_T_chr _
      | P_T_str _ | P_T_int _ ) as leaf ->
        leaf
  in
  { typ = Type.substitute bindings p.typ; pattern }
