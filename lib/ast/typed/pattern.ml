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
  (* | PBool Union Bool*)
  | P_T_chr of string
  | P_T_str of string
  | P_T_int of int
  | P_T_ctor of (Data.Name.t * t list)
[@@deriving show, compare, equal, hash]
