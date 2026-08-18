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
  | P_ctor of (string * t list)
[@@deriving show]
