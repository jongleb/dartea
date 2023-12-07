type t =
  | P_anything
  | P_var of string
  | P_record of string list
  (* | PAlias Pattern Name ?? *)
  | P_unit
  (* | PTuple of (pattern * pattern) (Maybe Pattern) *)
  | P_list of t list
  | P_cons of (t * t)
  (* | PBool Union Bool*)
  | P_chr of string
  | P_str of string
  | P_int of int
  | P_ctor of (string * t list)
[@@deriving show]
