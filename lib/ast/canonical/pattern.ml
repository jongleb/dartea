open Ppx_compare_lib.Builtin
open Base.Export

type t =
  | P_anything
  | P_var of string
  | P_record of string list
  (* | PAlias Pattern Name ?? *)
  | P_unit
  | P_tuple of t list
  | P_list of t list
  | P_cons of (t * t)
  (* | PBool Union Bool*)
  | P_chr of string
  | P_str of string
  | P_int of int
  | P_ctor of (string * t list)
[@@deriving show, compare, equal, hash]

let rec of_frontend = function
  | Frontend.Pattern.P_anything -> P_anything
  | P_tuple t -> P_tuple (List.map of_frontend t)
  | P_list l -> P_list (List.map of_frontend l)
  | P_cons (a, b) -> P_cons (of_frontend a, of_frontend b)
  | P_chr c -> P_chr c
  | P_str s -> P_str s
  | P_int i -> P_int i
  | P_ctor (s, l) -> P_ctor (s, List.map of_frontend l)
  | P_unit -> P_unit
  | P_var s -> P_var s
  | P_record r -> P_record r
