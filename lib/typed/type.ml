open Ppx_compare_lib.Builtin
open Base.Export

type t =
  | TVar of string
  | TInt
  | TBool
  | TStr
  | TUnit
  | TFun of t * t
  | TTup of t list
  | TCustom of string * t list
[@@deriving show, compare, equal, hash]

type scheme = Scheme of string list * t
[@@deriving show, compare, equal, hash]
