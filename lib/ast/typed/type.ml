open Ppx_compare_lib.Builtin
open Base.Export

type t =
  | TVar of string
  | TInt
  | TFloat
  | TChar
  | TBool
  | TStr
  | TUnit
  | TFun of t * t
  | TTup of t list
  | TCustom of Data.Name.t * t list
  | TRecord of t
  | TRowExtend of string * t * t
  | TRowEmpty
[@@deriving show, compare, equal, hash]

type scheme = Scheme of string list * t
[@@deriving show, compare, equal, hash]
