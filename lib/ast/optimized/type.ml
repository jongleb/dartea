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
  | TCustom of Data.Name.t * t list
  | TRecord of t
  | TRowExtend of string * t * t
  | TRowEmpty
[@@deriving show, compare, equal, hash]

type scheme = Scheme of string list * t
[@@deriving show, compare, equal, hash]

let rec arrows = function TFun (_, result) -> 1 + arrows result | _ -> 0

let rec result_after ~applied t =
  if applied <= 0 then t
  else
    match t with
    | TFun (_, result) -> result_after ~applied:(applied - 1) result
    | _ -> t

let rec parameters = function
  | TFun (parameter, result) -> parameter :: parameters result
  | _ -> []
