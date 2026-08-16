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

module By_variable = Stdlib.Map.Make (Stdlib.String)

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

let function_of parameters ~result =
  List.fold_right (fun parameter tail -> TFun (parameter, tail)) parameters
    result

let rec substitute bindings t =
  match t with
  | TVar name -> begin
      match By_variable.find_opt name bindings with
      | Some bound -> bound
      | None -> t
    end
  | TInt | TFloat | TChar | TBool | TStr | TUnit | TRowEmpty -> t
  | TFun (parameter, result) ->
      TFun (substitute bindings parameter, substitute bindings result)
  | TTup items -> TTup (List.map (substitute bindings) items)
  | TCustom (name, arguments) ->
      TCustom (name, List.map (substitute bindings) arguments)
  | TRecord row -> TRecord (substitute bindings row)
  | TRowExtend (label, field, rest) ->
      TRowExtend (label, substitute bindings field, substitute bindings rest)
