open Ppx_compare_lib.Builtin
open Base.Export

type t =
  | TVar of t Variable.t
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

type scheme = Scheme of t Variable.t list * t
[@@deriving show, compare, equal, hash]

module Identity = struct
  type nonrec t = t Variable.t

  let compare left right =
    Stdlib.Int.compare (Variable.identity left) (Variable.identity right)
end

module By_variable = Stdlib.Map.Make (Identity)
module Variables = Stdlib.Set.Make (Identity)

let fresh_variable constrained = TVar (Variable.fresh constrained)
let list_of element = TCustom (Data.Name.local "List", [ element ])

let rec head ty =
  match ty with
  | TVar variable -> begin
      match Variable.state variable with
      | Variable.Linked target -> head target
      | Variable.Unbound _ -> ty
    end
  | TInt | TFloat | TChar | TBool | TStr | TUnit | TFun _ | TTup _ | TCustom _
  | TRecord _ | TRowExtend _ | TRowEmpty ->
      ty

let map_children f ty =
  match ty with
  | TFun (parameter, result) -> TFun (f parameter, f result)
  | TTup items -> TTup (List.map f items)
  | TCustom (name, arguments) -> TCustom (name, List.map f arguments)
  | TRecord row -> TRecord (f row)
  | TRowExtend (label, field, rest) -> TRowExtend (label, f field, f rest)
  | (TVar _ | TInt | TFloat | TChar | TBool | TStr | TUnit | TRowEmpty) as leaf
    ->
      leaf

let rec zonk ty = map_children zonk (head ty)

let zonk_scheme (Scheme (quantified, body)) = Scheme (quantified, zonk body)

let rec fold_variables f collected ty =
  let across collected types = List.fold_left (fold_variables f) collected types in
  match head ty with
  | TVar variable -> f collected variable
  | TInt | TFloat | TChar | TBool | TStr | TUnit | TRowEmpty -> collected
  | TFun (parameter, result) -> across collected [ parameter; result ]
  | TTup items | TCustom (_, items) -> across collected items
  | TRecord row -> fold_variables f collected row
  | TRowExtend (_, field, rest) -> across collected [ field; rest ]

let iter_variables f ty = fold_variables (fun () variable -> f variable) () ty

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
  | TVar variable -> Option.value ~default:t (By_variable.find_opt variable bindings)
  | TInt | TFloat | TChar | TBool | TStr | TUnit | TRowEmpty | TFun _ | TTup _
  | TCustom _ | TRecord _ | TRowExtend _ ->
      map_children (substitute bindings) t
