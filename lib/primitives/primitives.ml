open Typed.Type

let over carried result =
  let alpha = Typed.Variable.fresh carried in
  let variable = TVar alpha in
  Scheme ([ alpha ], TFun (variable, TFun (variable, result variable)))

let scheme_of : Data.Operator.t -> scheme =
  let arithmetic =
    over (Some Data.Constraint.Number) (fun variable -> variable)
  in
  let comparison = over (Some Data.Constraint.Comparable) (fun _ -> TBool) in
  let concatenation =
    over (Some Data.Constraint.Appendable) (fun variable -> variable)
  in
  let float_division = Scheme ([], TFun (TFloat, TFun (TFloat, TFloat))) in
  let integer_division = Scheme ([], TFun (TInt, TFun (TInt, TInt))) in
  let logical = Scheme ([], TFun (TBool, TFun (TBool, TBool))) in
  let equality = over None (fun _ -> TBool) in
  function
  | Add | Subtract | Multiply | Power -> arithmetic
  | Divide -> float_division
  | Integer_divide -> integer_division
  | Append -> concatenation
  | Equal | Not_equal -> equality
  | Less | Less_or_equal | Greater | Greater_or_equal -> comparison
  | Conjunction | Disjunction -> logical

let values : (string * scheme) list =
  List.map
    (fun operator -> (Data.Operator.lexeme operator, scheme_of operator))
    Data.Operator.all

type builtin_type = Int | Float | Char | Bool | String | Unit | List
[@@deriving enumerate, to_string, equal]

let builtin_types = all_of_builtin_type
let name_of_builtin = string_of_builtin_type
let builtin_of_scalar = function
  | TInt -> Some Int
  | TFloat -> Some Float
  | TChar -> Some Char
  | TBool -> Some Bool
  | TStr -> Some String
  | TUnit -> Some Unit
  | TVar _ | TFun _ | TTup _ | TCustom _ | TRecord _ | TRowExtend _ | TRowEmpty
    ->
      None

let builtin_of_name written =
  List.find_opt
    (fun builtin -> String.equal (name_of_builtin builtin) written)
    builtin_types
module Known_type = struct
  type t = Maybe | List | Value [@@deriving enumerate, to_string, equal]

  let of_name name =
    List.find_opt
      (fun known -> String.equal (to_string known) (Data.Name.base name))
      all

  let fits known name =
    match of_name name with Some found -> equal found known | None -> false
end

let true_ = "True"
let false_ = "False"
let unit_ = "Unit"
let unit_written = "()"
let negate = Data.Name.local "negate"

let bool_of_constructor name =
  let written = Data.Name.base name in
  if String.equal written true_ then Some true
  else if String.equal written false_ then Some false
  else None

let bool_constructor value = Data.Name.local (if value then true_ else false_)

let is_unit_constructor name =
  let written = Data.Name.base name in
  String.equal written unit_ || String.equal written unit_written

let concrete_type name arguments =
  let builtin =
    match name with
    | Data.Name.Local written -> builtin_of_name written
    | Data.Name.Global _ -> None
  in
  match (builtin, arguments) with
  | Some Int, [] -> TInt
  | Some Float, [] -> TFloat
  | Some Char, [] -> TChar
  | Some Bool, [] -> TBool
  | Some String, [] -> TStr
  | Some Unit, [] -> TUnit
  | Some (Int | Float | Char | Bool | String | Unit | List), _ | None, _ ->
      TCustom (name, arguments)

let type_names = List.map name_of_builtin builtin_types

let types : Canonical.Typedecl.t list =
  Canonical.
    [
      {
        Typedecl.name = Data.Name.local (name_of_builtin Bool);
        params = [];
        region = Data.Region.nowhere;
        ctors =
          [
            {
              Typedecl.id = Data.Name.local true_;
              data = [];
              region = Data.Region.nowhere;
            };
            {
              Typedecl.id = Data.Name.local false_;
              data = [];
              region = Data.Region.nowhere;
            };
          ];
      };
    ]

let term_names =
  List.map fst values
  @ List.concat_map
      (fun (declared : Canonical.Typedecl.t) ->
        List.map
          (fun (ctor : Canonical.Typedecl.type_ctor) -> Data.Name.base ctor.id)
          declared.ctors)
      types
