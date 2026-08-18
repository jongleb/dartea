open Typed.Type

let over carried result =
  let quantified = Typed.Variable.fresh carried in
  let variable = TVar quantified in
  Scheme ([ quantified ], TFun (variable, TFun (variable, result variable)))

let scheme_of : Data.Operator.t -> scheme =
  let arithmetic =
    over (Some Data.Constraint.Number) (fun variable -> variable)
  in
  let ordering = over (Some Data.Constraint.Comparable) (fun _ -> TBool) in
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
  | Less | Less_or_equal | Greater | Greater_or_equal -> ordering
  | Conjunction | Disjunction -> logical

let values : (string * scheme) list =
  List.map
    (fun operator -> (Data.Operator.lexeme operator, scheme_of operator))
    Data.Operator.all

type builtin_type = Int | Float | Char | Bool | String | Unit | List

let builtin_types = [ Int; Float; Char; Bool; String; Unit; List ]

let name_of_builtin = function
  | Int -> "Int"
  | Float -> "Float"
  | Char -> "Char"
  | Bool -> "Bool"
  | String -> "String"
  | Unit -> "Unit"
  | List -> "List"

let builtin_named written =
  List.find_opt
    (fun builtin -> String.equal (name_of_builtin builtin) written)
    builtin_types

let concrete_type name arguments =
  let builtin =
    match name with
    | Data.Name.Local written -> builtin_named written
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
        Typedecl.name = Data.Name.local "Bool";
        params = [];
        region = Data.Region.nowhere;
        ctors =
          [
            {
              Typedecl.id = Data.Name.local "True";
              data = [];
              region = Data.Region.nowhere;
            };
            {
              Typedecl.id = Data.Name.local "False";
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
