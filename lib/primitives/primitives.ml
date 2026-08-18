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

let types : Canonical.Typedecl.t list =
  Canonical.
    [
      {
        Typedecl.name = Data.Name.local "Bool";
        params = [];
        ctors =
          [
            { Typedecl.id = Data.Name.local "True"; data = [] };
            { Typedecl.id = Data.Name.local "False"; data = [] };
          ];
      };
    ]
