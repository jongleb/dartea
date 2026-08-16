open Typed.Type

let constrained written result =
  let variable = TVar written in
  Scheme ([ written ], TFun (variable, TFun (variable, result variable)))

let scheme_of : Data.Operator.t -> scheme =
  let number = Data.Constraint.(name Number) in
  let comparable = Data.Constraint.(name Comparable) in
  let appendable = Data.Constraint.(name Appendable) in
  let arithmetic = constrained number (fun variable -> variable) in
  let ordering = constrained comparable (fun _ -> TBool) in
  let concatenation = constrained appendable (fun variable -> variable) in
  let float_division = Scheme ([], TFun (TFloat, TFun (TFloat, TFloat))) in
  let integer_division = Scheme ([], TFun (TInt, TFun (TInt, TInt))) in
  let logical = Scheme ([], TFun (TBool, TFun (TBool, TBool))) in
  let equality = Scheme ([ "'a" ], TFun (TVar "'a", TFun (TVar "'a", TBool))) in
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
