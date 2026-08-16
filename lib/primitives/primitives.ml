open Typed.Type

let scheme_of : Data.Operator.t -> scheme =
  let comparable = Scheme ([], TFun (TInt, TFun (TInt, TBool))) in
  let arithmetic = Scheme ([], TFun (TInt, TFun (TInt, TInt))) in
  let logical = Scheme ([], TFun (TBool, TFun (TBool, TBool))) in
  let equality = Scheme ([ "'a" ], TFun (TVar "'a", TFun (TVar "'a", TBool))) in
  function
  | Add | Subtract | Multiply | Divide | Integer_divide | Power -> arithmetic
  | Append -> Scheme ([], TFun (TStr, TFun (TStr, TStr)))
  | Equal | Not_equal -> equality
  | Less | Less_or_equal | Greater | Greater_or_equal -> comparable
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
