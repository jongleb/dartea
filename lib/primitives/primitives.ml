open Typed.Type

let values : (string * scheme) list =
  [
    ("+", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
    ("-", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
    ("*", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
    ("/", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
    ("++", Scheme ([], TFun (TStr, TFun (TStr, TStr))));
    ("==", Scheme ([ "'a" ], TFun (TVar "'a", TFun (TVar "'a", TBool))));
    ("/=", Scheme ([ "'a" ], TFun (TVar "'a", TFun (TVar "'a", TBool))));
    (">", Scheme ([], TFun (TInt, TFun (TInt, TBool))));
    ("<", Scheme ([], TFun (TInt, TFun (TInt, TBool))));
    (">=", Scheme ([], TFun (TInt, TFun (TInt, TBool))));
    ("<=", Scheme ([], TFun (TInt, TFun (TInt, TBool))));
    ("&&", Scheme ([], TFun (TBool, TFun (TBool, TBool))));
    ("||", Scheme ([], TFun (TBool, TFun (TBool, TBool))));
  ]

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
