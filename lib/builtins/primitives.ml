open Typed.Type

let values : (string * scheme) list =
  [
    ("+", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
    ("-", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
    ("*", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
    ("/", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
    ("^", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
    ("==", Scheme ([ "'a" ], TFun (TVar "'a", TFun (TVar "'a", TBool))));
    ("/=", Scheme ([ "'a" ], TFun (TVar "'a", TFun (TVar "'a", TBool))));
    (">", Scheme ([], TFun (TInt, TFun (TInt, TBool))));
    ("<", Scheme ([], TFun (TInt, TFun (TInt, TBool))));
    (">=", Scheme ([], TFun (TInt, TFun (TInt, TBool))));
    ("<=", Scheme ([], TFun (TInt, TFun (TInt, TBool))));
    ("&&", Scheme ([], TFun (TBool, TFun (TBool, TBool))));
    ("||", Scheme ([], TFun (TBool, TFun (TBool, TBool))));
  ]
