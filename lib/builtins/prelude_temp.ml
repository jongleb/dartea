open Typed.Type

let values : (string * scheme) list =
  [
    ("identity", Scheme ([ "'a" ], TFun (TVar "'a", TVar "'a")));
    ( "always",
      Scheme ([ "'a"; "'b" ], TFun (TVar "'a", TFun (TVar "'b", TVar "'a"))) );
    ("fromInt", Scheme ([], TFun (TInt, TStr)));
    ("toInt", Scheme ([], TFun (TStr, TCustom (Data.Name.local "Maybe", [ TInt ]))));
    ("length", Scheme ([], TFun (TStr, TInt)));
    ("negate", Scheme ([], TFun (TInt, TInt)));
    ("append", Scheme ([], TFun (TStr, TFun (TStr, TStr))));
    ("++", Scheme ([], TFun (TStr, TFun (TStr, TStr))));
    ( "pair",
      Scheme
        ( [ "'a"; "'b" ],
          TFun (TVar "'a", TFun (TVar "'b", TTup [ TVar "'a"; TVar "'b" ])) ) );
    ( "first",
      Scheme ([ "'a"; "'b" ], TFun (TTup [ TVar "'a"; TVar "'b" ], TVar "'a")) );
    ( "second",
      Scheme ([ "'a"; "'b" ], TFun (TTup [ TVar "'a"; TVar "'b" ], TVar "'b")) );
    ( "|>",
      Scheme
        ( [ "'a"; "'b" ],
          TFun (TVar "'a", TFun (TFun (TVar "'a", TVar "'b"), TVar "'b")) ) );
    ( "<|",
      Scheme
        ( [ "'a"; "'b" ],
          TFun (TFun (TVar "'a", TVar "'b"), TFun (TVar "'a", TVar "'b")) ) );
  ]

let types : Canonical.Typedecl.t list =
  let open Data.Located in
  Canonical.
    [
      {
        Typedecl.name = Data.Name.local "Maybe";
        params = [ "a" ];
        ctors =
          [
            {
              Typedecl.id = Data.Name.local "Just";
              data =
                [
                  {
                    Typedef.Impl.parameters = [];
                    body = Typedef.Kind.Tkind_var ~?"a";
                  };
                ];
            };
            { Typedecl.id = Data.Name.local "Nothing"; data = [] };
          ];
      };
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
