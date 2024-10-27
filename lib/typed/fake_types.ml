let test_custom_definitions =
  let open Data.Located in
  Frontend.
    [
      {
        Typedecl.name = "Maybe";
        params = [ "a" ];
        ctors =
          [
            {
              Typedecl.id = "Just";
              data =
                [
                  {
                    Typedef.Impl.parameters = [];
                    body = Typedef.Kind.Tkind_var ~?"a";
                  };
                ];
            };
            { Typedecl.id = "None"; data = [] };
          ];
      };
      {
        Typedecl.name = "T1";
        params = [];
        ctors =
          [
            { Typedecl.id = "A"; data = [] };
            {
              Typedecl.id = "B";
              data =
                [
                  {
                    Typedef.Impl.parameters = [];
                    body =
                      Typedef.Kind.Tkind_concrete (Data.Located.dummy "String");
                  };
                ];
            };
          ];
      };
      {
        Typedecl.name = "T2";
        params = [];
        ctors =
          [ { Typedecl.id = "C"; data = [] }; { Typedecl.id = "D"; data = [] } ];
      };
      {
        Typedecl.name = "T3";
        params = [];
        ctors =
          [
            {
              Typedecl.id = "E";
              data =
                [
                  {
                    Typedef.Impl.parameters = [];
                    body = Typedef.Kind.Tkind_concrete (Data.Located.dummy "T2");
                  };
                ];
            };
            {
              Typedecl.id = "F";
              data =
                [
                  {
                    Typedef.Impl.parameters = [];
                    body = Typedef.Kind.Tkind_concrete (Data.Located.dummy "T1");
                  };
                ];
            };
            {
              Typedecl.id = "G";
              data =
                [
                  {
                    Typedef.Impl.parameters = [];
                    body =
                      Typedef.Kind.Tkind_concrete (Data.Located.dummy "Int");
                  };
                ];
            };
          ];
      };
    ]

(* type T1 = A | B String
   type T2 = C | D
   type T3 = E T2 | F T1 | G Int *)
