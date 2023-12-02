open OUnit2
open Dartea

let test_ty_type_with_params_complicated _ =
  let expect_data =
    [
      Ast.Type_dec
        {
          Ast.id = "Complicated";
          constrs =
            [
              {
                Ast.id = "Constr1";
                data =
                  [
                    { Ast.params = []; content = Ast.Type_var "a" };
                    {
                      Ast.params = [];
                      content =
                        Ast.Function
                          {
                            Ast.arguments =
                              [
                                { Ast.params = []; content = Ast.Type_var "a" };
                                { Ast.params = []; content = Ast.Type_var "a" };
                              ];
                          };
                    };
                    {
                      Ast.params = [];
                      content =
                        Ast.Record
                          {
                            Ast.values =
                              [
                                {
                                  Ast.key = "field";
                                  value =
                                    {
                                      Ast.params = [];
                                      content =
                                        Ast.Function
                                          {
                                            Ast.arguments =
                                              [
                                                {
                                                  Ast.params =
                                                    [
                                                      {
                                                        Ast.params = [];
                                                        content =
                                                          Ast.Type_var "a";
                                                      };
                                                    ];
                                                  content = Ast.Concrete "Maybe";
                                                };
                                                {
                                                  Ast.params = [];
                                                  content =
                                                    Ast.Concrete "String";
                                                };
                                              ];
                                          };
                                    };
                                };
                              ];
                            row_type = Some "a";
                          };
                    };
                  ];
              };
            ];
          params = [ "a" ];
        };
    ]
  in
  let input =
    "type Complicated a = Constr1 a (a -> a) {a | field: Maybe a -> String}"
  in
  let result = Main.parse (Lexing.from_string input) in
  assert_equal expect_data result

(** I am bothered thinking up the names of these functions. *)

let test_types_fn_2 _ =
  let expect_data =
    [
      Ast.Type_dec
        {
          Ast.id = "Complicated";
          constrs =
            [
              {
                Ast.id = "Constr1";
                data =
                  [
                    { Ast.params = []; content = Ast.Type_var "a" };
                    {
                      Ast.params = [];
                      content =
                        Ast.Function
                          {
                            Ast.arguments =
                              [
                                { Ast.params = []; content = Ast.Type_var "a" };
                                { Ast.params = []; content = Ast.Type_var "a" };
                              ];
                          };
                    };
                    {
                      Ast.params = [];
                      content =
                        Ast.Record
                          {
                            Ast.values =
                              [
                                {
                                  Ast.key = "field";
                                  value =
                                    {
                                      Ast.params = [];
                                      content =
                                        Ast.Function
                                          {
                                            Ast.arguments =
                                              [
                                                {
                                                  Ast.params =
                                                    [
                                                      {
                                                        Ast.params = [];
                                                        content =
                                                          Ast.Type_var "a";
                                                      };
                                                    ];
                                                  content = Ast.Concrete "Maybe";
                                                };
                                                {
                                                  Ast.params = [];
                                                  content =
                                                    Ast.Concrete "String";
                                                };
                                              ];
                                          };
                                    };
                                };
                              ];
                            row_type = Some "a";
                          };
                    };
                  ];
              };
              {
                Ast.id = "Constr2";
                data =
                  [
                    {
                      Ast.params = [];
                      content =
                        Ast.Function
                          {
                            Ast.arguments =
                              [
                                { Ast.params = []; content = Ast.Type_var "a" };
                                { Ast.params = []; content = Ast.Type_var "a" };
                                {
                                  Ast.params = [];
                                  content = Ast.Concrete "String";
                                };
                              ];
                          };
                    };
                    {
                      Ast.params = [];
                      content =
                        Ast.Tuples
                          [
                            { Ast.params = []; content = Ast.Concrete "String" };
                            {
                              Ast.params = [];
                              content =
                                Ast.Function
                                  {
                                    Ast.arguments =
                                      [
                                        {
                                          Ast.params = [];
                                          content = Ast.Type_var "a";
                                        };
                                        {
                                          Ast.params = [];
                                          content = Ast.Type_var "a";
                                        };
                                      ];
                                  };
                            };
                          ];
                    };
                    { Ast.params = []; content = Ast.Concrete "String" };
                    {
                      Ast.params = [];
                      content =
                        Ast.Record
                          {
                            Ast.values =
                              [
                                {
                                  Ast.key = "field";
                                  value =
                                    {
                                      Ast.params = [];
                                      content =
                                        Ast.Tuples
                                          [
                                            {
                                              Ast.params = [];
                                              content =
                                                Ast.Function
                                                  {
                                                    Ast.arguments =
                                                      [
                                                        {
                                                          Ast.params =
                                                            [
                                                              {
                                                                Ast.params = [];
                                                                content =
                                                                  Ast.Type_var
                                                                    "a";
                                                              };
                                                            ];
                                                          content =
                                                            Ast.Concrete "Maybe";
                                                        };
                                                        {
                                                          Ast.params = [];
                                                          content =
                                                            Ast.Concrete
                                                              "String";
                                                        };
                                                      ];
                                                  };
                                            };
                                            {
                                              Ast.params = [];
                                              content = Ast.Type_var "a";
                                            };
                                            {
                                              Ast.params = [];
                                              content =
                                                Ast.Function
                                                  {
                                                    Ast.arguments =
                                                      [
                                                        {
                                                          Ast.params = [];
                                                          content =
                                                            Ast.Type_var "a";
                                                        };
                                                        {
                                                          Ast.params = [];
                                                          content =
                                                            Ast.Type_var "a";
                                                        };
                                                      ];
                                                  };
                                            };
                                          ];
                                    };
                                };
                              ];
                            row_type = Some "a";
                          };
                    };
                  ];
              };
            ];
          params = [ "a" ];
        };
    ]
  in
  let input =
    "type Complicated a = Constr1 a (a -> a) {a | field: Maybe a -> String} | \
     Constr2 (a -> a -> String) (String, a -> a) String {a | field: (Maybe a \
     -> String, a, a -> a)}"
  in
  let result = Main.parse (Lexing.from_string input) in
  assert_equal expect_data result

let suite =
  [
    "test_ty_type_with_params_complicated"
    >:: test_ty_type_with_params_complicated;
    "test_types_fn_2" >:: test_types_fn_2;
  ]
