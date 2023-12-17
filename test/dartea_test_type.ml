open OUnit2
open Ast.Kind.Frontend
open Data
open Located
module Main = Parse.Main

let test_ty_type_with_params_complicated _ =
  let expect_data =
    [
      Impl.Type_dec
        {
          Typedecl.name = "Complicated";
          ctors =
            [
              {
                Typedecl.id = "Constr1";
                data =
                  [
                    {
                      Typedef.Impl.parameters = [];
                      body = Typedef.Kind.Tkind_var ~?"a";
                    };
                    {
                      Typedef.Impl.parameters = [];
                      body =
                        Typedef.Kind.Tkind_function
                          {
                            Typedef.Type_function.arguments =
                              [
                                {
                                  Typedef.Impl.parameters = [];
                                  body = Typedef.Kind.Tkind_var ~?"a";
                                };
                                {
                                  Typedef.Impl.parameters = [];
                                  body = Typedef.Kind.Tkind_var ~?"a";
                                };
                              ];
                          };
                    };
                    {
                      Typedef.Impl.parameters = [];
                      body =
                        Typedef.Kind.Tkind_record
                          {
                            Typedef.Type_record.values =
                              [
                                {
                                  Typedef.Type_record_row.name = ~?"field";
                                  body =
                                    {
                                      Typedef.Impl.parameters = [];
                                      body =
                                        Typedef.Kind.Tkind_function
                                          {
                                            Typedef.Type_function.arguments =
                                              [
                                                {
                                                  Typedef.Impl.parameters =
                                                    [
                                                      {
                                                        Typedef.Impl.parameters =
                                                          [];
                                                        body =
                                                          Typedef.Kind.Tkind_var
                                                            ~?"a";
                                                      };
                                                    ];
                                                  body =
                                                    Typedef.Kind.Tkind_concrete
                                                      ~?"Maybe";
                                                };
                                                {
                                                  Typedef.Impl.parameters = [];
                                                  body =
                                                    Typedef.Kind.Tkind_concrete
                                                      ~?"String";
                                                };
                                              ];
                                          };
                                    };
                                };
                              ];
                            row_type = Some ~?"a";
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
      Impl.Type_dec
        {
          Typedecl.name = "Complicated";
          ctors =
            [
              {
                Typedecl.id = "Constr1";
                data =
                  [
                    {
                      Typedef.Impl.parameters = [];
                      body = Typedef.Kind.Tkind_var ~?"a";
                    };
                    {
                      Typedef.Impl.parameters = [];
                      body =
                        Typedef.Kind.Tkind_function
                          {
                            Typedef.Type_function.arguments =
                              [
                                {
                                  Typedef.Impl.parameters = [];
                                  body = Typedef.Kind.Tkind_var ~?"a";
                                };
                                {
                                  Typedef.Impl.parameters = [];
                                  body = Typedef.Kind.Tkind_var ~?"a";
                                };
                              ];
                          };
                    };
                    {
                      Typedef.Impl.parameters = [];
                      body =
                        Typedef.Kind.Tkind_record
                          {
                            Typedef.Type_record.values =
                              [
                                {
                                  Typedef.Type_record_row.name = ~?"field";
                                  body =
                                    {
                                      Typedef.Impl.parameters = [];
                                      body =
                                        Typedef.Kind.Tkind_function
                                          {
                                            Typedef.Type_function.arguments =
                                              [
                                                {
                                                  Typedef.Impl.parameters =
                                                    [
                                                      {
                                                        Typedef.Impl.parameters =
                                                          [];
                                                        body =
                                                          Typedef.Kind.Tkind_var
                                                            ~?"a";
                                                      };
                                                    ];
                                                  body =
                                                    Typedef.Kind.Tkind_concrete
                                                      ~?"Maybe";
                                                };
                                                {
                                                  Typedef.Impl.parameters = [];
                                                  body =
                                                    Typedef.Kind.Tkind_concrete
                                                      ~?"String";
                                                };
                                              ];
                                          };
                                    };
                                };
                              ];
                            row_type = Some ~?"a";
                          };
                    };
                  ];
              };
              {
                Typedecl.id = "Constr2";
                data =
                  [
                    {
                      Typedef.Impl.parameters = [];
                      body =
                        Typedef.Kind.Tkind_function
                          {
                            Typedef.Type_function.arguments =
                              [
                                {
                                  Typedef.Impl.parameters = [];
                                  body = Typedef.Kind.Tkind_var ~?"a";
                                };
                                {
                                  Typedef.Impl.parameters = [];
                                  body = Typedef.Kind.Tkind_var ~?"a";
                                };
                                {
                                  Typedef.Impl.parameters = [];
                                  body = Typedef.Kind.Tkind_concrete ~?"String";
                                };
                              ];
                          };
                    };
                    {
                      Typedef.Impl.parameters = [];
                      body =
                        Typedef.Kind.Tkind_tuple
                          [
                            {
                              Typedef.Impl.parameters = [];
                              body = Typedef.Kind.Tkind_concrete ~?"String";
                            };
                            {
                              Typedef.Impl.parameters = [];
                              body =
                                Typedef.Kind.Tkind_function
                                  {
                                    Typedef.Type_function.arguments =
                                      [
                                        {
                                          Typedef.Impl.parameters = [];
                                          body = Typedef.Kind.Tkind_var ~?"a";
                                        };
                                        {
                                          Typedef.Impl.parameters = [];
                                          body = Typedef.Kind.Tkind_var ~?"a";
                                        };
                                      ];
                                  };
                            };
                          ];
                    };
                    {
                      Typedef.Impl.parameters = [];
                      body = Typedef.Kind.Tkind_concrete ~?"String";
                    };
                    {
                      Typedef.Impl.parameters = [];
                      body =
                        Typedef.Kind.Tkind_record
                          {
                            Typedef.Type_record.values =
                              [
                                {
                                  Typedef.Type_record_row.name = ~?"field";
                                  body =
                                    {
                                      Typedef.Impl.parameters = [];
                                      body =
                                        Typedef.Kind.Tkind_tuple
                                          [
                                            {
                                              Typedef.Impl.parameters = [];
                                              body =
                                                Typedef.Kind.Tkind_function
                                                  {
                                                    Typedef.Type_function
                                                    .arguments =
                                                      [
                                                        {
                                                          Typedef.Impl
                                                          .parameters =
                                                            [
                                                              {
                                                                Typedef.Impl
                                                                .parameters = [];
                                                                body =
                                                                  Typedef.Kind
                                                                  .Tkind_var
                                                                    ~?"a";
                                                              };
                                                            ];
                                                          body =
                                                            Typedef.Kind
                                                            .Tkind_concrete
                                                              ~?"Maybe";
                                                        };
                                                        {
                                                          Typedef.Impl
                                                          .parameters = [];
                                                          body =
                                                            Typedef.Kind
                                                            .Tkind_concrete
                                                              ~?"String";
                                                        };
                                                      ];
                                                  };
                                            };
                                            {
                                              Typedef.Impl.parameters = [];
                                              body =
                                                Typedef.Kind.Tkind_var ~?"a";
                                            };
                                            {
                                              Typedef.Impl.parameters = [];
                                              body =
                                                Typedef.Kind.Tkind_function
                                                  {
                                                    Typedef.Type_function
                                                    .arguments =
                                                      [
                                                        {
                                                          Typedef.Impl
                                                          .parameters = [];
                                                          body =
                                                            Typedef.Kind
                                                            .Tkind_var
                                                              ~?"a";
                                                        };
                                                        {
                                                          Typedef.Impl
                                                          .parameters = [];
                                                          body =
                                                            Typedef.Kind
                                                            .Tkind_var
                                                              ~?"a";
                                                        };
                                                      ];
                                                  };
                                            };
                                          ];
                                    };
                                };
                              ];
                            row_type = Some ~?"a";
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
