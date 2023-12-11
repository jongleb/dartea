open OUnit2
open Dartea
open Ast

let test_ty_alias_record _ =
  let expect_data =
    [
      Utils.make_type_alias_no_loc_top ~name:"User"
        ~typedef:
          {
            Typedef.parameters = [];
            body =
              Typedef.Tkind_record
                {
                  Typedef.values =
                    [
                      {
                        Typedef.name = "name";
                        body =
                          {
                            Typedef.parameters = [];
                            body = Typedef.Tkind_concrete "String";
                          };
                      };
                      {
                        Typedef.name = "age";
                        body =
                          {
                            Typedef.parameters = [];
                            body = Typedef.Tkind_concrete "Int";
                          };
                      };
                    ];
                  row_type = None;
                };
          }
        ();
    ]
  in
  let input = "type alias User = { name: String, age: Int }" in
  let result = Main.parse (Lexing.from_string input) in
  assert_equal expect_data result

let test_ty_alias_type_with_params _ =
  let expect_data =
    [
      Impl.Type_alias
        {
          Typealias.typedef =
            {
              Typedef.parameters =
                [
                  {
                    Typedef.parameters = [];
                    body = Typedef.Tkind_concrete "Param1";
                  };
                  {
                    Typedef.parameters =
                      [
                        {
                          Typedef.parameters = [];
                          body = Typedef.Tkind_concrete "Param3";
                        };
                        {
                          Typedef.parameters =
                            [
                              {
                                Typedef.parameters = [];
                                body = Typedef.Tkind_concrete "Param5";
                              };
                            ];
                          body = Typedef.Tkind_concrete "Param4";
                        };
                      ];
                    body = Typedef.Tkind_concrete "Param2";
                  };
                ];
              body = Typedef.Tkind_concrete "With";
            };
          params = [];
          name = "Complicated";
        };
    ]
  in
  let input =
    "type alias Complicated = With Param1 (Param2 Param3 (Param4 Param5))"
  in
  let result = Main.parse (Lexing.from_string input) in
  assert_equal expect_data result

let test_ty_alias_type_with_params_and_record_param _ =
  let expect_data =
    [
      Impl.Type_alias
        {
          Typealias.typedef =
            {
              Typedef.parameters =
                [
                  {
                    Typedef.parameters = [];
                    body = Typedef.Tkind_concrete "Param1";
                  };
                  {
                    Typedef.parameters =
                      [
                        {
                          Typedef.parameters = [];
                          body =
                            Typedef.Tkind_record
                              {
                                Typedef.values =
                                  [
                                    {
                                      Typedef.name = "a";
                                      body =
                                        {
                                          Typedef.parameters = [];
                                          body = Typedef.Tkind_concrete "String";
                                        };
                                    };
                                  ];
                                row_type = None;
                              };
                        };
                      ];
                    body = Typedef.Tkind_concrete "Param2";
                  };
                ];
              body = Typedef.Tkind_concrete "With";
            };
          params = [];
          name = "Complicated";
        };
    ]
  in
  let input = "type alias Complicated = With Param1 (Param2 {a: String})" in
  let result = Main.parse (Lexing.from_string input) in
  assert_equal expect_data result

let test_ty_alias_type_with_params_and_record_param_with_a_lot_of_parens _ =
  let expect_data =
    [
      Impl.Type_alias
        {
          Typealias.typedef =
            {
              Typedef.parameters =
                [
                  {
                    Typedef.parameters = [];
                    body = Typedef.Tkind_concrete "Param1";
                  };
                  {
                    Typedef.parameters =
                      [
                        {
                          Typedef.parameters = [];
                          body =
                            Typedef.Tkind_record
                              {
                                Typedef.values =
                                  [
                                    {
                                      Typedef.name = "a";
                                      body =
                                        {
                                          Typedef.parameters = [];
                                          body = Typedef.Tkind_concrete "String";
                                        };
                                    };
                                  ];
                                row_type = None;
                              };
                        };
                      ];
                    body = Typedef.Tkind_concrete "Param2";
                  };
                ];
              body = Typedef.Tkind_concrete "With";
            };
          params = [];
          name = "Complicated";
        };
    ]
  in
  let input =
    "type alias Complicated = With ((Param1)) (Param2 {a: ((((((String))))))})"
  in
  let result = Main.parse (Lexing.from_string input) in
  assert_equal expect_data result

let test_ty_alias_tuples _ =
  let expect_data =
    [
      Impl.Type_alias
        {
          Typealias.typedef =
            {
              Typedef.parameters = [];
              body =
                Typedef.Tkind_tuple
                  [
                    {
                      Typedef.parameters = [];
                      body = Typedef.Tkind_concrete "String";
                    };
                    {
                      Typedef.parameters = [];
                      body = Typedef.Tkind_concrete "Int";
                    };
                  ];
            };
          params = [];
          name = "User";
        };
    ]
  in
  let input = "type alias User = (String, Int)" in
  let result = Main.parse (Lexing.from_string input) in
  assert_equal expect_data result

let test_ty_alias_and_record_and_plain _ =
  let expect_data =
    [
      Impl.Type_alias
        {
          Typealias.typedef =
            {
              Typedef.parameters =
                [
                  {
                    Typedef.parameters = [];
                    body =
                      Typedef.Tkind_tuple
                        [
                          {
                            Typedef.parameters = [];
                            body = Typedef.Tkind_concrete "String";
                          };
                          {
                            Typedef.parameters = [];
                            body =
                              Typedef.Tkind_record
                                {
                                  Typedef.values =
                                    [
                                      {
                                        Typedef.name = "age";
                                        body =
                                          {
                                            Typedef.parameters = [];
                                            body = Typedef.Tkind_concrete "Int";
                                          };
                                      };
                                      {
                                        Typedef.name = "dog";
                                        body =
                                          {
                                            Typedef.parameters =
                                              [
                                                {
                                                  Typedef.parameters = [];
                                                  body =
                                                    Typedef.Tkind_record
                                                      {
                                                        Typedef.values =
                                                          [
                                                            {
                                                              Typedef.name =
                                                                "dogName";
                                                              body =
                                                                {
                                                                  Typedef
                                                                  .parameters =
                                                                    [];
                                                                  body =
                                                                    Typedef
                                                                    .Tkind_concrete
                                                                      "String";
                                                                };
                                                            };
                                                          ];
                                                        row_type = None;
                                                      };
                                                };
                                              ];
                                            body =
                                              Typedef.Tkind_concrete "Maybe";
                                          };
                                      };
                                    ];
                                  row_type = None;
                                };
                          };
                        ];
                  };
                ];
              body = Typedef.Tkind_concrete "Maybe";
            };
          params = [];
          name = "MaybeUser";
        };
    ]
  in
  let input =
    "type alias MaybeUser = Maybe (String, { age: Int, dog: Maybe {dogName: \
     String} })"
  in
  let result = Main.parse (Lexing.from_string input) in
  assert_equal expect_data result

let test_ty_alias_and_record_and_plain_and_one_more_tuple _ =
  let expect_data =
    [
      Impl.Type_alias
        {
          Typealias.typedef =
            {
              Typedef.parameters =
                [
                  {
                    Typedef.parameters = [];
                    body =
                      Typedef.Tkind_tuple
                        [
                          {
                            Typedef.parameters = [];
                            body = Typedef.Tkind_concrete "String";
                          };
                          {
                            Typedef.parameters = [];
                            body =
                              Typedef.Tkind_record
                                {
                                  Typedef.values =
                                    [
                                      {
                                        Typedef.name = "age";
                                        body =
                                          {
                                            Typedef.parameters = [];
                                            body = Typedef.Tkind_concrete "Int";
                                          };
                                      };
                                      {
                                        Typedef.name = "dog";
                                        body =
                                          {
                                            Typedef.parameters =
                                              [
                                                {
                                                  Typedef.parameters = [];
                                                  body =
                                                    Typedef.Tkind_record
                                                      {
                                                        Typedef.values =
                                                          [
                                                            {
                                                              Typedef.name =
                                                                "dogName";
                                                              body =
                                                                {
                                                                  Typedef
                                                                  .parameters =
                                                                    [];
                                                                  body =
                                                                    Typedef
                                                                    .Tkind_tuple
                                                                      [
                                                                        {
                                                                          Typedef
                                                                          .parameters =
                                                                            [];
                                                                          body =
                                                                            Typedef
                                                                            .Tkind_concrete
                                                                              "String";
                                                                        };
                                                                        {
                                                                          Typedef
                                                                          .parameters =
                                                                            [];
                                                                          body =
                                                                            Typedef
                                                                            .Tkind_concrete
                                                                              "Int";
                                                                        };
                                                                        {
                                                                          Typedef
                                                                          .parameters =
                                                                            [];
                                                                          body =
                                                                            Typedef
                                                                            .Tkind_concrete
                                                                              "Int";
                                                                        };
                                                                      ];
                                                                };
                                                            };
                                                          ];
                                                        row_type = None;
                                                      };
                                                };
                                              ];
                                            body =
                                              Typedef.Tkind_concrete "Maybe";
                                          };
                                      };
                                    ];
                                  row_type = None;
                                };
                          };
                        ];
                  };
                ];
              body = Typedef.Tkind_concrete "Maybe";
            };
          params = [];
          name = "MaybeUser";
        };
    ]
  in
  let input =
    "type alias MaybeUser = Maybe (String, { age: Int, dog: Maybe {dogName: \
     (String, Int, Int)} })"
  in
  let result = Main.parse (Lexing.from_string input) in
  assert_equal expect_data result

let test_ty_alias_record_with_params_row_type _ =
  let expect_data =
    [
      Impl.Type_alias
        {
          Typealias.typedef =
            {
              Typedef.parameters = [];
              body =
                Typedef.Tkind_record
                  {
                    Typedef.values =
                      [
                        {
                          Typedef.name = "fieldN";
                          body =
                            {
                              Typedef.parameters = [];
                              body = Typedef.Tkind_concrete "String";
                            };
                        };
                      ];
                    row_type = Some "a";
                  };
            };
          params = [ "a" ];
          name = "User";
        };
    ]
  in
  let input = "type alias User a = { a | fieldN: String }" in
  let result = Main.parse (Lexing.from_string input) in
  assert_equal expect_data result

let test_function_types _ =
  let expect_data =
    [
      Impl.Type_alias
        {
          Typealias.typedef =
            {
              Typedef.parameters = [];
              body =
                Typedef.Tkind_function
                  {
                    Typedef.arguments =
                      [
                        {
                          Typedef.parameters = [];
                          body = Typedef.Tkind_var "a";
                        };
                        {
                          Typedef.parameters = [];
                          body = Typedef.Tkind_var "a";
                        };
                      ];
                  };
            };
          params = [ "a" ];
          name = "Id";
        };
    ]
  in
  let input = "type alias Id a = a -> a" in
  let result = Main.parse (Lexing.from_string input) in
  assert_equal expect_data result

let test_function_with_function_param_types _ =
  let expect_data =
    [
      Impl.Type_alias
        {
          Typealias.typedef =
            {
              Typedef.parameters = [];
              body =
                Typedef.Tkind_function
                  {
                    Typedef.arguments =
                      [
                        {
                          Typedef.parameters = [];
                          body = Typedef.Tkind_concrete "String";
                        };
                        {
                          Typedef.parameters = [];
                          body = Typedef.Tkind_concrete "Int";
                        };
                        {
                          Typedef.parameters = [];
                          body =
                            Typedef.Tkind_tuple
                              [
                                {
                                  Typedef.parameters = [];
                                  body = Typedef.Tkind_var "a";
                                };
                                {
                                  Typedef.parameters = [];
                                  body = Typedef.Tkind_var "b";
                                };
                              ];
                        };
                        {
                          Typedef.parameters = [];
                          body =
                            Typedef.Tkind_function
                              {
                                Typedef.arguments =
                                  [
                                    {
                                      Typedef.parameters = [];
                                      body = Typedef.Tkind_concrete "String";
                                    };
                                    {
                                      Typedef.parameters = [];
                                      body = Typedef.Tkind_var "a";
                                    };
                                    {
                                      Typedef.parameters = [];
                                      body =
                                        Typedef.Tkind_tuple
                                          [
                                            {
                                              Typedef.parameters = [];
                                              body = Typedef.Tkind_var "b";
                                            };
                                            {
                                              Typedef.parameters = [];
                                              body = Typedef.Tkind_var "b";
                                            };
                                            {
                                              Typedef.parameters = [];
                                              body = Typedef.Tkind_var "c";
                                            };
                                          ];
                                    };
                                  ];
                              };
                        };
                      ];
                  };
            };
          params = [ "a"; "b"; "c" ];
          name = "FunName";
        };
    ]
  in
  let input =
    "type alias FunName a b c = String -> Int -> (a, b) -> (String -> a -> (b, \
     b, c))"
  in
  let result = Main.parse (Lexing.from_string input) in
  assert_equal expect_data result

let test_any_fail_if_type_parametr_is_uppecase _ =
  let input = "type alias Id A = A -> A" in
  let res =
    try
      let _ = Main.parse (Lexing.from_string input) in
      true
    with _ -> false
  in
  assert_equal false res

let test_any_fail_if_ty_alias_name_is_lowercase _ =
  let input = "type alias anyName a = a -> a" in
  let res =
    try
      let _ = Main.parse (Lexing.from_string input) in
      true
    with _ -> false
  in
  assert_equal false res

let test_any_fail_if_any_char_in_middle_of_valid_code _ =
  let input = "type alias ValidName - a = a -> a" in
  let res =
    try
      let _ = Main.parse (Lexing.from_string input) in
      true
    with _ -> false
  in
  assert_equal false res

let test_fun_no_lrbraces _ =
  let input = "type alias Fun1 a = (a -> a, { a| field: a -> a })" in
  let expect =
    [
      Impl.Type_alias
        {
          Typealias.typedef =
            {
              Typedef.parameters = [];
              body =
                Typedef.Tkind_tuple
                  [
                    {
                      Typedef.parameters = [];
                      body =
                        Typedef.Tkind_function
                          {
                            Typedef.arguments =
                              [
                                {
                                  Typedef.parameters = [];
                                  body = Typedef.Tkind_var "a";
                                };
                                {
                                  Typedef.parameters = [];
                                  body = Typedef.Tkind_var "a";
                                };
                              ];
                          };
                    };
                    {
                      Typedef.parameters = [];
                      body =
                        Typedef.Tkind_record
                          {
                            Typedef.values =
                              [
                                {
                                  Typedef.name = "field";
                                  body =
                                    {
                                      Typedef.parameters = [];
                                      body =
                                        Typedef.Tkind_function
                                          {
                                            Typedef.arguments =
                                              [
                                                {
                                                  Typedef.parameters = [];
                                                  body = Typedef.Tkind_var "a";
                                                };
                                                {
                                                  Typedef.parameters = [];
                                                  body = Typedef.Tkind_var "a";
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
          params = [ "a" ];
          name = "Fun1";
        };
    ]
  in
  let result = Main.parse (Lexing.from_string input) in
  assert_equal expect result

let suite =
  [
    "test_ty_alias_record" >:: test_ty_alias_record;
    (* "test_ty_alias_type_with_params" >:: test_ty_alias_type_with_params;
       "test_ty_alias_type_with_params_and_record_param"
       >:: test_ty_alias_type_with_params_and_record_param;
       "test_ty_alias_type_with_params_and_record_param_with_a_lot_of_parens"
       >:: test_ty_alias_type_with_params_and_record_param_with_a_lot_of_parens;
       "test_ty_alias_tuples" >:: test_ty_alias_tuples;
       "test_ty_alias_and_record_and_Concrete"
       >:: test_ty_alias_and_record_and_plain;
       "test_ty_alias_and_record_and_plain_and_one_more_tuple"
       >:: test_ty_alias_and_record_and_plain_and_one_more_tuple;
       "test_ty_alias_record_with_params_row_type"
       >:: test_ty_alias_record_with_params_row_type;
       "test_function_types" >:: test_function_types;
       "test_function_with_function_param_types"
       >:: test_function_with_function_param_types;
       "test_fail_if_type_parametr_is_uppecase"
       >:: test_any_fail_if_type_parametr_is_uppecase;
       "test_any_fail_if_ty_alias_name_is_lowercase"
       >:: test_any_fail_if_ty_alias_name_is_lowercase;
       "test_any_fail_if_any_char_in_middle_of_valid_code"
       >:: test_any_fail_if_any_char_in_middle_of_valid_code;
       "test_fun_no_lrbraces" >:: test_fun_no_lrbraces; *)
  ]
