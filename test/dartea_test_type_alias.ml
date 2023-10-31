open OUnit2
open Dartea
open Ast

let test_ty_alias_record _ =
  let expect_data =
    [
      Type_alias
        {
          id = "User";
          params = [];
          data =
            {
              content =
                Record
                  {
                    values =
                      [
                        {
                          key = "name";
                          value = { content = Concrete "String"; params = [] };
                        };
                        {
                          key = "age";
                          value = { content = Concrete "Int"; params = [] };
                        };
                      ];
                    row_type = None;
                  };
              params = [];
            };
        };
    ]
  in
  let input = "type alias User = { name: String, age: Int }" in
  let result = Parser.prog Lexer.token (Lexing.from_string input) in
  assert_equal expect_data result

let test_ty_alias_type_with_params _ =
  let expect_data =
    [
      Type_alias
        {
          id = "Complicated";
          data =
            {
              content = Concrete "With";
              params =
                [
                  { content = Concrete "Param1"; params = [] };
                  {
                    content = Concrete "Param2";
                    params =
                      [
                        { content = Concrete "Param3"; params = [] };
                        {
                          content = Concrete "Param4";
                          params =
                            [ { content = Concrete "Param5"; params = [] } ];
                        };
                      ];
                  };
                ];
            };
          params = [];
        };
    ]
  in
  let input =
    "type alias Complicated = With Param1 (Param2 Param3 (Param4 Param5))"
  in
  let result = Parser.prog Lexer.token (Lexing.from_string input) in
  assert_equal expect_data result

let test_ty_alias_type_with_params_and_record_param _ =
  let expect_data =
    [
      Type_alias
        {
          id = "Complicated";
          data =
            {
              content = Concrete "With";
              params =
                [
                  { content = Concrete "Param1"; params = [] };
                  {
                    content = Concrete "Param2";
                    params =
                      [
                        {
                          content =
                            Record
                              {
                                values =
                                  [
                                    {
                                      key = "a";
                                      value =
                                        {
                                          content = Concrete "String";
                                          params = [];
                                        };
                                    };
                                  ];
                                row_type = None;
                              };
                          params = [];
                        };
                      ];
                  };
                ];
            };
          params = [];
        };
    ]
  in
  let input = "type alias Complicated = With Param1 (Param2 {a: String})" in
  let result = Parser.prog Lexer.token (Lexing.from_string input) in
  assert_equal expect_data result

let test_ty_alias_type_with_params_and_record_param_with_a_lot_of_parens _ =
  let expect_data =
    [
      Type_alias
        {
          id = "Complicated";
          data =
            {
              content = Concrete "With";
              params =
                [
                  { content = Concrete "Param1"; params = [] };
                  {
                    content = Concrete "Param2";
                    params =
                      [
                        {
                          content =
                            Record
                              {
                                values =
                                  [
                                    {
                                      key = "a";
                                      value =
                                        {
                                          content = Concrete "String";
                                          params = [];
                                        };
                                    };
                                  ];
                                row_type = None;
                              };
                          params = [];
                        };
                      ];
                  };
                ];
            };
          params = [];
        };
    ]
  in
  let input =
    "type alias Complicated = With ((Param1)) (Param2 {a: ((((((String))))))})"
  in
  let result = Parser.prog Lexer.token (Lexing.from_string input) in
  assert_equal expect_data result

let test_ty_alias_tuples _ =
  let expect_data =
    [
      Type_alias
        {
          id = "User";
          data =
            {
              content =
                Tuples
                  [
                    { content = Concrete "String"; params = [] };
                    { content = Concrete "Int"; params = [] };
                  ];
              params = [];
            };
          params = [];
        };
    ]
  in
  let input = "type alias User = (String, Int)" in
  let result = Parser.prog Lexer.token (Lexing.from_string input) in
  assert_equal expect_data result

let test_ty_alias_and_record_and_plain _ =
  let expect_data =
    [
      Type_alias
        {
          id = "MaybeUser";
          data =
            {
              content = Concrete "Maybe";
              params =
                [
                  {
                    content =
                      Tuples
                        [
                          { content = Concrete "String"; params = [] };
                          {
                            content =
                              Record
                                {
                                  values =
                                    [
                                      {
                                        key = "age";
                                        value =
                                          {
                                            content = Concrete "Int";
                                            params = [];
                                          };
                                      };
                                      {
                                        key = "dog";
                                        value =
                                          {
                                            content = Concrete "Maybe";
                                            params =
                                              [
                                                {
                                                  content =
                                                    Record
                                                      {
                                                        values =
                                                          [
                                                            {
                                                              key = "dogName";
                                                              value =
                                                                {
                                                                  content =
                                                                    Concrete
                                                                      "String";
                                                                  params = [];
                                                                };
                                                            };
                                                          ];
                                                        row_type = None;
                                                      };
                                                  params = [];
                                                };
                                              ];
                                          };
                                      };
                                    ];
                                  row_type = None;
                                };
                            params = [];
                          };
                        ];
                    params = [];
                  };
                ];
            };
          params = [];
        };
    ]
  in
  let input =
    "type alias MaybeUser = Maybe (String, { age: Int, dog: Maybe {dogName: \
     String} })"
  in
  let result = Parser.prog Lexer.token (Lexing.from_string input) in
  assert_equal expect_data result

let test_ty_alias_and_record_and_plain_and_one_more_tuple _ =
  let expect_data =
    [
      Type_alias
        {
          id = "MaybeUser";
          data =
            {
              content = Concrete "Maybe";
              params =
                [
                  {
                    content =
                      Tuples
                        [
                          { content = Concrete "String"; params = [] };
                          {
                            content =
                              Record
                                {
                                  values =
                                    [
                                      {
                                        key = "age";
                                        value =
                                          {
                                            content = Concrete "Int";
                                            params = [];
                                          };
                                      };
                                      {
                                        key = "dog";
                                        value =
                                          {
                                            content = Concrete "Maybe";
                                            params =
                                              [
                                                {
                                                  content =
                                                    Record
                                                      {
                                                        row_type = None;
                                                        values =
                                                          [
                                                            {
                                                              key = "dogName";
                                                              value =
                                                                {
                                                                  content =
                                                                    Tuples
                                                                      [
                                                                        {
                                                                          content =
                                                                            Concrete
                                                                              "String";
                                                                          params =
                                                                            [];
                                                                        };
                                                                        {
                                                                          content =
                                                                            Concrete
                                                                              "Int";
                                                                          params =
                                                                            [];
                                                                        };
                                                                        {
                                                                          content =
                                                                            Concrete
                                                                              "Int";
                                                                          params =
                                                                            [];
                                                                        };
                                                                      ];
                                                                  params = [];
                                                                };
                                                            };
                                                          ];
                                                      };
                                                  params = [];
                                                };
                                              ];
                                          };
                                      };
                                    ];
                                  row_type = None;
                                };
                            params = [];
                          };
                        ];
                    params = [];
                  };
                ];
            };
          params = [];
        };
    ]
  in
  let input =
    "type alias MaybeUser = Maybe (String, { age: Int, dog: Maybe {dogName: \
     (String, Int, Int)} })"
  in
  let result = Parser.prog Lexer.token (Lexing.from_string input) in
  assert_equal expect_data result

let test_ty_alias_record_with_params_row_type _ =
  let expect_data =
    [
      Type_alias
        {
          id = "User";
          params = [ "a" ];
          data =
            {
              content =
                Record
                  {
                    values =
                      [
                        {
                          key = "fieldN";
                          value = { content = Concrete "String"; params = [] };
                        };
                      ];
                    row_type = Some "a";
                  };
              params = [];
            };
        };
    ]
  in
  let input = "type alias User a = { a | fieldN: String }" in
  let result = Parser.prog Lexer.token (Lexing.from_string input) in
  assert_equal expect_data result

let test_function_types _ =
  let expect_data =
    [
      Type_alias
        {
          data =
            {
              params = [];
              content =
                Function
                  {
                    arguments =
                      [
                        { params = []; content = Type_var "a" };
                        { params = []; content = Type_var "a" };
                      ];
                  };
            };
          params = [ "a" ];
          id = "Id";
        };
    ]
  in
  let input = "type alias Id a = a -> a" in
  let result = Parser.prog Lexer.token (Lexing.from_string input) in
  assert_equal expect_data result

let test_function_with_function_param_types _ =
  let expect_data =
    [
      Type_alias
        {
          data =
            {
              params = [];
              content =
                Function
                  {
                    arguments =
                      [
                        { params = []; content = Concrete "String" };
                        { params = []; content = Concrete "Int" };
                        {
                          params = [];
                          content =
                            Tuples
                              [
                                { params = []; content = Type_var "a" };
                                { params = []; content = Type_var "b" };
                              ];
                        };
                        {
                          params = [];
                          content =
                            Function
                              {
                                arguments =
                                  [
                                    { params = []; content = Concrete "String" };
                                    { params = []; content = Type_var "a" };
                                    {
                                      params = [];
                                      content =
                                        Tuples
                                          [
                                            {
                                              params = [];
                                              content = Type_var "b";
                                            };
                                            {
                                              params = [];
                                              content = Type_var "b";
                                            };
                                            {
                                              params = [];
                                              content = Type_var "c";
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
          id = "FunName";
        };
    ]
  in
  let input =
    "type alias FunName a b c = String -> Int -> (a, b) -> (String -> a -> (b, \
     b, c))"
  in
  let result = Parser.prog Lexer.token (Lexing.from_string input) in
  assert_equal expect_data result

let test_any_fail_if_type_parametr_is_uppecase _ =
  let input = "type alias Id A = A -> A" in
  let res =
    try
      let _ = Parser.prog Lexer.token (Lexing.from_string input) in
      true
    with _ -> false
  in
  assert_equal false res

let test_any_fail_if_ty_alias_name_is_lowercase _ =
  let input = "type alias anyName a = a -> a" in
  let res =
    try
      let _ = Parser.prog Lexer.token (Lexing.from_string input) in
      true
    with _ -> false
  in
  assert_equal false res

let test_any_fail_if_any_char_in_middle_of_valid_code _ =
  let input = "type alias ValidName - a = a -> a" in
  let res =
    try
      let _ = Parser.prog Lexer.token (Lexing.from_string input) in
      true
    with _ -> false
  in
  assert_equal false res

let test_fun_no_lrbraces _ =
  let input = "type alias Fun1 a = (a -> a, { a| field: a -> a })" in
  let expect =
    [
      Ast.Type_alias
        {
          Ast.data =
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
                                };
                              ];
                            row_type = Some "a";
                          };
                    };
                  ];
            };
          params = [ "a" ];
          id = "Fun1";
        };
    ]
  in
  let result = Parser.prog Lexer.token (Lexing.from_string input) in
  assert_equal expect result

let suite =
  [
    "test_ty_alias_record" >:: test_ty_alias_record;
    "test_ty_alias_type_with_params" >:: test_ty_alias_type_with_params;
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
    "test_fun_no_lrbraces" >:: test_fun_no_lrbraces;
  ]
