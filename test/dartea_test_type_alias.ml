open OUnit2
open Ast.Kind.Frontend
module Main = Parse.Main

let parsed result =
  Result.get_ok result |> List.map Utils.dummify_all_locs

let test_ty_alias_record _ =
  let typedef =
    Typedef.(
      let name =
        let name_body = Utils.make_tkind ~name:"String" in
        let name_impl = Utils.make_typedef ~body:name_body () in
        let name = Data.Located.dummy "name" in
        Type_record_row.Fields.create ~name ~body:name_impl
      in

      let age =
        let age_body = Utils.make_tkind ~name:"Int" in
        let age_impl = Utils.make_typedef ~body:age_body () in
        let name = Data.Located.dummy "age" in
        Type_record_row.Fields.create ~name ~body:age_impl
      in

      let type_record =
        Type_record.Fields.create ~values:[ name; age ] ~row_type:None
      in
      let tkind_record = Kind.Tkind_record type_record in
      Impl.Fields.create ~parameters:[] ~body:tkind_record)
  in

  let typealias =
    Typealias.Fields.create ~typedef ~params:[]
      ~name:(Data.Located.dummy "User")
  in

  let top = Impl.Type_alias typealias in

  let expect_data = [ top ] in
  let input = "type alias User = { name: String, age: Int }" in
  let result = Main.parse ~file:"Main.elm" input in
  assert_equal expect_data (parsed result)

let test_ty_alias_type_with_params _ =
  let expect_data =
    [
      Impl.Type_alias
        {
          Typealias.typedef =
            Utils.concrete_type
              ~parameters:
                [
                  Utils.concrete_type "Param1";
                  Utils.concrete_type
                    ~parameters:
                      [
                        Utils.concrete_type "Param3";
                        Utils.concrete_type
                          ~parameters:[ Utils.concrete_type "Param5" ]
                          "Param4";
                      ]
                    "Param2";
                ]
              "With";
          params = [];
          name = Data.Located.dummy "Complicated";
        };
    ]
  in
  let input =
    "type alias Complicated = With Param1 (Param2 Param3 (Param4 Param5))"
  in
  let result =
    parsed (Main.parse ~file:"Main.elm" input)
  in
  assert_equal expect_data result

let test_ty_alias_type_with_params_and_record_param _ =
  let expect_data =
    [
      Impl.Type_alias
        {
          Typealias.typedef =
            Utils.concrete_type
              ~parameters:
                [
                  Utils.concrete_type "Param1";
                  Utils.concrete_type
                    ~parameters:
                      [
                        Utils.record_type
                          [
                            {
                              Typedef.Type_record_row.name =
                                Data.Located.dummy "a";
                              body = Utils.concrete_type "String";
                            };
                          ];
                      ]
                    "Param2";
                ]
              "With";
          params = [];
          name = Data.Located.dummy "Complicated";
        };
    ]
  in
  let input = "type alias Complicated = With Param1 (Param2 {a: String})" in
  let result = Main.parse ~file:"Main.elm" input in
  assert_equal expect_data (parsed result)

let test_ty_alias_type_with_params_and_record_param_with_a_lot_of_parens _ =
  let expect_data =
    [
      Impl.Type_alias
        {
          Typealias.typedef =
            Utils.concrete_type
              ~parameters:
                [
                  Utils.concrete_type "Param1";
                  Utils.concrete_type
                    ~parameters:
                      [
                        Utils.record_type
                          [
                            {
                              Typedef.Type_record_row.name =
                                Data.Located.dummy "a";
                              body = Utils.concrete_type "String";
                            };
                          ];
                      ]
                    "Param2";
                ]
              "With";
          params = [];
          name = Data.Located.dummy "Complicated";
        };
    ]
  in
  let input =
    "type alias Complicated = With ((Param1)) (Param2 {a: ((((((String))))))})"
  in
  let result = Main.parse ~file:"Main.elm" input in
  assert_equal expect_data (parsed result)

let test_ty_alias_tuples _ =
  let expect_data =
    [
      Impl.Type_alias
        {
          Typealias.typedef =
            Utils.make_typedef
              ~body:
                (Typedef.Kind.Tkind_tuple
                   [ Utils.concrete_type "String"; Utils.concrete_type "Int" ])
              ();
          params = [];
          name = Data.Located.dummy "User";
        };
    ]
  in
  let input = "type alias User = (String, Int)" in
  let result = Main.parse ~file:"Main.elm" input in
  assert_equal expect_data (parsed result)

let test_ty_alias_and_record_and_plain _ =
  let expect_data =
    [
      Impl.Type_alias
        {
          Typealias.typedef =
            Utils.concrete_type
              ~parameters:
                [
                  Utils.make_typedef
                    ~body:
                      (Typedef.Kind.Tkind_tuple
                         [
                           Utils.concrete_type "String";
                           Utils.record_type
                             [
                               {
                                 Typedef.Type_record_row.name =
                                   Data.Located.dummy "age";
                                 body = Utils.concrete_type "Int";
                               };
                               {
                                 Typedef.Type_record_row.name =
                                   Data.Located.dummy "dog";
                                 body =
                                   Utils.concrete_type
                                     ~parameters:
                                       [
                                         Utils.record_type
                                           [
                                             {
                                               Typedef.Type_record_row.name =
                                                 Data.Located.dummy "dogName";
                                               body =
                                                 Utils.concrete_type "String";
                                             };
                                           ];
                                       ]
                                     "Maybe";
                               };
                             ];
                         ])
                    ();
                ]
              "Maybe";
          params = [];
          name = Data.Located.dummy "MaybeUser";
        };
    ]
  in
  let input =
    "type alias MaybeUser = Maybe (String, { age: Int, dog: Maybe {dogName: \
     String} })"
  in
  let result = Main.parse ~file:"Main.elm" input in
  assert_equal expect_data (parsed result)

let test_ty_alias_and_record_and_plain_and_one_more_tuple _ =
  let expect_data =
    [
      Impl.Type_alias
        {
          Typealias.typedef =
            Utils.concrete_type
              ~parameters:
                [
                  Utils.make_typedef
                    ~body:
                      (Typedef.Kind.Tkind_tuple
                         [
                           Utils.concrete_type "String";
                           Utils.record_type
                             [
                               {
                                 Typedef.Type_record_row.name =
                                   Data.Located.dummy "age";
                                 body = Utils.concrete_type "Int";
                               };
                               {
                                 Typedef.Type_record_row.name =
                                   Data.Located.dummy "dog";
                                 body =
                                   Utils.concrete_type
                                     ~parameters:
                                       [
                                         Utils.record_type
                                           [
                                             {
                                               Typedef.Type_record_row.name =
                                                 Data.Located.dummy "dogName";
                                               body =
                                                 Utils.make_typedef
                                                   ~body:
                                                     (Typedef.Kind.Tkind_tuple
                                                        [
                                                          Utils.concrete_type
                                                            "String";
                                                          Utils.concrete_type
                                                            "Int";
                                                          Utils.concrete_type
                                                            "Int";
                                                        ])
                                                   ();
                                             };
                                           ];
                                       ]
                                     "Maybe";
                               };
                             ];
                         ])
                    ();
                ]
              "Maybe";
          params = [];
          name = Data.Located.dummy "MaybeUser";
        };
    ]
  in
  let input =
    "type alias MaybeUser = Maybe (String, { age: Int, dog: Maybe {dogName: \
     (String, Int, Int)} })"
  in
  let result = Main.parse ~file:"Main.elm" input in
  assert_equal expect_data (parsed result)

let test_ty_alias_record_with_params_row_type _ =
  let expect_data =
    [
      Impl.Type_alias
        {
          Typealias.typedef =
            Utils.record_type
              ~row_type:(Data.Located.dummy "a")
              [
                {
                  Typedef.Type_record_row.name = Data.Located.dummy "fieldN";
                  body = Utils.concrete_type "String";
                };
              ];
          params = [ Data.Located.dummy "a" ];
          name = Data.Located.dummy "User";
        };
    ]
  in
  let input = "type alias User a = { a | fieldN: String }" in
  let result = Main.parse ~file:"Main.elm" input in
  assert_equal expect_data (parsed result)

let test_function_types _ =
  let expect_data =
    [
      Impl.Type_alias
        {
          Typealias.typedef =
            Utils.fn_type
              ~arguments:[ Utils.var_type "a" ]
              ~result:(Utils.var_type "a");
          params = [ Data.Located.dummy "a" ];
          name = Data.Located.dummy "Id";
        };
    ]
  in
  let input = "type alias Id a = a -> a" in
  let result = Main.parse ~file:"Main.elm" input in
  assert_equal expect_data (parsed result)

let test_function_with_function_param_types _ =
  let expect_data =
    [
      Impl.Type_alias
        {
          Typealias.typedef =
            Utils.fn_type
              ~arguments:
                [
                  Utils.concrete_type "String";
                  Utils.concrete_type "Int";
                  Utils.make_typedef
                    ~body:
                      (Typedef.Kind.Tkind_tuple
                         [ Utils.var_type "a"; Utils.var_type "b" ])
                    ();
                ]
              ~result:
                (Utils.fn_type
                   ~arguments:
                     [ Utils.concrete_type "String"; Utils.var_type "a" ]
                   ~result:
                     (Utils.make_typedef
                        ~body:
                          (Typedef.Kind.Tkind_tuple
                             [
                               Utils.var_type "b";
                               Utils.var_type "b";
                               Utils.var_type "c";
                             ])
                        ()));
          params =
            [
              Data.Located.dummy "a";
              Data.Located.dummy "b";
              Data.Located.dummy "c";
            ];
          name = Data.Located.dummy "FunName";
        };
    ]
  in
  let input =
    "type alias FunName a b c = String -> Int -> (a, b) -> (String -> a -> (b, \
     b, c))"
  in
  let result = Main.parse ~file:"Main.elm" input in
  assert_equal expect_data (parsed result)

let test_any_fail_if_type_parametr_is_uppecase _ =
  let input = "type alias Id A = A -> A" in
  let res = Main.parse ~file:"Main.elm" input in
  assert_equal false (Result.is_ok res)

let test_any_fail_if_ty_alias_name_is_lowercase _ =
  let input = "type alias anyName a = a -> a" in
  let res = Main.parse ~file:"Main.elm" input in
  assert_equal false (Result.is_ok res)

let test_any_fail_if_any_char_in_middle_of_valid_code _ =
  let input = "type alias ValidName - a = a -> a" in
  let res = Main.parse ~file:"Main.elm" input in
  assert_equal false (Result.is_ok res)

let test_fun_no_lrbraces _ =
  let input = "type alias Fun1 a = (a -> a, { a| field: a -> a })" in
  let expect =
    [
      Impl.Type_alias
        {
          Typealias.typedef =
            Utils.make_typedef
              ~body:
                (Typedef.Kind.Tkind_tuple
                   [
                     Utils.fn_type
                       ~arguments:[ Utils.var_type "a" ]
                       ~result:(Utils.var_type "a");
                     Utils.record_type
                       ~row_type:(Data.Located.dummy "a")
                       [
                         {
                           Typedef.Type_record_row.name =
                             Data.Located.dummy "field";
                           body =
                             Utils.fn_type
                               ~arguments:[ Utils.var_type "a" ]
                               ~result:(Utils.var_type "a");
                         };
                       ];
                   ])
              ();
          params = [ Data.Located.dummy "a" ];
          name = Data.Located.dummy "Fun1";
        };
    ]
  in
  let result = Main.parse ~file:"Main.elm" input in
  assert_equal expect (parsed result)

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
