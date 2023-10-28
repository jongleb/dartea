open OUnit2
open Dartea
open Ast

let test_ty_alias_record _ =
  let expect_data = [Type_alias({
    id="User";
    params=[];
    data={
      content=Record({
        values=[
          {key="name"; value={
            content=Concrete("String");
            params=[]
          }};
          {key="age"; value={
            content=Concrete("Int");
            params=[]
          }}
        ];
        row_type=None;
      });
    params=[];
    }
  })] in
  let input = "type alias User = { name: String, age: Int }" in
  let result = Parser.prog Lexer.token (Lexing.from_string input) in
  assert_equal expect_data result

let test_ty_alias_type_with_params _ =
    let expect_data = [Type_alias({
      id="Complicated";
      data={
        content=Concrete("With");
        params=[
          {
            content=Concrete("Param1");
            params=[]
          };
          {
            content=Concrete("Param2");
            params=[
              {
                content=Concrete("Param3");
                params=[]
              };
              {
                content=Concrete("Param4");
                params=[
                  {
                    content=Concrete("Param5");
                    params=[]
                  }
                ]
              }
            ]
          }
        ];
      };
      params=[];
    })] in
    let input = "type alias Complicated = With Param1 (Param2 Param3 (Param4 Param5))" in
    let result = Parser.prog Lexer.token (Lexing.from_string input) in
    assert_equal expect_data result


let test_ty_alias_type_with_params_and_record_param _ =
      let expect_data = [Type_alias({
        id="Complicated";
        data={
          content=Concrete("With");
          params=[
            {
              content=Concrete("Param1");
              params=[]
            };
            {
              content=Concrete("Param2");
              params=[
                {
                  content=Record({
                    values=[
                      {key="a"; value={
                        content=Concrete("String");
                        params=[]
                      }};
                    ];
                    row_type=None;
                  });
                params=[];
                }
              ]
            }
          ];
        };
        params=[];
      })] in
      let input = "type alias Complicated = With Param1 (Param2 {a: String})" in
      let result = Parser.prog Lexer.token (Lexing.from_string input) in
      assert_equal expect_data result

  
      let test_ty_alias_type_with_params_and_record_param_with_a_lot_of_parens _ =
        let expect_data = [Type_alias({
          id="Complicated";
          data={
            content=Concrete("With");
            params=[
              {
                content=Concrete("Param1");
                params=[]
              };
              {
                content=Concrete("Param2");
                params=[
                  {
                    content=Record({
                      values=[
                        {key="a"; value={
                          content=Concrete("String");
                          params=[]
                        }};
                      ];
                      row_type=None;
                    });
                  params=[];
                  }
                ]
              }
            ];
          };
          params=[];
        })] in
        let input = "type alias Complicated = With ((Param1)) (Param2 {a: ((((((String))))))})" in
        let result = Parser.prog Lexer.token (Lexing.from_string input) in
        assert_equal expect_data result

let test_ty_alias_tuples _ =
          let expect_data = [Type_alias({
            id="User";
            data={
              content=Tuples([
                {
                  content=Concrete("String");
                  params=[]
                };
                {
                  content=Concrete("Int");
                  params=[]
                }
              ]);
              params=[];
            };
            params=[];
          })] in
          let input = "type alias User = (String, Int)" in
          let result = Parser.prog Lexer.token (Lexing.from_string input) in
          assert_equal expect_data result

let test_ty_alias_and_record_and_plain _ =
            let expect_data = [Type_alias({
              id="MaybeUser";
              data={
                content=Concrete("Maybe");
                params=[
                  {
                    content=Tuples([
                      {
                        content=Concrete("String");
                        params=[]
                      };
                      {
                        content=Record({
                          values=[
                            {key="age"; value={
                              content=Concrete("Int");
                              params=[]
                            }};
                            {key="dog"; value={
                              content=Concrete("Maybe");
                              params=[
                                {
                                      content=Record(
                                        {
                                          values=[{key="dogName"; value={
                                            content=Concrete("String");
                                            params=[]
                                            }};];
                                            row_type=None;
                                        }
                                      );
                                      params=[];
                                    }
                              ]
                            }};
                          ];
                          row_type=None;
                        });
                        params=[];
                    }
                    ]);
                    params=[];
                  }
                ]
              };
              params=[];
            })] in
            let input = "type alias MaybeUser = Maybe (String, { age: Int, dog: Maybe {dogName: String} })" in
            let result = Parser.prog Lexer.token (Lexing.from_string input) in
            assert_equal expect_data result

let test_ty_alias_and_record_and_plain_and_one_more_tuple _ =
              let expect_data = [Type_alias({
                id="MaybeUser";
                data={
                  content=Concrete("Maybe");
                  params=[
                    {
                      content=Tuples([
                        {
                          content=Concrete("String");
                          params=[]
                        };
                        {
                          content=Record({
                            values=[
                              {key="age"; value={
                                content=Concrete("Int");
                                params=[]
                              }};
                              {key="dog"; value={
                                content=Concrete("Maybe");
                                params=[
                                  {
                                        content=Record({
                                          row_type=None;
                                          values=[{key="dogName"; value={
                                            content=Tuples([
                                              {
                                            content=Concrete("String");
                                            params=[]
                                            };
                                            {
                                            content=Concrete("Int");
                                            params=[]
                                            };
                                            {
                                            content=Concrete("Int");
                                            params=[]
                                            };
                                            ]);
                                            params=[]
                                            }};];
                                        });
                                        params=[];
                                      }
                                ]
                              }};
                            ];
                            row_type=None;
                          });
                          params=[];
                      }
                      ]);
                      params=[];
                    }
                  ]
                };
                params=[];
              })] in
              let input = "type alias MaybeUser = Maybe (String, { age: Int, dog: Maybe {dogName: (String, Int, Int)} })" in
              let result = Parser.prog Lexer.token (Lexing.from_string input) in
              assert_equal expect_data result

let test_ty_alias_record_with_params_row_type _ =
                let expect_data = [Type_alias({
                  id="User";
                  params=["a"];
                  data={
                    content=Record({
                      values=[
                        {key="fieldN"; value={
                          content=Concrete("String");
                          params=[]
                        }};
                      ];
                      row_type=Some("a");
                    });
                  params=[];
                  }
                })] in
                let input = "type alias User a = { a | fieldN: String }" in
                let result = Parser.prog Lexer.token (Lexing.from_string input) in
                assert_equal expect_data result

let suite =
  "Dartea_test" >::: [
    "test_ty_alias_record" >:: test_ty_alias_record;
    "test_ty_alias_type_with_params" >:: test_ty_alias_type_with_params;
    "test_ty_alias_type_with_params_and_record_param" >:: test_ty_alias_type_with_params_and_record_param;
    "test_ty_alias_type_with_params_and_record_param_with_a_lot_of_parens" >:: test_ty_alias_type_with_params_and_record_param_with_a_lot_of_parens;
    "test_ty_alias_tuples" >:: test_ty_alias_tuples;
    "test_ty_alias_and_record_and_Concrete" >:: test_ty_alias_and_record_and_plain;
    "test_ty_alias_and_record_and_plain_and_one_more_tuple" >:: test_ty_alias_and_record_and_plain_and_one_more_tuple;
    "test_ty_alias_record_with_params_row_type" >:: test_ty_alias_record_with_params_row_type;
  ]

let () =
  run_test_tt_main suite


