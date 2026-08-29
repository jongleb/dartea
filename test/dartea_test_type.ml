open OUnit2
open Frontend
open Data
open Located
module Main = Parse.Main

let test_ty_type_with_params_complicated _ =
  let expect_data =
    [
      Impl.Type_dec
        {
          Typedecl.name = ~?"Complicated";
          ctors =
            [
              {
                Typedecl.id = ~?"Constr1";
                data =
                  [
                    Utils.var_type "a";
                    Utils.fn_type
                      ~arguments:[ Utils.var_type "a" ]
                      ~result:(Utils.var_type "a");
                    Utils.record_type
                      ~row_type:(~?"a")
                      [
                        {
                          Typedef.Type_record_row.name = ~?"field";
                          body =
                            Utils.fn_type
                              ~arguments:
                                [
                                  Utils.concrete_type
                                    ~parameters:[ Utils.var_type "a" ]
                                    "Maybe";
                                ]
                              ~result:(Utils.concrete_type "String");
                        };
                      ];
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
  let result = Main.parse ~file:"Main.elm" input in
  assert_equal expect_data (Utils.dummified result)

(** I am bothered thinking up the names of these functions. *)

let test_types_fn_2 _ =
  let expect_data =
    [
      Impl.Type_dec
        {
          Typedecl.name = ~?"Complicated";
          ctors =
            [
              {
                Typedecl.id = ~?"Constr1";
                data =
                  [
                    Utils.var_type "a";
                    Utils.fn_type
                      ~arguments:[ Utils.var_type "a" ]
                      ~result:(Utils.var_type "a");
                    Utils.record_type
                      ~row_type:(~?"a")
                      [
                        {
                          Typedef.Type_record_row.name = ~?"field";
                          body =
                            Utils.fn_type
                              ~arguments:
                                [
                                  Utils.concrete_type
                                    ~parameters:[ Utils.var_type "a" ]
                                    "Maybe";
                                ]
                              ~result:(Utils.concrete_type "String");
                        };
                      ];
                  ];
              };
              {
                Typedecl.id = ~?"Constr2";
                data =
                  [
                    Utils.fn_type
                      ~arguments:[ Utils.var_type "a"; Utils.var_type "a" ]
                      ~result:(Utils.concrete_type "String");
                    Utils.make_typedef
                      ~body:
                        (Typedef.Kind.Tkind_tuple
                           [
                             Utils.concrete_type "String";
                             Utils.fn_type
                               ~arguments:[ Utils.var_type "a" ]
                               ~result:(Utils.var_type "a");
                           ])
                      ();
                    Utils.concrete_type "String";
                    Utils.record_type
                      ~row_type:(~?"a")
                      [
                        {
                          Typedef.Type_record_row.name = ~?"field";
                          body =
                            Utils.make_typedef
                              ~body:
                                (Typedef.Kind.Tkind_tuple
                                   [
                                     Utils.fn_type
                                       ~arguments:
                                         [
                                           Utils.concrete_type
                                             ~parameters:[ Utils.var_type "a" ]
                                             "Maybe";
                                         ]
                                       ~result:(Utils.concrete_type "String");
                                     Utils.var_type "a";
                                     Utils.fn_type
                                       ~arguments:[ Utils.var_type "a" ]
                                       ~result:(Utils.var_type "a");
                                   ])
                              ();
                        };
                      ];
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
  let result = Main.parse ~file:"Main.elm" input in
  assert_equal expect_data (Utils.dummified result)

let suite =
  [
    "test_ty_type_with_params_complicated"
    >:: test_ty_type_with_params_complicated;
    "test_types_fn_2" >:: test_types_fn_2;
  ]
