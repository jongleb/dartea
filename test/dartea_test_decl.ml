open OUnit2
open Dartea
open Ast

let test_decl_string _ =
  let expect_data =
    [
      Declaration
        {
          type_part_data =
            Some
              {
                decl_name = "thisIsTheString";
                type_alias = { content = Concrete "String"; params = [] };
              };
          body_part = { name = "thisIsTheString"; expr = String_constr "This" };
        };
    ]
  in
  let input = {|
thisIsTheString: String
thisIsTheString = "This"|} in
  let result = Main.parse (Lexing.from_string input) in
  assert_equal expect_data result

let test_let_in _ =
  let expect_data =
    [
      Ast.Declaration
        {
          Ast.type_part_data =
            Some
              {
                Ast.decl_name = "lol";
                type_alias = { Ast.params = []; content = Ast.Concrete "Kek" };
              };
          body_part =
            {
              Ast.name = "lol";
              expr =
                Ast.Let
                  {
                    let_expr_items =
                      [
                        {
                          body_part =
                            { Ast.name = "a"; body = Ast.Int_constr 2 };
                          type_part = None;
                        };
                      ];
                    in_ = Ast.Int_constr 2;
                  };
            };
        };
    ]
  in
  let input = {|
lol: Kek                            
lol = let a = 2 in 2|} in
  let result = Main.parse (Lexing.from_string input) in
  assert_equal expect_data result

let test_let_in_binop _ =
  let expect_data =
    [
      Ast.Declaration
        {
          Ast.type_part_data =
            Some
              {
                Ast.decl_name = "lol";
                type_alias = { Ast.params = []; content = Ast.Concrete "Kek" };
              };
          body_part =
            {
              Ast.name = "lol";
              expr =
                Let
                  {
                    let_expr_items =
                      [
                        {
                          body_part =
                            { Ast.name = "a"; body = Ast.Int_constr 2 };
                          type_part = None;
                        };
                      ];
                    in_ =
                      Ast.Binop
                        {
                          Ast.op_id = "+";
                          params = (Ast.Int_constr 2, Ast.Int_constr 3);
                        };
                  };
            };
        };
    ]
  in
  let input =
    {|
lol: Kek                            
lol = let a = 2 in 2 + 3|}
  in
  let result = Main.parse (Lexing.from_string input) in
  assert_equal expect_data result

let test_let_in_let_in_let _ =
  let expect_data =
    [
      Ast.Declaration
        {
          Ast.type_part_data =
            Some
              {
                Ast.decl_name = "kek";
                type_alias = { Ast.params = []; content = Ast.Concrete "Lol" };
              };
          body_part =
            {
              Ast.name = "kek";
              expr =
                Ast.Let
                  {
                    Ast.let_expr_items =
                      [
                        {
                          type_part = None;
                          body_part =
                            {
                              Ast.name = "a";
                              body =
                                Ast.Let
                                  {
                                    Ast.let_expr_items =
                                      [
                                        {
                                          type_part = None;
                                          body_part =
                                            {
                                              Ast.name = "b";
                                              body =
                                                Ast.Let
                                                  {
                                                    Ast.let_expr_items =
                                                      [
                                                        {
                                                          type_part = None;
                                                          body_part =
                                                            {
                                                              Ast.name = "c";
                                                              body =
                                                                Ast.Int_constr 3;
                                                            };
                                                        };
                                                      ];
                                                    in_ = Ast.Int_constr 3;
                                                  };
                                            };
                                        };
                                      ];
                                    in_ = Ast.Int_constr 3;
                                  };
                            };
                        };
                      ];
                    in_ = Ast.Int_constr 3;
                  };
            };
        };
    ]
  in
  let input =
    {|
kek: Lol                            
kek = let a = let b = let c = 3 in 3 in 3 in 3|}
  in
  let result = Main.parse (Lexing.from_string input) in
  assert_equal expect_data result

let test_math _ =
  let expect_data =
    [
      Ast.Declaration
        {
          Ast.type_part_data =
            Some
              {
                Ast.decl_name = "kek";
                type_alias = { Ast.params = []; content = Ast.Concrete "Int" };
              };
          body_part =
            {
              Ast.name = "kek";
              expr =
                Ast.Binop
                  {
                    Ast.op_id = "+";
                    params =
                      ( Ast.Int_constr 2,
                        Ast.Binop
                          {
                            Ast.op_id = "/";
                            params =
                              ( Ast.Binop
                                  {
                                    Ast.op_id = "*";
                                    params = (Ast.Int_constr 3, Ast.Int_constr 8);
                                  },
                                Ast.Int_constr 2 );
                          } );
                  };
            };
        };
    ]
  in
  let input = {|
kek: Int                            
kek = 2 + 3 * 8 / 2|} in
  let result = Main.parse (Lexing.from_string input) in
  let head = List.hd result in
  let rec calc_binops = function
    | Binop { op_id = "/"; params = a, b } -> calc_binops a / calc_binops b
    | Binop { op_id = "*"; params = a, b } -> calc_binops a * calc_binops b
    | Binop { op_id = "-"; params = a, b } -> calc_binops a - calc_binops b
    | Binop { op_id = "+"; params = a, b } -> calc_binops a + calc_binops b
    | Int_constr i -> i
    | _ -> assert false
  in
  let math_result =
    match head with
    | Declaration { body_part = { expr; _ }; _ } -> calc_binops expr
    | _ -> assert false
  in
  assert_equal expect_data result;
  assert_equal (2 + (3 * 8 / 2)) math_result

let test_multiple_let _ =
  let expect_data =
    [
      Ast.Declaration
        {
          Ast.type_part_data =
            Some
              {
                Ast.decl_name = "kek";
                type_alias = { Ast.params = []; content = Ast.Concrete "Lol" };
              };
          body_part =
            {
              Ast.name = "kek";
              expr =
                Ast.Let
                  {
                    Ast.let_expr_items =
                      [
                        {
                          type_part = None;
                          body_part = { name = "a"; body = Ast.Int_constr 2 };
                        };
                        {
                          type_part = None;
                          body_part = { name = "b"; body = Ast.Int_constr 3 };
                        };
                        {
                          type_part = None;
                          body_part = { name = "c"; body = Ast.Int_constr 4 };
                        };
                      ];
                    in_ = Ast.Int_constr 3;
                  };
            };
        };
    ]
  in
  let input =
    {|
kek: Lol                            
kek = let a = 2
b = 3 
c = 4 in 3|}
  in
  let result = Main.parse (Lexing.from_string input) in
  assert_equal expect_data result

let test_multiple_let_with_types _ =
  let expect_data =
    [
      Ast.Declaration
        {
          Ast.type_part_data =
            Some
              {
                Ast.decl_name = "kek";
                type_alias = { Ast.params = []; content = Ast.Concrete "Lol" };
              };
          body_part =
            {
              Ast.name = "kek";
              expr =
                Ast.Let
                  {
                    Ast.let_expr_items =
                      [
                        {
                          Ast.type_part = None;
                          body_part =
                            { Ast.name = "a"; body = Ast.Int_constr 2 };
                        };
                        {
                          Ast.type_part =
                            Some
                              {
                                Ast.name = "b";
                                content =
                                  {
                                    Ast.params = [];
                                    content = Ast.Concrete "Int";
                                  };
                              };
                          body_part =
                            { Ast.name = "b"; body = Ast.Int_constr 3 };
                        };
                        {
                          Ast.type_part =
                            Some
                              {
                                Ast.name = "c";
                                content =
                                  {
                                    Ast.params = [];
                                    content = Ast.Concrete "Int";
                                  };
                              };
                          body_part =
                            { Ast.name = "c"; body = Ast.Int_constr 4 };
                        };
                      ];
                    in_ = Ast.Int_constr 3;
                  };
            };
        };
    ]
  in
  let input =
    {|
kek: Lol                            
kek = let a = 2

b: Int
b = 3 

c: Int
c = 4 in 3|}
  in
  let result = Main.parse (Lexing.from_string input) in
  assert_equal expect_data result

let test_if_then_else _ =
  let expect_data =
    [
      Ast.Declaration
        {
          Ast.type_part_data =
            Some
              {
                Ast.decl_name = "lol";
                type_alias = { Ast.params = []; content = Ast.Concrete "Kek" };
              };
          body_part =
            {
              Ast.name = "lol";
              expr =
                Ast.If_then_else
                  {
                    Ast.if_exp =
                      Ast.Constr { Ast.constr_name = "True"; params = [] };
                    then_exp =
                      Ast.Binop
                        {
                          Ast.op_id = "+";
                          params = (Ast.Int_constr 3, Ast.Int_constr 2);
                        };
                    else_exp =
                      Some
                        (Ast.Binop
                           {
                             Ast.op_id = "+";
                             params = (Ast.Int_constr 4, Ast.Int_constr 5);
                           });
                  };
            };
        };
    ]
  in
  let input =
    {|
lol: Kek                            
lol = if True then 3 + 2 else 4 + 5|}
  in
  let result = Main.parse (Lexing.from_string input) in
  assert_equal expect_data result

let test_if_then_else_if_else _ =
  let expect_data =
    [
      Ast.Declaration
        {
          Ast.type_part_data =
            Some
              {
                Ast.decl_name = "lol";
                type_alias = { Ast.params = []; content = Ast.Concrete "Kek" };
              };
          body_part =
            {
              Ast.name = "lol";
              expr =
                Ast.If_then_else
                  {
                    Ast.if_exp =
                      Ast.Constr { Ast.constr_name = "True"; params = [] };
                    then_exp =
                      Ast.Binop
                        {
                          Ast.op_id = "+";
                          params = (Ast.Int_constr 3, Ast.Int_constr 2);
                        };
                    else_exp =
                      Some
                        (Ast.If_then_else
                           {
                             Ast.if_exp =
                               Ast.Constr
                                 { Ast.constr_name = "False"; params = [] };
                             then_exp = Ast.Int_constr 4;
                             else_exp =
                               Some
                                 (Ast.If_then_else
                                    {
                                      Ast.if_exp =
                                        Ast.Constr
                                          {
                                            Ast.constr_name = "True";
                                            params = [];
                                          };
                                      then_exp = Ast.Int_constr 10;
                                      else_exp = Some (Ast.Int_constr 155);
                                    });
                           });
                  };
            };
        };
    ]
  in
  let input =
    {|
lol: Kek                            
lol = if True then 3 + 2 else if False then 4 else if True then 10 else 155|}
  in
  let result = Main.parse (Lexing.from_string input) in
  assert_equal expect_data result

let test_record _ =
  let expect_data =
    [
      Ast.Declaration
        {
          Ast.type_part_data =
            Some
              {
                Ast.decl_name = "lel";
                type_alias = { Ast.params = []; content = Ast.Concrete "Kek" };
              };
          body_part =
            {
              Ast.name = "lel";
              expr =
                Ast.Record
                  [
                    { Ast.name = "a"; value = Ast.String_constr "LOL" };
                    { Ast.name = "b"; value = Ast.Int_constr 69 };
                  ];
            };
        };
    ]
  in
  let input =
    {|
lel: Kek                            
lel = { a = "LOL", b = 69 }|}
  in
  let result = Main.parse (Lexing.from_string input) in
  assert_equal expect_data result

let test_call_fn_inside_record _ =
  let expect_data =
    [
      Ast.Declaration
        {
          Ast.type_part_data =
            Some
              {
                Ast.decl_name = "lel";
                type_alias = { Ast.params = []; content = Ast.Concrete "Lol" };
              };
          body_part =
            {
              Ast.name = "kek";
              expr =
                Ast.Record
                  [
                    {
                      Ast.name = "a";
                      value =
                        Ast.Apply
                          {
                            Ast.ident = Ast.Ident "fn";
                            args = [ Ast.Int_constr 2; Ast.Int_constr 3 ];
                          };
                    };
                  ];
            };
        };
    ]
  in
  let input = {|
lel: Lol
kek = {a=fn 2 3}|} in
  let result = Main.parse (Lexing.from_string input) in
  assert_equal expect_data result

let test_call_fn_inside_record_plus_fn _ =
  let expect_data =
    [
      Ast.Declaration
        {
          Ast.type_part_data =
            Some
              {
                Ast.decl_name = "kek";
                type_alias = { Ast.params = []; content = Ast.Concrete "Any" };
              };
          body_part =
            {
              Ast.name = "kek";
              expr =
                Ast.Record
                  [
                    {
                      Ast.name = "a";
                      value =
                        Ast.Apply
                          {
                            Ast.ident = Ast.Ident "fn";
                            args =
                              [
                                Ast.Int_constr 2;
                                Ast.Apply
                                  {
                                    Ast.ident = Ast.Ident "fn2";
                                    args =
                                      [ Ast.Int_constr 2; Ast.Int_constr 3 ];
                                  };
                              ];
                          };
                    };
                  ];
            };
        };
    ]
  in
  let input =
    {|
kek: Any                            
kek = {a=fn 2 (fn2 2 3) }|}
  in
  let result = Main.parse (Lexing.from_string input) in
  assert_equal expect_data result

let test_list_pm _ =
  let expect_data =
    [
      Ast.Declaration
        {
          Ast.type_part_data =
            Some
              {
                Ast.decl_name = "listTestPM";
                type_alias = { Ast.params = []; content = Ast.Concrete "Int" };
              };
          body_part =
            {
              Ast.name = "listTestPM";
              expr =
                Ast.Case_of
                  {
                    Ast.expr =
                      Ast.List_constr
                        [ Ast.Int_constr 1; Ast.Int_constr 2; Ast.Int_constr 4 ];
                    pattern_data_items =
                      [
                        {
                          Ast.pattern =
                            Ast.PList
                              [ Ast.PInt 1; Ast.PInt 3; Ast.PInt 5; Ast.PInt 6 ];
                          expr = Ast.Int_constr 123;
                        };
                        {
                          Ast.pattern =
                            Ast.PList
                              [
                                Ast.PAnything;
                                Ast.PAnything;
                                Ast.PAnything;
                                Ast.PInt 19;
                              ];
                          expr = Ast.Int_constr 60;
                        };
                        {
                          Ast.pattern = Ast.PAnything;
                          expr =
                            Ast.Case_of
                              {
                                Ast.expr = Ast.Int_constr 2;
                                pattern_data_items =
                                  [
                                    {
                                      Ast.pattern = Ast.PInt 2;
                                      expr = Ast.Int_constr 6;
                                    };
                                    {
                                      Ast.pattern = Ast.PAnything;
                                      expr = Ast.Int_constr 0;
                                    };
                                  ];
                              };
                        };
                      ];
                  };
            };
        };
    ]
  in
  let input =
    {|
listTestPM: Int                     
listTestPM = case [1, 2, 4] of
  [1, 3, 5, 6] -> 123
  [_, _, _, 19] -> 60
  _ -> case 2 of
    2 -> 6
    _ -> 0|}
  in
  let result = Main.parse (Lexing.from_string input) in
  assert_equal expect_data result

let test_record_pm _ =
  let expect_data =
    [
      Ast.Declaration
        {
          Ast.type_part_data =
            Some
              {
                Ast.decl_name = "lol";
                type_alias = { Ast.params = []; content = Ast.Type_var "kek" };
              };
          body_part =
            {
              Ast.name = "dfsf";
              expr =
                Ast.Case_of
                  {
                    Ast.expr = Ast.Int_constr 2;
                    pattern_data_items =
                      [
                        {
                          Ast.pattern = Ast.PRecord [ "a"; "b"; "c" ];
                          expr = Ast.Int_constr 3;
                        };
                        { Ast.pattern = Ast.PAnything; expr = Ast.Int_constr 5 };
                      ];
                  };
            };
        };
    ]
  in
  let input =
    {|
lol: kek                            
dfsf = case 2 of
  {a, b, c} -> 3
  _ -> 5|}
  in
  let result = Main.parse (Lexing.from_string input) in
  assert_equal expect_data result

let test_cons_pm _ =
  let expect_data =
    [
      Ast.Declaration
        {
          Ast.type_part_data =
            Some
              {
                Ast.decl_name = "abcd";
                type_alias = { Ast.params = []; content = Ast.Concrete "Int" };
              };
          body_part =
            {
              Ast.name = "abcd";
              expr =
                Ast.Case_of
                  {
                    Ast.expr = Ast.Ident "b";
                    pattern_data_items =
                      [
                        {
                          Ast.pattern =
                            Ast.PCtor
                              ( "F",
                                [
                                  Ast.PCtor
                                    ("C", [ Ast.PCtor ("D", [ Ast.PStr "" ]) ]);
                                ] );
                          expr = Ast.Int_constr 3;
                        };
                        {
                          Ast.pattern =
                            Ast.PCtor
                              ("F", [ Ast.PCtor ("C", [ Ast.PAnything ]) ]);
                          expr = Ast.Int_constr 6;
                        };
                        { Ast.pattern = Ast.PAnything; expr = Ast.Int_constr 4 };
                      ];
                  };
            };
        };
    ]
  in
  let input =
    {|
abcd: Int                           
abcd = case b of
  F (C (D "")) -> 3
  F (C _) -> 6
  _ -> 4|}
  in
  let result = Main.parse (Lexing.from_string input) in
  assert_equal expect_data result

let suite =
  [
    "test_decl_string" >:: test_decl_string;
    "test_let_in_binop" >:: test_let_in_binop;
    "test_let_in_let_in_let" >:: test_let_in_let_in_let;
    "test_math" >:: test_math;
    "test_multiple_let" >:: test_multiple_let;
    "test_multiple_let_with_types" >:: test_multiple_let_with_types;
    "test_if_then_else" >:: test_if_then_else;
    "test_if_then_else_if_else" >:: test_if_then_else_if_else;
    "test_record" >:: test_record;
    "test_call_fn_inside_record_plus_fn" >:: test_call_fn_inside_record_plus_fn;
    "test_list_pm" >:: test_list_pm;
    "test_record_pm" >:: test_record_pm;
    "test_cons_pm" >:: test_cons_pm;
  ]
