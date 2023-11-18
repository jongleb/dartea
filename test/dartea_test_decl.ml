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
  let input = {|thisIsTheString: String
    thisIsTheString = "This"|} in
  let result = Parser.prog Lexer.token (Lexing.from_string input) in
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
  let input = {|lol: Kek                            
  lol = let a = 2 in 2|} in
  let result = Parser.prog Lexer.token (Lexing.from_string input) in
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
    {|lol: Kek                            
    lol = let a = 2 in 2 + 3|}
  in
  let result = Parser.prog Lexer.token (Lexing.from_string input) in
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
    {|kek: Lol                            
  kek = let a = let b = let c = 3 in 3 in 3 in 3|}
  in
  let result = Parser.prog Lexer.token (Lexing.from_string input) in
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
  let input =
    {|kek: Int                            
    kek = 2 + 3 * 8 / 2|}
  in
  let result = Parser.prog Lexer.token (Lexing.from_string input) in
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
    {|kek: Lol                            
  kek = let a = 2
  b = 3 
  c = 4 in 3|}
  in
  let result = Parser.prog Lexer.token (Lexing.from_string input) in
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
    {|kek: Lol                            
    kek = let a = 2

    b: Int
    b = 3 

    c: Int
    c = 4 in 3|}
  in
  let result = Parser.prog Lexer.token (Lexing.from_string input) in
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
    {|lol: Kek                            
    lol = if True then 3 + 2 else 4 + 5|}
  in
  let result = Parser.prog Lexer.token (Lexing.from_string input) in
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
    {|lol: Kek                            
    lol = if True then 3 + 2 else if False then 4 else if True then 10 else 155|}
  in
  let result = Parser.prog Lexer.token (Lexing.from_string input) in
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
    {|lel: Kek                            
    lel = { a = "LOL", b = 69 }|}
  in
  let result = Parser.prog Lexer.token (Lexing.from_string input) in
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
  let input = {|lel: Lol
  kek = {a=fn 2 3}|} in
  let result = Parser.prog Lexer.token (Lexing.from_string input) in
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
    {|kek: Any                            
  kek = {a=fn 2 (fn2 2 3) }|}
  in
  let result = Parser.prog Lexer.token (Lexing.from_string input) in
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
  ]
