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
                    Ast.let_name = "a";
                    body = Ast.Int_constr 2;
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
                    Ast.let_name = "a";
                    body = Ast.Int_constr 2;
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
                    Ast.let_name = "a";
                    body =
                      Ast.Let
                        {
                          Ast.let_name = "b";
                          body =
                            Ast.Let
                              {
                                Ast.let_name = "c";
                                body = Ast.Int_constr 3;
                                in_ = Ast.Int_constr 3;
                              };
                          in_ = Ast.Int_constr 3;
                        };
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

let suite =
  [
    "test_decl_string" >:: test_decl_string;
    "test_let_in_binop" >:: test_let_in_binop;
    "test_let_in_let_in_let" >:: test_let_in_let_in_let;
  ]
