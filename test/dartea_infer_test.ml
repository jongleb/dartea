open OUnit2
open Ast.Kind.Frontend
open Data
open Located
module Main = Parse.Main

let test_a_plus_5 _ =
  let expect_type = Typed.Infer.Ast.TInt in
  let expr =
    Expr.Expr_let
      {
        binding =
          {
            bind_type = None;
            bind_body = { Expr.name = ~?"a"; body = Expr.Expr_int 2 };
          };
        body =
          Expr.Expr_apply
            {
              fn =
                Expr.Expr_apply
                  { fn = Expr.Expr_ident "plus"; arg = Expr.Expr_int 3 };
              arg = Expr.Expr_ident "a";
            };
      }
  in
  let result = Typed.Infer.infer expr in
  assert_equal expect_type result

let suite = [ "test_a_plus_5" >:: test_a_plus_5 ]
