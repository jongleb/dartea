open OUnit2
open Ast.Kind.Frontend
open Data
open Located
module Main = Parse.Main

let test_module_parsed _ =
  let input =
    {|
module Abcd exposing (..)

import Ab as A exposing (someThing)
import Ab as A exposing (someThing)
|}
  in
  let result = Main.parse input in
  let module_ =
    Module.of_impl (match result with Ok r -> r | Error e -> raise e)
  in

  let expect_import_name = "Abcd" in
  let expect_imports =
    [
      {
        Import_thing.name = ~?"Ab";
        alias = Some "A";
        exposing =
          Exposing.Explicit
            [
              Exposing.Upper
                { Exposing.name = ~?"someThing"; privacy = Exposing.Private };
            ];
      };
    ]
  in

  let expect_top_level_decls =
    [
      Module.Top_declaration
        {
          Declaration.type_part_data = None;
          body_part =
            {
              Declaration.name = ~?"someData";
              expr =
                ~?(Expr.Expr_constr
                     { Expr.name = "Maybe"; arguments = [ Expr.Expr_int 2 ] });
            };
        };
    ]
  in

  let expect_exports = Exposing.Open in

  assert_equal module_.name ~?expect_import_name;
  assert_equal module_.imports expect_imports;
  assert_equal module_.top_levels expect_top_level_decls;
  assert_equal module_.exports expect_exports

let suite = [ "test_module_parsed" >:: test_module_parsed ]
