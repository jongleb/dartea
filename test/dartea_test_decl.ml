open OUnit2
open Ast.Kind.Frontend
open Data
open Located
open Expr
module Main = Parse.Main

let test_decl_string _ =
  let expect_data =
    [
      Impl.Top_declaration
        {
          Declaration.type_part_data =
            Some
              {
                Declaration.name = Data.Located.dummy "thisIsTheString";
                type_alias =
                  {
                    Typedef.Impl.parameters = [];
                    body = Typedef.Kind.Tkind_concrete ~?"String";
                  };
              };
          body_part =
            {
              Declaration.name = ~?"thisIsTheString";
              expr = ~?(Expr.Expr_string "This");
            };
        };
    ]
  in
  let input = {|
   thisIsTheString: String
   thisIsTheString = "This"|} in
  let result = Main.parse input in
  assert_equal (Ok expect_data) result

(* let test_let_in _ =
     let expect_data =
       [
         Impl.Top_declaration
           {
             Declaration.type_part_data =
               Some
                 {
                   Declaration.name = ~?"lol";
                   type_alias =
                     {
                       Typedef.Impl.parameters = [];
                       body = Typedef.Kind.Tkind_concrete ~?"Kek";
                     };
                 };
             body_part =
               {
                 Declaration.name = ~?"lol";
                 expr =
                   ~?(Expr.Expr_let
                        {
                          Expr.bindings =
                            [
                              {
                                Expr.bind_type = None;
                                bind_body =
                                  { Expr.name = ~?"a"; body = Expr.Expr_int 2 };
                              };
                            ];
                          body = Expr.Expr_int 2;
                        });
               };
           };
       ]
     in
     let input = {|
   lol: Kek
   lol = let a = 2 in 2|} in
     let result = Main.parse input in
     assert_equal (Ok expect_data) result

   let test_let_in_binop _ =
     let expect_data =
       [
         Impl.Top_declaration
           {
             Declaration.type_part_data =
               Some
                 {
                   Declaration.name = ~?"lol";
                   type_alias =
                     {
                       Typedef.Impl.parameters = [];
                       body = Typedef.Kind.Tkind_concrete ~?"Kek";
                     };
                 };
             body_part =
               {
                 Declaration.name = ~?"lol";
                 expr =
                   ~?(Expr.Expr_let
                        {
                          Expr.bindings =
                            [
                              {
                                Expr.bind_type = None;
                                bind_body =
                                  { Expr.name = ~?"a"; body = Expr.Expr_int 2 };
                              };
                            ];
                          body =
                            Expr.Expr_binop
                              {
                                Expr.name = "+";
                                operands = (Expr.Expr_int 2, Expr.Expr_int 3);
                              };
                        });
               };
           };
       ]
     in
     let input =
       {|
   lol: Kek
   lol = let a = 2 in 2 + 3|}
     in
     let result = Main.parse input in
     assert_equal (Ok expect_data) result

   let test_let_in_let_in_let _ =
     let expect_data =
       [
         Impl.Top_declaration
           {
             Declaration.type_part_data =
               Some
                 {
                   Declaration.name = ~?"kek";
                   type_alias =
                     {
                       Typedef.Impl.parameters = [];
                       body = Typedef.Kind.Tkind_concrete ~?"Lol";
                     };
                 };
             body_part =
               {
                 Declaration.name = ~?"kek";
                 expr =
                   ~?(Expr.Expr_let
                        {
                          Expr.bindings =
                            [
                              {
                                Expr.bind_type = None;
                                bind_body =
                                  {
                                    Expr.name = ~?"a";
                                    body =
                                      Expr.Expr_let
                                        {
                                          Expr.bindings =
                                            [
                                              {
                                                Expr.bind_type = None;
                                                bind_body =
                                                  {
                                                    Expr.name = ~?"b";
                                                    body =
                                                      Expr.Expr_let
                                                        {
                                                          Expr.bindings =
                                                            [
                                                              {
                                                                Expr.bind_type =
                                                                  None;
                                                                bind_body =
                                                                  {
                                                                    Expr.name =
                                                                      ~?"c";
                                                                    body =
                                                                      Expr.Expr_int
                                                                        3;
                                                                  };
                                                              };
                                                            ];
                                                          body = Expr.Expr_int 3;
                                                        };
                                                  };
                                              };
                                            ];
                                          body = Expr.Expr_int 3;
                                        };
                                  };
                              };
                            ];
                          body = Expr.Expr_int 3;
                        });
               };
           };
       ]
     in
     let input =
       {|
   kek: Lol
   kek = let a = let b = let c = 3 in 3 in 3 in 3|}
     in
     let result = Main.parse input in
     assert_equal expect_data (Result.get_ok result)

   let test_math _ =
     let expect_data =
       [
         Impl.Top_declaration
           {
             Declaration.type_part_data =
               Some
                 {
                   Declaration.name = ~?"kek";
                   type_alias =
                     {
                       Typedef.Impl.parameters = [];
                       body = Typedef.Kind.Tkind_concrete ~?"Int";
                     };
                 };
             body_part =
               {
                 Declaration.name = ~?"kek";
                 expr =
                   ~?(Expr.Expr_binop
                        {
                          Expr.name = "+";
                          operands =
                            ( Expr.Expr_int 2,
                              Expr.Expr_binop
                                {
                                  Expr.name = "/";
                                  operands =
                                    ( Expr.Expr_binop
                                        {
                                          Expr.name = "*";
                                          operands =
                                            (Expr.Expr_int 3, Expr.Expr_int 8);
                                        },
                                      Expr.Expr_int 2 );
                                } );
                        });
               };
           };
       ]
     in
     let input = {|
   kek: Int
   kek = 2 + 3 * 8 / 2|} in
     let result = Main.parse input in
     let head = List.hd (Result.get_ok result) in
     let rec calc_binops = function
       | Expr.Expr_binop { name = "/"; operands = a, b } ->
           calc_binops a / calc_binops b
       | Expr.Expr_binop { name = "*"; operands = a, b } ->
           calc_binops a * calc_binops b
       | Expr.Expr_binop { name = "-"; operands = a, b } ->
           calc_binops a - calc_binops b
       | Expr.Expr_binop { name = "+"; operands = a, b } ->
           calc_binops a + calc_binops b
       | Expr_int i -> i
       | _ -> assert false
     in
     let math_result =
       match head with
       | Impl.Top_declaration { body_part = { expr = { thing; _ }; _ }; _ } ->
           calc_binops thing
       | _ -> assert false
     in
     assert_equal expect_data (Result.get_ok result);
     assert_equal (2 + (3 * 8 / 2)) math_result

   let test_multiple_let _ =
     let expect_data =
       [
         Impl.Top_declaration
           {
             Declaration.type_part_data =
               Some
                 {
                   Declaration.name = ~?"kek";
                   type_alias =
                     {
                       Typedef.Impl.parameters = [];
                       body = Typedef.Kind.Tkind_concrete ~?"Lol";
                     };
                 };
             body_part =
               {
                 Declaration.name = ~?"kek";
                 expr =
                   ~?(Expr.Expr_let
                        {
                          Expr.bindings =
                            [
                              {
                                Expr.bind_type = None;
                                bind_body =
                                  { Expr.name = ~?"a"; body = Expr.Expr_int 2 };
                              };
                              {
                                Expr.bind_type = None;
                                bind_body =
                                  { Expr.name = ~?"b"; body = Expr.Expr_int 3 };
                              };
                              {
                                Expr.bind_type = None;
                                bind_body =
                                  { Expr.name = ~?"c"; body = Expr.Expr_int 4 };
                              };
                            ];
                          body = Expr.Expr_int 3;
                        });
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
     let result = Main.parse input in
     assert_equal expect_data (Result.get_ok result)

   let test_multiple_let_with_types _ =
     let expect_data =
       [
         Impl.Top_declaration
           {
             Declaration.type_part_data =
               Some
                 {
                   Declaration.name = ~?"kek";
                   type_alias =
                     {
                       Typedef.Impl.parameters = [];
                       body = Typedef.Kind.Tkind_concrete ~?"Lol";
                     };
                 };
             body_part =
               {
                 Declaration.name = ~?"kek";
                 expr =
                   ~?(Expr.Expr_let
                        {
                          Expr.bindings =
                            [
                              {
                                Expr.bind_type = None;
                                bind_body =
                                  { Expr.name = ~?"a"; body = Expr.Expr_int 2 };
                              };
                              {
                                Expr.bind_type =
                                  Some
                                    {
                                      Expr.name = "b";
                                      content =
                                        {
                                          Typedef.Impl.parameters = [];
                                          body =
                                            Typedef.Kind.Tkind_concrete ~?"Int";
                                        };
                                    };
                                bind_body =
                                  { Expr.name = ~?"b"; body = Expr.Expr_int 3 };
                              };
                              {
                                Expr.bind_type =
                                  Some
                                    {
                                      Expr.name = "c";
                                      content =
                                        {
                                          Typedef.Impl.parameters = [];
                                          body =
                                            Typedef.Kind.Tkind_concrete ~?"Int";
                                        };
                                    };
                                bind_body =
                                  { Expr.name = ~?"c"; body = Expr.Expr_int 4 };
                              };
                            ];
                          body = Expr.Expr_int 3;
                        });
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
     let result = Main.parse input in
     assert_equal expect_data (Result.get_ok result)

   let test_if_then_else _ =
     let expect_data =
       [
         Impl.Top_declaration
           {
             Declaration.type_part_data =
               Some
                 {
                   Declaration.name = ~?"lol";
                   type_alias =
                     {
                       Typedef.Impl.parameters = [];
                       body = Typedef.Kind.Tkind_concrete ~?"Kek";
                     };
                 };
             body_part =
               {
                 Declaration.name = ~?"lol";
                 expr =
                   ~?(Expr.Expr_if_then_else
                        {
                          Expr.if_exp =
                            Expr.Expr_constr { Expr.name = "True"; arguments = [] };
                          then_exp =
                            Expr.Expr_binop
                              {
                                Expr.name = "+";
                                operands = (Expr.Expr_int 3, Expr.Expr_int 2);
                              };
                          else_exp =
                            Some
                              (Expr.Expr_binop
                                 {
                                   Expr.name = "+";
                                   operands = (Expr.Expr_int 4, Expr.Expr_int 5);
                                 });
                        });
               };
           };
       ]
     in
     let input =
       {|
   lol: Kek
   lol = if True then 3 + 2 else 4 + 5|}
     in
     let result = Main.parse input in
     assert_equal expect_data (Result.get_ok result)

   let test_if_then_else_if_else _ =
     let expect_data =
       [
         Impl.Top_declaration
           {
             Declaration.type_part_data =
               Some
                 {
                   Declaration.name = ~?"lol";
                   type_alias =
                     {
                       Typedef.Impl.parameters = [];
                       body = Typedef.Kind.Tkind_concrete ~?"Kek";
                     };
                 };
             body_part =
               {
                 Declaration.name = ~?"lol";
                 expr =
                   ~?(Expr.Expr_if_then_else
                        {
                          Expr.if_exp =
                            Expr.Expr_constr { Expr.name = "True"; arguments = [] };
                          then_exp =
                            Expr.Expr_binop
                              {
                                Expr.name = "+";
                                operands = (Expr.Expr_int 3, Expr.Expr_int 2);
                              };
                          else_exp =
                            Some
                              (Expr.Expr_if_then_else
                                 {
                                   Expr.if_exp =
                                     Expr.Expr_constr
                                       { Expr.name = "False"; arguments = [] };
                                   then_exp = Expr.Expr_int 4;
                                   else_exp =
                                     Some
                                       (Expr.Expr_if_then_else
                                          {
                                            Expr.if_exp =
                                              Expr.Expr_constr
                                                {
                                                  Expr.name = "True";
                                                  arguments = [];
                                                };
                                            then_exp = Expr.Expr_int 10;
                                            else_exp = Some (Expr.Expr_int 155);
                                          });
                                 });
                        });
               };
           };
       ]
     in
     let input =
       {|
   lol: Kek
   lol = if True then 3 + 2 else if False then 4 else if True then 10 else 155|}
     in
     let result = Main.parse input in
     assert_equal expect_data (Result.get_ok result)

   let test_record _ =
     let expect_data =
       [
         Impl.Top_declaration
           {
             Declaration.type_part_data =
               Some
                 {
                   Declaration.name = ~?"lel";
                   type_alias =
                     {
                       Typedef.Impl.parameters = [];
                       body = Typedef.Kind.Tkind_concrete ~?"Kek";
                     };
                 };
             body_part =
               {
                 Declaration.name = ~?"lel";
                 expr =
                   ~?(Expr.Expr_record
                        [
                          { Expr.name = "a"; value = Expr.Expr_string "LOL" };
                          { Expr.name = "b"; value = Expr.Expr_int 69 };
                        ]);
               };
           };
       ]
     in
     let input =
       {|
   lel: Kek
   lel = { a = "LOL", b = 69 }|}
     in
     let result = Main.parse input in
     assert_equal expect_data (Result.get_ok result) *)

(* let test_call_fn_inside_record _ =
     let expect_data =
       [
         Impl.Top_declaration
           {
             Declaration.type_part_data =
               Some
                 {
                   Declaration.name = ~?"lel";
                   type_alias =
                     {
                       Typedef.Impl.parameters = [];
                       body = Typedef.Kind.Tkind_concrete ~?"Lol";
                     };
                 };
             body_part =
               {
                 Declaration.name = ~?"kek";
                 expr =
                   ~?(Expr.Expr_record
                        [
                          (* Let a (Int 2) (App (App (Var +) (Var a)) (Int 1)) *)
                          {
                            Expr.name = "a";
                            value =
                              Expr.Expr_apply
                                {
                                  Expr.ident = Expr.Expr_ident "fn";
                                  args = [ Expr.Expr_int 2; Expr.Expr_int 3 ];
                                };
                          };
                        ]);
               };
           };
       ]
     in
     let input = {|
   lel: Lol
   kek = {a=fn 2 3}|} in
     let result = Main.parse input in
     assert_equal expect_data (Result.get_ok result) *)

(* let test_call_fn_inside_record_plus_fn _ =
     let expect_data =
       [
         Impl.Top_declaration
           {
             Declaration.type_part_data =
               Some
                 {
                   Declaration.name = ~?"kek";
                   type_alias =
                     {
                       Typedef.Impl.parameters = [];
                       body = Typedef.Kind.Tkind_concrete ~?"Any";
                     };
                 };
             body_part =
               {
                 Declaration.name = ~?"kek";
                 expr =
                   ~?(Expr.Expr_record
                        [
                          {
                            Expr.name = "a";
                            value =
                              Expr.Expr_apply
                                {
                                  Expr.ident = Expr.Expr_ident "fn";
                                  args =
                                    [
                                      Expr.Expr_int 2;
                                      Expr.Expr_apply
                                        {
                                          Expr.ident = Expr.Expr_ident "fn2";
                                          args =
                                            [ Expr.Expr_int 2; Expr.Expr_int 3 ];
                                        };
                                    ];
                                };
                          };
                        ]);
               };
           };
       ]
     in
     let input =
       {|
   kek: Any
   kek = {a=fn 2 (fn2 2 3) }|}
     in
     let result = Main.parse input in
     assert_equal expect_data (Result.get_ok result) *)

let test_list_pm _ =
  let expect_data =
    [
      Impl.Top_declaration
        {
          Declaration.type_part_data =
            Some
              {
                Declaration.name = ~?"listTestPM";
                type_alias =
                  {
                    Typedef.Impl.parameters = [];
                    body = Typedef.Kind.Tkind_concrete ~?"Int";
                  };
              };
          body_part =
            {
              Declaration.name = ~?"listTestPM";
              expr =
                ~?(Expr.Expr_pattern
                     {
                       Expr.expr =
                         Expr.Expr_list
                           [ Expr.Expr_int 1; Expr.Expr_int 2; Expr.Expr_int 4 ];
                       pattern_data_items =
                         [
                           {
                             Expr.pattern =
                               Pattern.P_list
                                 [
                                   Pattern.P_int 1;
                                   Pattern.P_int 3;
                                   Pattern.P_int 5;
                                   Pattern.P_int 6;
                                 ];
                             expr = Expr.Expr_int 123;
                           };
                           {
                             Expr.pattern =
                               Pattern.P_list
                                 [
                                   Pattern.P_anything;
                                   Pattern.P_anything;
                                   Pattern.P_anything;
                                   Pattern.P_int 19;
                                 ];
                             expr = Expr.Expr_int 60;
                           };
                           {
                             Expr.pattern = Pattern.P_anything;
                             expr =
                               Expr.Expr_pattern
                                 {
                                   Expr.expr = Expr.Expr_int 2;
                                   pattern_data_items =
                                     [
                                       {
                                         Expr.pattern = Pattern.P_int 2;
                                         expr = Expr.Expr_int 6;
                                       };
                                       {
                                         Expr.pattern = Pattern.P_anything;
                                         expr = Expr.Expr_int 0;
                                       };
                                     ];
                                 };
                           };
                         ];
                     });
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
  let result = Main.parse input in
  assert_equal expect_data (Result.get_ok result)

let test_record_pm _ =
  let expect_data =
    [
      Impl.Top_declaration
        {
          Declaration.type_part_data =
            Some
              {
                Declaration.name = ~?"lol";
                type_alias =
                  {
                    Typedef.Impl.parameters = [];
                    body = Typedef.Kind.Tkind_var ~?"kek";
                  };
              };
          body_part =
            {
              Declaration.name = ~?"dfsf";
              expr =
                ~?(Expr.Expr_pattern
                     {
                       Expr.expr = Expr.Expr_int 2;
                       pattern_data_items =
                         [
                           {
                             Expr.pattern = Pattern.P_record [ "a"; "b"; "c" ];
                             expr = Expr.Expr_int 3;
                           };
                           {
                             Expr.pattern = Pattern.P_anything;
                             expr = Expr.Expr_int 5;
                           };
                         ];
                     });
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
  let result = Main.parse input in
  assert_equal expect_data (Result.get_ok result)

let test_constr_pm _ =
  let expect_data =
    [
      Impl.Top_declaration
        {
          Declaration.type_part_data =
            Some
              {
                Declaration.name = ~?"abcd";
                type_alias =
                  {
                    Typedef.Impl.parameters = [];
                    body = Typedef.Kind.Tkind_concrete ~?"Int";
                  };
              };
          body_part =
            {
              Declaration.name = ~?"abcd";
              expr =
                ~?(Expr.Expr_pattern
                     {
                       Expr.expr = Expr.Expr_ident "b";
                       pattern_data_items =
                         [
                           {
                             Expr.pattern =
                               Pattern.P_ctor
                                 ( "F",
                                   [
                                     Pattern.P_ctor
                                       ( "C",
                                         [
                                           Pattern.P_ctor
                                             ("D", [ Pattern.P_str "" ]);
                                         ] );
                                   ] );
                             expr = Expr.Expr_int 3;
                           };
                           {
                             Expr.pattern =
                               Pattern.P_ctor
                                 ( "F",
                                   [
                                     Pattern.P_ctor ("C", [ Pattern.P_anything ]);
                                   ] );
                             expr = Expr.Expr_int 6;
                           };
                           {
                             Expr.pattern = Pattern.P_anything;
                             expr = Expr.Expr_int 4;
                           };
                         ];
                     });
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
  let result = Main.parse input in
  assert_equal expect_data (Result.get_ok result)

let test_cons_pm _ =
  let expect_data =
    [
      Impl.Top_declaration
        {
          Declaration.type_part_data =
            Some
              {
                Declaration.name = ~?"dsf";
                type_alias =
                  {
                    Typedef.Impl.parameters = [];
                    body = Typedef.Kind.Tkind_concrete ~?"Int";
                  };
              };
          body_part =
            {
              Declaration.name = ~?"dfsf";
              expr =
                ~?(Expr.Expr_pattern
                     {
                       Expr.expr = Expr.Expr_int 2;
                       pattern_data_items =
                         [
                           {
                             Expr.pattern =
                               Pattern.P_cons
                                 ( Pattern.P_int 2,
                                   Pattern.P_list [ Pattern.P_int 2 ] );
                             expr = Expr.Expr_int 3;
                           };
                           {
                             Expr.pattern = Pattern.P_anything;
                             expr = Expr.Expr_int 5;
                           };
                         ];
                     });
            };
        };
    ]
  in
  let input =
    {|
dsf: Int                           
dfsf = case 2 of
    2 :: [2] -> 3
    _ -> 5|}
  in
  let result = Main.parse input in
  assert_equal expect_data (Result.get_ok result)

let test_import_maybe_two_dots _ =
  let expect_data =
    [
      Impl.Import
        {
          Import_thing.name = ~?"Maybe";
          alias = None;
          exposing =
            Exposing.Explicit
              [
                Exposing.Upper
                  {
                    Exposing.name = ~?"Maybe";
                    privacy = Exposing.Public dummy_pair;
                  };
              ];
        };
    ]
  in
  let input = {|import Maybe exposing ( Maybe(..) )|} in
  let result =
    input |> Main.parse |> Result.get_ok |> List.map Utils.dummify_all_locs
  in
  assert_equal expect_data result

let test_import_maybe_as_m _ =
  let expect_data =
    [
      Impl.Import
        {
          Import_thing.name = ~?"Maybe";
          alias = Some "M";
          exposing = Exposing.Explicit [];
        };
    ]
  in
  let input = {|import Maybe as M|} in
  let result =
    input |> Main.parse |> Result.get_ok |> List.map Utils.dummify_all_locs
  in
  assert_equal expect_data result

let test_import_maybe_enum _ =
  let expect_data =
    [
      Impl.Import
        {
          Import_thing.name = ~?"List";
          alias = None;
          exposing =
            Exposing.Explicit
              [
                Exposing.Upper
                  { Exposing.name = ~?"map"; privacy = Exposing.Private };
                Exposing.Upper
                  { Exposing.name = ~?"foldl"; privacy = Exposing.Private };
              ];
        };
    ]
  in
  let input = {|import List exposing ( map, foldl )|} in
  let result =
    input |> Main.parse |> Result.get_ok |> List.map Utils.dummify_all_locs
  in
  assert_equal expect_data result

let test_access _ =
  let expect_data =
    [
      Impl.Top_declaration
        {
          Declaration.type_part_data = None;
          body_part =
            {
              Declaration.name = ~?"abcd";
              expr =
                ~?(Expr.Expr_access
                     {
                       Expr.expr =
                         Expr.Expr_access
                           {
                             Expr.expr = Expr.Expr_ident "test";
                             field = ~?"lol";
                           };
                       field = ~?"kek";
                     });
            };
        };
    ]
  in
  let input = {|abcd = test.lol.kek|} in
  let result = input |> Main.parse in
  assert_equal expect_data (Result.get_ok result)

(* let test_accessor _ =
   let expect_data =
     [
       Impl.Top_declaration
         {
           Declaration.type_part_data = None;
           body_part =
             {
               Declaration.name = ~?"abcd";
               expr =
                 ~?(Expr.Expr_apply
                      {
                        Expr.ident = Expr.Expr_ident "map";
                        args =
                          [
                            Expr.Expr_accessor ~?"xField"; Expr.Expr_ident "list";
                          ];
                      });
             };
         };
     ]
   in
   let input = {|abcd = map .xField list|} in
   let result = input |> Main.parse in
   assert_equal expect_data (Result.get_ok result) *)

let test_module_export_all _ =
  let expect_data = [ Impl.ModuleName ~?"Lol"; Impl.Export Exposing.Open ] in
  let input = "module Lol exposing (..)\n" in
  (* FIXME: \n terminated (think about it) *)
  let result = input |> Main.parse in
  assert_equal expect_data (Result.get_ok result)

(* let test_apply _ =
   let expect_data =
     [
       Impl.Top_declaration
         {
           Declaration.type_part_data = None;
           body_part =
             {
               Declaration.name = ~?"abcd";
               expr =
                 ~?(Expr.Expr_let
                      {
                        binding =
                          {
                            bind_type = None;
                            bind_body =
                              { Expr.name = ~?"a"; body = Expr.Expr_int 2 };
                          };
                        body =
                          Expr.Expr_binop
                            {
                              Expr.name = "+";
                              operands = (Expr.Expr_ident "a", Expr.Expr_int 3);
                            };
                      });
             };
         };
     ]
   in
   let input = {|abcd = let a = 2 in a + 3|} in
   let result = input |> Main.parse in
   assert_equal expect_data (Result.get_ok result) *)

let test_apply_long _ =
  (* let expect_data =
     [
       Impl.Top_declaration
         {
           Declaration.type_part_data = None;
           body_part =
             {
               Declaration.name = ~?"abcd";
               expr =
                 ~?(Expr.Expr_let
                      {
                        binding =
                          {
                            bind_type = None;
                            bind_body =
                              { Expr.name = ~?"a"; body = Expr.Expr_int 2 };
                          };
                        body =
                          Expr.Expr_apply
                            {
                              ident = Expr.Expr_ident "eklmn";
                              args =
                                [
                                  Expr.Expr_int 5;
                                  Expr.Expr_ident "a";
                                  Expr.Expr_apply
                                    {
                                      ident = Expr.Expr_ident "fb";
                                      args =
                                        [
                                          Expr.Expr_int 1;
                                          Expr.Expr_int 2;
                                          Expr.Expr_int 3;
                                        ];
                                    };
                                ];
                            };
                      });
             };
         };
     ] *)
  let expect_data =
    [
      Impl.Top_declaration
        {
          Declaration.type_part_data = None;
          body_part =
            {
              Declaration.name = ~?"abcd";
              expr =
                ~?(Expr.Expr_let
                     {
                       binding =
                         {
                           bind_type = None;
                           bind_body =
                             { Expr.name = ~?"a"; body = Expr.Expr_int 2 };
                         };
                       body =
                         Expr.Expr_apply
                           {
                             fn =
                               Expr.Expr_apply
                                 {
                                   fn = Expr.Expr_ident "eklmn";
                                   arg = Expr.Expr_int 5;
                                 };
                             arg = Expr.Expr_ident "a";
                           };
                     });
            };
        };
    ]
  in
  (* let input = {|abcd = let a = 2 in eklmn 5 a (fb (c + 2) hj (jh 34))|} in *)
  (* let input = {|abcd = let a = 2 in eklmn 5 a (fb 1 2 3)|} in *)
  let input = {|abcd = let a = 2 in eklmn 5 a|} in
  let result = input |> Main.parse in
  assert_equal expect_data (Result.get_ok result)

let decls_wih_a_lot_of_gaps_and_newlines_between _ =
  let expect_data =
    [
      Impl.Top_declaration
        {
          Declaration.type_part_data = None;
          body_part = { Declaration.name = ~?"abcd"; expr = ~?(Expr_int 1) };
        };
      Impl.Top_declaration
        {
          Declaration.type_part_data = None;
          body_part =
            {
              Declaration.name = ~?"efg";
              expr =
                ~?(Expr.Expr_binop
                     {
                       Expr.name = "+";
                       operands = (Expr.Expr_int 2, Expr.Expr_int 2);
                     });
            };
        };
      Impl.Top_declaration
        {
          Declaration.type_part_data = None;
          body_part =
            {
              Declaration.name = ~?"test";
              expr =
                ~?(Expr.Expr_binop
                     {
                       Expr.name = "+";
                       operands = (Expr.Expr_int 2, Expr.Expr_int 3);
                     });
            };
        };
    ]
  in
  let input =
    {|




abcd = 1

efg =
 2 + 2

test = 
            2
                          +
  3

|}
  in
  let result = input |> Main.parse in
  assert_equal expect_data (Result.get_ok result)

let decl_case_of _ =
  let expect_data =
    [
      Impl.Top_declaration
        {
          Declaration.type_part_data = None;
          body_part =
            {
              Declaration.name = ~?"dfsdsf";
              expr =
                ~?(Expr.Expr_pattern
                     {
                       Expr.expr = Expr.Expr_int 2;
                       pattern_data_items =
                         [
                           {
                             Expr.pattern = Pattern.P_int 2;
                             expr = Expr.Expr_int 26;
                           };
                           {
                             Expr.pattern = Pattern.P_int 3;
                             expr = Expr.Expr_int 4;
                           };
                           {
                             Expr.pattern = Pattern.P_anything;
                             expr =
                               Expr.Expr_binop
                                 {
                                   Expr.name = "+";
                                   operands = (Expr.Expr_int 2, Expr.Expr_int 3);
                                 };
                           };
                         ];
                     });
            };
        };
    ]
  in
  let input =
    {|




dfsdsf = case 2 of
  2 -> 26
  3 -> 4
  _ -> 2
    + 3
|}
  in
  let result = input |> Main.parse in
  assert_equal expect_data (Result.get_ok result)

let decl_case_of_plus_case_of _ =
  let expect_data =
    [
      Impl.Top_declaration
        {
          Declaration.type_part_data = None;
          body_part =
            {
              Declaration.name = ~?"dfsdsf";
              expr =
                ~?(Expr.Expr_pattern
                     {
                       Expr.expr = Expr.Expr_int 2;
                       pattern_data_items =
                         [
                           {
                             Expr.pattern = Pattern.P_int 2;
                             expr = Expr.Expr_int 26;
                           };
                           {
                             Expr.pattern = Pattern.P_int 3;
                             expr = Expr.Expr_int 4;
                           };
                           {
                             Expr.pattern = Pattern.P_anything;
                             expr =
                               Expr.Expr_binop
                                 {
                                   Expr.name = "+";
                                   operands =
                                     ( Expr.Expr_int 2,
                                       Expr.Expr_pattern
                                         {
                                           Expr.expr = Expr.Expr_int 211;
                                           pattern_data_items =
                                             [
                                               {
                                                 Expr.pattern = Pattern.P_int 66;
                                                 expr = Expr.Expr_int 99;
                                               };
                                               {
                                                 Expr.pattern = Pattern.P_int 77;
                                                 expr = Expr.Expr_int 0;
                                               };
                                               {
                                                 Expr.pattern =
                                                   Pattern.P_anything;
                                                 expr =
                                                   Expr.Expr_binop
                                                     {
                                                       Expr.name = "+";
                                                       operands =
                                                         ( Expr.Expr_int 0,
                                                           Expr.Expr_int 3 );
                                                     };
                                               };
                                             ];
                                         } );
                                 };
                           };
                         ];
                     });
            };
        };
    ]
  in
  let input =
    {|
  
  
  
  
dfsdsf = case 2 of
  2 -> 26
  3 -> 4
  _ -> 2
    + case 211 of
      66 -> 99
      77 -> 0
      _ -> 0
        + 3



|}
  in
  let result = input |> Main.parse in
  assert_equal expect_data (Result.get_ok result)

let suite =
  [
    (* "test_decl_string" >:: test_decl_string;
       "test_let_in_binop" >:: test_let_in_binop;
       "test_let_in_let_in_let" >:: test_let_in_let_in_let;
       "test_math" >:: test_math;
       "test_multiple_let" >:: test_multiple_let;
       "test_multiple_let_with_types" >:: test_multiple_let_with_types;
       "test_if_then_else" >:: test_if_then_else;
       "test_if_then_else_if_else" >:: test_if_then_else_if_else;
       "test_record" >:: test_record; *)
    (* "test_call_fn_inside_record_plus_fn" >:: test_call_fn_inside_record_plus_fn; *)
    (* "test_list_pm" >:: test_list_pm;
       "test_record_pm" >:: test_record_pm;
       "test_constr_pm" >:: test_constr_pm;
       "test_cons_pm" >:: test_cons_pm;
       "test_import_maybe_two_dots" >:: test_import_maybe_two_dots;
       "test_import_maybe_as_m" >:: test_import_maybe_as_m;
       "test_import_maybe_enum" >:: test_import_maybe_enum;
       "test_access" >:: test_access; *)
    (* "test_accessor" >:: test_accessor; *)
    (* "test_module_export_all" >:: test_module_export_all; *)
    (* "test_apply" >:: test_apply; *)
    (* "test_apply_long" >:: test_apply_long; *)
    "decls_wih_a_lot_of_gaps_and_newlines_between"
    >:: decls_wih_a_lot_of_gaps_and_newlines_between;
    "decl_case_of" >:: decl_case_of;
    "decl_case_of_plus_case_of" >:: decl_case_of_plus_case_of;
  ]

(*

test = { 1 + 2 }

test = { 
  1 
  
  
  
  + 2 
} 

*)
