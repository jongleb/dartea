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
              params = [];
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
              params = [];
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
              params = [];
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
              params = [];
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
              params = [];
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
              params = [];
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

let test_apply _ =
  let expect_data =
    [
      Impl.Top_declaration
        {
          Declaration.type_part_data = None;
          body_part =
            {
              Declaration.name = ~?"abcd";
              params = [];
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
  assert_equal expect_data (Result.get_ok result)

let test_apply_long _ =
  let expect_data =
    [
      Impl.Top_declaration
        {
          Declaration.type_part_data = None;
          body_part =
            {
              Declaration.name = ~?"abcd";
              params = [];
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
                                   fn =
                                     Expr.Expr_apply
                                       {
                                         fn = Expr.Expr_ident "eklmn";
                                         arg = Expr.Expr_int 5;
                                       };
                                   arg = Expr.Expr_ident "a";
                                 };
                             arg =
                               Expr.Expr_apply
                                 {
                                   fn =
                                     Expr.Expr_apply
                                       {
                                         fn =
                                           Expr.Expr_apply
                                             {
                                               fn = Expr.Expr_ident "fb";
                                               arg = Expr.Expr_int 1;
                                             };
                                         arg = Expr.Expr_int 2;
                                       };
                                   arg = Expr.Expr_int 3;
                                 };
                           };
                     });
            };
        };
    ]
  in
  let input = {|abcd = let a = 2 in eklmn 5 a (fb 1 2 3)|} in
  let result = input |> Main.parse in
  assert_equal expect_data (Result.get_ok result)

let decls_wih_a_lot_of_gaps_and_newlines_between _ =
  let expect_data =
    [
      Impl.Top_declaration
        {
          Declaration.type_part_data = None;
          body_part =
            { Declaration.name = ~?"abcd"; params = []; expr = ~?(Expr_int 1) };
        };
      Impl.Top_declaration
        {
          Declaration.type_part_data = None;
          body_part =
            {
              Declaration.name = ~?"efg";
              params = [];
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
              params = [];
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
              params = [];
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
              params = [];
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

let let_name_equal_expr _ =
  let expect_data =
    [
      Impl.Top_declaration
        {
          Declaration.type_part_data = None;
          body_part =
            {
              Declaration.name = ~?"abcd";
              params = [];
              expr =
                ~?(Expr.Expr_let
                     {
                       binding =
                         {
                           bind_type = None;
                           bind_body = { name = ~?"a"; body = Expr_int 2 };
                         };
                       body =
                         Expr.Expr_binop
                           {
                             Expr.name = "+";
                             operands =
                               ( Expr.Expr_binop
                                   {
                                     Expr.name = "-";
                                     operands =
                                       ( Expr.Expr_binop
                                           {
                                             Expr.name = "+";
                                             operands =
                                               ( Expr.Expr_binop
                                                   {
                                                     Expr.name = "+";
                                                     operands =
                                                       ( Expr.Expr_ident "a",
                                                         Expr.Expr_int 3 );
                                                   },
                                                 Expr.Expr_int 2 );
                                           },
                                         Expr.Expr_int 7 );
                                   },
                                 Expr.Expr_int 4 );
                           };
                     });
            };
        };
      Impl.Top_declaration
        {
          Declaration.type_part_data = None;
          body_part =
            {
              Declaration.name = ~?"abcd400";
              params = [];
              expr =
                ~?(Expr.Expr_let
                     {
                       binding =
                         {
                           bind_type = None;
                           bind_body = { name = ~?"a"; body = Expr_int 2 };
                         };
                       body =
                         Expr.Expr_let
                           {
                             binding =
                               {
                                 bind_type = None;
                                 bind_body = { name = ~?"y"; body = Expr_int 3 };
                               };
                             body =
                               Expr.Expr_let
                                 {
                                   binding =
                                     {
                                       bind_type = None;
                                       bind_body =
                                         { name = ~?"d"; body = Expr_int 300 };
                                     };
                                   body =
                                     Expr.Expr_binop
                                       {
                                         Expr.name = "+";
                                         operands =
                                           ( Expr.Expr_binop
                                               {
                                                 Expr.name = "-";
                                                 operands =
                                                   ( Expr.Expr_binop
                                                       {
                                                         Expr.name = "+";
                                                         operands =
                                                           ( Expr.Expr_binop
                                                               {
                                                                 Expr.name = "+";
                                                                 operands =
                                                                   ( Expr
                                                                     .Expr_ident
                                                                       "a",
                                                                     Expr
                                                                     .Expr_int
                                                                       3 );
                                                               },
                                                             Expr.Expr_int 2 );
                                                       },
                                                     Expr.Expr_int 7 );
                                               },
                                             Expr.Expr_int 4 );
                                       };
                                 };
                           };
                     });
            };
        };
      Impl.Top_declaration
        {
          Declaration.type_part_data = None;
          body_part =
            {
              Declaration.name = ~?"abcd2";
              params = [];
              expr =
                ~?(Expr.Expr_let
                     {
                       binding =
                         {
                           bind_type = None;
                           bind_body = { name = ~?"b"; body = Expr_int 2 };
                         };
                       body =
                         Expr.Expr_binop
                           {
                             Expr.name = "+";
                             operands = (Expr.Expr_int 5, Expr.Expr_int 7);
                           };
                     });
            };
        };
      Impl.Top_declaration
        {
          Declaration.type_part_data = None;
          body_part =
            {
              Declaration.name = ~?"abcd3";
              params = [];
              expr =
                ~?(Expr.Expr_let
                     {
                       binding =
                         {
                           bind_type = None;
                           bind_body = { name = ~?"b"; body = Expr_int 3 };
                         };
                       body =
                         Expr.Expr_let
                           {
                             binding =
                               {
                                 bind_type = None;
                                 bind_body = { name = ~?"c"; body = Expr_int 7 };
                               };
                             body =
                               Expr.Expr_binop
                                 {
                                   Expr.name = "+";
                                   operands =
                                     (Expr.Expr_ident "b", Expr.Expr_ident "c");
                                 };
                           };
                     });
            };
        };
      Impl.Top_declaration
        {
          Declaration.type_part_data = None;
          body_part =
            {
              Declaration.name = ~?"abcd4";
              params = [];
              expr =
                ~?(Expr.Expr_let
                     {
                       binding =
                         {
                           bind_type = None;
                           bind_body = { name = ~?"b"; body = Expr_int 3 };
                         };
                       body =
                         Expr.Expr_let
                           {
                             binding =
                               {
                                 bind_type = None;
                                 bind_body =
                                   { name = ~?"x"; body = Expr_int 222 };
                               };
                             body =
                               Expr.Expr_binop
                                 {
                                   Expr.name = "+";
                                   operands =
                                     (Expr.Expr_ident "b", Expr.Expr_ident "c");
                                 };
                           };
                     });
            };
        };
      Impl.Top_declaration
        {
          Declaration.type_part_data = None;
          body_part =
            {
              Declaration.name = ~?"dfsdsf222";
              params = [];
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
                                                   Expr.Expr_let
                                                     {
                                                       binding =
                                                         {
                                                           bind_type = None;
                                                           bind_body =
                                                             {
                                                               name = ~?"a";
                                                               body =
                                                                 Expr.Expr_binop
                                                                   {
                                                                     Expr.name =
                                                                       "+";
                                                                     operands =
                                                                       ( Expr
                                                                         .Expr_int
                                                                           3,
                                                                         Expr
                                                                         .Expr_int
                                                                           4 );
                                                                   };
                                                             };
                                                         };
                                                       body =
                                                         Expr.Expr_let
                                                           {
                                                             binding =
                                                               {
                                                                 bind_type =
                                                                   None;
                                                                 bind_body =
                                                                   {
                                                                     name =
                                                                       ~?"b";
                                                                     body =
                                                                       Expr_int
                                                                         3;
                                                                   };
                                                               };
                                                             body =
                                                               Expr.Expr_let
                                                                 {
                                                                   binding =
                                                                     {
                                                                       bind_type =
                                                                         None;
                                                                       bind_body =
                                                                         {
                                                                           name =
                                                                             ~?"cd";
                                                                           body =
                                                                             Expr
                                                                             .Expr_let
                                                                               {
                                                                                binding =
                                                                                {
                                                                                bind_type =
                                                                                None;
                                                                                bind_body =
                                                                                {
                                                                                name =
                                                                                ~?"c";
                                                                                body =
                                                                                Expr_int
                                                                                2;
                                                                                };
                                                                                };
                                                                                body =
                                                                                Expr
                                                                                .Expr_binop
                                                                                {
                                                                                Expr
                                                                                .name =
                                                                                "+";
                                                                                operands =
                                                                                ( 
                                                                                Expr
                                                                                .Expr_ident
                                                                                "c",
                                                                                Expr
                                                                                .Expr_int
                                                                                5
                                                                                );
                                                                                };
                                                                               };
                                                                         };
                                                                     };
                                                                   body =
                                                                     Expr
                                                                     .Expr_let
                                                                       {
                                                                         binding =
                                                                           {
                                                                             bind_type =
                                                                               None;
                                                                             bind_body =
                                                                               {
                                                                                name =
                                                                                ~?"resultttttttt";
                                                                                body =
                                                                                Expr
                                                                                .Expr_pattern
                                                                                {
                                                                                Expr
                                                                                .expr =
                                                                                Expr
                                                                                .Expr_int
                                                                                2222;
                                                                                pattern_data_items =
                                                                                [
                                                                                {
                                                                                Expr
                                                                                .pattern =
                                                                                Pattern
                                                                                .P_int
                                                                                3;
                                                                                expr =
                                                                                Expr
                                                                                .Expr_int
                                                                                3;
                                                                                };
                                                                                {
                                                                                Expr
                                                                                .pattern =
                                                                                Pattern
                                                                                .P_anything;
                                                                                expr =
                                                                                Expr
                                                                                .Expr_let
                                                                                {
                                                                                binding =
                                                                                {
                                                                                bind_type =
                                                                                None;
                                                                                bind_body =
                                                                                {
                                                                                name =
                                                                                ~?"c";
                                                                                body =
                                                                                Expr_int
                                                                                3;
                                                                                };
                                                                                };
                                                                                body =
                                                                                Expr
                                                                                .Expr_pattern
                                                                                {
                                                                                Expr
                                                                                .expr =
                                                                                Expr
                                                                                .Expr_int
                                                                                3;
                                                                                pattern_data_items =
                                                                                [
                                                                                {
                                                                                Expr
                                                                                .pattern =
                                                                                Pattern
                                                                                .P_int
                                                                                2;
                                                                                expr =
                                                                                Expr
                                                                                .Expr_let
                                                                                {
                                                                                binding =
                                                                                {
                                                                                bind_type =
                                                                                None;
                                                                                bind_body =
                                                                                {
                                                                                name =
                                                                                ~?"fddg";
                                                                                body =
                                                                                Expr
                                                                                .Expr_binop
                                                                                {
                                                                                Expr
                                                                                .name =
                                                                                "+";
                                                                                operands =
                                                                                ( 
                                                                                Expr
                                                                                .Expr_binop
                                                                                {
                                                                                Expr
                                                                                .name =
                                                                                "+";
                                                                                operands =
                                                                                ( 
                                                                                Expr
                                                                                .Expr_int
                                                                                3,
                                                                                Expr
                                                                                .Expr_int
                                                                                4
                                                                                );
                                                                                },
                                                                                Expr
                                                                                .Expr_int
                                                                                4
                                                                                );
                                                                                };
                                                                                };
                                                                                };
                                                                                body =
                                                                                Expr
                                                                                .Expr_int
                                                                                2;
                                                                                };
                                                                                };
                                                                                {
                                                                                Expr
                                                                                .pattern =
                                                                                Pattern
                                                                                .P_anything;
                                                                                expr =
                                                                                Expr
                                                                                .Expr_int
                                                                                55555;
                                                                                };
                                                                                ];
                                                                                };
                                                                                };
                                                                                };
                                                                                ];
                                                                                };
                                                                               };
                                                                           };
                                                                         body =
                                                                           Expr
                                                                           .Expr_binop
                                                                             {
                                                                               Expr
                                                                               .name =
                                                                                "+";
                                                                               operands =
                                                                                ( 
                                                                                Expr
                                                                                .Expr_ident
                                                                                "b",
                                                                                Expr
                                                                                .Expr_int
                                                                                3
                                                                                );
                                                                             };
                                                                       };
                                                                 };
                                                           };
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
      Impl.Top_declaration
        {
          Declaration.type_part_data = None;
          body_part =
            {
              Declaration.name = ~?"testxxxx3";
              params = [];
              expr =
                ~?(Expr.Expr_let
                     {
                       binding =
                         {
                           bind_type = None;
                           bind_body = { name = ~?"v"; body = Expr_int 3 };
                         };
                       body =
                         Expr.Expr_let
                           {
                             binding =
                               {
                                 bind_type = None;
                                 bind_body = { name = ~?"c"; body = Expr_int 3 };
                               };
                             body =
                               Expr.Expr_let
                                 {
                                   binding =
                                     {
                                       bind_type = None;
                                       bind_body =
                                         { name = ~?"c2"; body = Expr_int 8 };
                                     };
                                   body = Expr.Expr_int 1;
                                 };
                           };
                     });
            };
        };
      Impl.Top_declaration
        {
          Declaration.type_part_data = None;
          body_part =
            {
              Declaration.name = ~?"xxx";
              params = [];
              expr =
                ~?(Expr.Expr_let
                     {
                       binding =
                         {
                           bind_type = None;
                           bind_body = { name = ~?"a"; body = Expr_int 3 };
                         };
                       body =
                         Expr.Expr_let
                           {
                             binding =
                               {
                                 bind_type = None;
                                 bind_body =
                                   {
                                     name = ~?"c";
                                     body =
                                       Expr.Expr_binop
                                         {
                                           Expr.name = "+";
                                           operands =
                                             (Expr.Expr_int 6, Expr.Expr_int 1);
                                         };
                                   };
                               };
                             body =
                               Expr.Expr_let
                                 {
                                   binding =
                                     {
                                       bind_type = None;
                                       bind_body =
                                         {
                                           name = ~?"o";
                                           body =
                                             Expr.Expr_let
                                               {
                                                 binding =
                                                   {
                                                     bind_type = None;
                                                     bind_body =
                                                       {
                                                         name = ~?"x";
                                                         body =
                                                           Expr.Expr_binop
                                                             {
                                                               Expr.name = "+";
                                                               operands =
                                                                 ( Expr.Expr_int
                                                                     6,
                                                                   Expr.Expr_int
                                                                     2 );
                                                             };
                                                       };
                                                   };
                                                 body =
                                                   Expr.Expr_binop
                                                     {
                                                       Expr.name = "+";
                                                       operands =
                                                         ( Expr.Expr_ident "x",
                                                           Expr.Expr_int 3 );
                                                     };
                                               };
                                         };
                                     };
                                   body = Expr.Expr_int 33333;
                                 };
                           };
                     });
            };
        };
    ]
  in

  let input =
    {|
  
abcd =
                let a =
                                                2
  in
  a
                          +
     3 + 2
     - 7 + 4

abcd400 =
                let 
                  a =
                                                2
                  y = 3
                  d = 300 

  in
  a
                          +
     3 + 2
     - 7 + 4
    
abcd2 = let
          b =
            2
                          in
      5
              +
         7

abcd3 = let b = 3 in let c = 7 in b + c

abcd4 = let b = 3 in 
    let x = 222 in 
    b + c

dfsdsf222 = case 2 of
  2 -> 26
  3 -> 4
  _ -> 2
    + case 211 of
              66 -> 99
              77 -> 0
              _ -> 
                let a = 3 + 4 in 
                let b = 3 in
                let cd = 
                     let c = 2 in c + 5 in
                let resultttttttt =
                     case 2222 of
                          3 -> 3
                          _ -> let c = 3 in case 3 of 
                            2 ->
                              let fddg =
                                   3 +
                                               4 +
                                          4 in
                                          
                                          
                                          
                                          
                                          2
                            _ -> 55555
                in
                b + 3
        
testxxxx3 = let v = 3 in let c = 3 in 
  let c2 = 
                                     8 in 1       


xxx = 
  let
    a =



      3
    c = 6 
          +
      1
    o =
      let x = 6 + 2 in
      x + 3
  in
  33333

|}
  in
  let result = input |> Main.parse in
  assert_equal expect_data (Result.get_ok result)

let ifthenelse_test _ =
  let expect_data =
    [
      Impl.Top_declaration
        {
          Declaration.type_part_data = None;
          body_part =
            {
              Declaration.name = ~?"kek";
              params = [];
              expr =
                ~?(Expr.Expr_if_then_else
                     {
                       if_exp =
                         Expr.Expr_binop
                           {
                             Expr.name = ">";
                             operands = (Expr.Expr_int 3, Expr.Expr_int 4);
                           };
                       then_exp =
                         Expr.Expr_binop
                           {
                             Expr.name = "-";
                             operands = (Expr.Expr_int 6, Expr.Expr_int 2);
                           };
                       else_exp =
                         Expr.Expr_binop
                           {
                             Expr.name = "-";
                             operands = (Expr.Expr_int 5, Expr.Expr_int 2);
                           };
                     });
            };
        };
    ]
  in
  let input =
    {|
    
    
    
kek = if 3 > 4 
      then 6 
          - 
  2 else 
    5

  - 
  
  2

  
  
  
  |}
  in
  let result = input |> Main.parse in
  assert_equal expect_data (Result.get_ok result)

let giant_merged_test _ =
  let expected_data =
    [
      Impl.Top_declaration
        {
          Declaration.type_part_data = None;
          body_part =
            {
              Declaration.name = ~?"megaTest";
              params = [];
              expr =
                ~?(Expr.Expr_let
                     {
                       binding =
                         {
                           bind_type = None;
                           bind_body = { name = ~?"a"; body = Expr_int 3 };
                         };
                       body =
                         Expr.Expr_let
                           {
                             binding =
                               {
                                 bind_type = None;
                                 bind_body =
                                   {
                                     name = ~?"b";
                                     body =
                                       Expr.Expr_pattern
                                         {
                                           Expr.expr = Expr.Expr_int 5;
                                           pattern_data_items =
                                             [
                                               {
                                                 Expr.pattern = Pattern.P_int 3;
                                                 expr = Expr.Expr_int 4;
                                               };
                                               {
                                                 Expr.pattern = Pattern.P_int 5;
                                                 expr =
                                                   Expr.Expr_if_then_else
                                                     {
                                                       if_exp =
                                                         Expr.Expr_binop
                                                           {
                                                             Expr.name = ">";
                                                             operands =
                                                               ( Expr.Expr_ident
                                                                   "a",
                                                                 Expr.Expr_int 2
                                                               );
                                                           };
                                                       then_exp =
                                                         Expr.Expr_let
                                                           {
                                                             binding =
                                                               {
                                                                 bind_type =
                                                                   None;
                                                                 bind_body =
                                                                   {
                                                                     name =
                                                                       ~?"x";
                                                                     body =
                                                                       Expr_int
                                                                         6;
                                                                   };
                                                               };
                                                             body =
                                                               Expr.Expr_pattern
                                                                 {
                                                                   Expr.expr =
                                                                     Expr
                                                                     .Expr_ident
                                                                       "x";
                                                                   pattern_data_items =
                                                                     [
                                                                       {
                                                                         Expr
                                                                         .pattern =
                                                                           Pattern
                                                                           .P_int
                                                                             6;
                                                                         expr =
                                                                           Expr
                                                                           .Expr_let
                                                                             {
                                                                               binding =
                                                                                {
                                                                                bind_type =
                                                                                None;
                                                                                bind_body =
                                                                                {
                                                                                name =
                                                                                ~?"y";
                                                                                body =
                                                                                Expr_int
                                                                                7;
                                                                                };
                                                                                };
                                                                               body =
                                                                                Expr
                                                                                .Expr_binop
                                                                                {
                                                                                Expr
                                                                                .name =
                                                                                "+";
                                                                                operands =
                                                                                ( 
                                                                                Expr
                                                                                .Expr_ident
                                                                                "y",
                                                                                Expr
                                                                                .Expr_int
                                                                                2
                                                                                );
                                                                                };
                                                                             };
                                                                       };
                                                                       {
                                                                         Expr
                                                                         .pattern =
                                                                           Pattern
                                                                           .P_anything;
                                                                         expr =
                                                                           Expr
                                                                           .Expr_int
                                                                             0;
                                                                       };
                                                                     ];
                                                                 };
                                                           };
                                                       else_exp =
                                                         Expr.Expr_int 5;
                                                     };
                                               };
                                               {
                                                 Expr.pattern =
                                                   Pattern.P_anything;
                                                 expr =
                                                   Expr.Expr_let
                                                     {
                                                       binding =
                                                         {
                                                           bind_type = None;
                                                           bind_body =
                                                             {
                                                               name = ~?"temp";
                                                               body =
                                                                 Expr
                                                                 .Expr_pattern
                                                                   {
                                                                     Expr.expr =
                                                                       Expr
                                                                       .Expr_int
                                                                         3;
                                                                     pattern_data_items =
                                                                       [
                                                                         {
                                                                           Expr
                                                                           .pattern =
                                                                             Pattern
                                                                             .P_int
                                                                               4;
                                                                           expr =
                                                                             Expr
                                                                             .Expr_if_then_else
                                                                               {
                                                                                if_exp =
                                                                                Expr
                                                                                .Expr_binop
                                                                                {
                                                                                Expr
                                                                                .name =
                                                                                ">";
                                                                                operands =
                                                                                ( 
                                                                                Expr
                                                                                .Expr_int
                                                                                2,
                                                                                Expr
                                                                                .Expr_int
                                                                                3
                                                                                );
                                                                                };
                                                                                then_exp =
                                                                                Expr
                                                                                .Expr_let
                                                                                {
                                                                                binding =
                                                                                {
                                                                                bind_type =
                                                                                None;
                                                                                bind_body =
                                                                                {
                                                                                name =
                                                                                ~?"deep";
                                                                                body =
                                                                                Expr_int
                                                                                8;
                                                                                };
                                                                                };
                                                                                body =
                                                                                Expr
                                                                                .Expr_binop
                                                                                {
                                                                                Expr
                                                                                .name =
                                                                                "+";
                                                                                operands =
                                                                                ( 
                                                                                Expr
                                                                                .Expr_ident
                                                                                "deep",
                                                                                Expr
                                                                                .Expr_int
                                                                                1
                                                                                );
                                                                                };
                                                                                };
                                                                                else_exp =
                                                                                Expr
                                                                                .Expr_int
                                                                                0;
                                                                               };
                                                                         };
                                                                         {
                                                                           Expr
                                                                           .pattern =
                                                                             Pattern
                                                                             .P_anything;
                                                                           expr =
                                                                             Expr
                                                                             .Expr_int
                                                                               5;
                                                                         };
                                                                       ];
                                                                   };
                                                             };
                                                         };
                                                       body =
                                                         Expr.Expr_binop
                                                           {
                                                             Expr.name = "+";
                                                             operands =
                                                               ( Expr.Expr_ident
                                                                   "temp",
                                                                 Expr.Expr_int 3
                                                               );
                                                           };
                                                     };
                                               };
                                             ];
                                         };
                                   };
                               };
                             body =
                               Expr.Expr_let
                                 {
                                   binding =
                                     {
                                       bind_type = None;
                                       bind_body =
                                         {
                                           name = ~?"final";
                                           body =
                                             Expr.Expr_if_then_else
                                               {
                                                 if_exp =
                                                   Expr.Expr_binop
                                                     {
                                                       Expr.name = ">";
                                                       operands =
                                                         ( Expr.Expr_ident "b",
                                                           Expr.Expr_int 10 );
                                                     };
                                                 then_exp =
                                                   Expr.Expr_pattern
                                                     {
                                                       Expr.expr =
                                                         Expr.Expr_ident "b";
                                                       pattern_data_items =
                                                         [
                                                           {
                                                             Expr.pattern =
                                                               Pattern.P_int 11;
                                                             expr =
                                                               Expr.Expr_let
                                                                 {
                                                                   binding =
                                                                     {
                                                                       bind_type =
                                                                         None;
                                                                       bind_body =
                                                                         {
                                                                           name =
                                                                             ~?"final_temp";
                                                                           body =
                                                                             Expr_int
                                                                               15;
                                                                         };
                                                                     };
                                                                   body =
                                                                     Expr
                                                                     .Expr_if_then_else
                                                                       {
                                                                         if_exp =
                                                                           Expr
                                                                           .Expr_binop
                                                                             {
                                                                               Expr
                                                                               .name =
                                                                                ">";
                                                                               operands =
                                                                                ( 
                                                                                Expr
                                                                                .Expr_ident
                                                                                "final_temp",
                                                                                Expr
                                                                                .Expr_int
                                                                                10
                                                                                );
                                                                             };
                                                                         then_exp =
                                                                           Expr
                                                                           .Expr_binop
                                                                             {
                                                                               Expr
                                                                               .name =
                                                                                "+";
                                                                               operands =
                                                                                ( 
                                                                                Expr
                                                                                .Expr_ident
                                                                                "final_temp",
                                                                                Expr
                                                                                .Expr_int
                                                                                2
                                                                                );
                                                                             };
                                                                         else_exp =
                                                                           Expr
                                                                           .Expr_int
                                                                             0;
                                                                       };
                                                                 };
                                                           };
                                                           {
                                                             Expr.pattern =
                                                               Pattern
                                                               .P_anything;
                                                             expr =
                                                               Expr.Expr_int 100;
                                                           };
                                                         ];
                                                     };
                                                 else_exp =
                                                   Expr.Expr_let
                                                     {
                                                       binding =
                                                         {
                                                           bind_type = None;
                                                           bind_body =
                                                             {
                                                               name =
                                                                 ~?"default";
                                                               body =
                                                                 Expr_int 50;
                                                             };
                                                         };
                                                       body =
                                                         Expr.Expr_binop
                                                           {
                                                             Expr.name = "-";
                                                             operands =
                                                               ( Expr.Expr_ident
                                                                   "default",
                                                                 Expr.Expr_int 5
                                                               );
                                                           };
                                                     };
                                               };
                                         };
                                     };
                                   body =
                                     Expr.Expr_binop
                                       {
                                         Expr.name = "+";
                                         operands =
                                           ( Expr.Expr_ident "final",
                                             Expr.Expr_ident "a" );
                                       };
                                 };
                           };
                     });
            };
        };
    ]
  in
  let input =
    {|

megaTest = let a = 3 in
  let b =
        case 5 of
          3 -> 4
          5 -> if a > 2 
               then let x = 6 in
                    case x of
                      6 -> let y = 7


                          in

                          y + 2
                      _ -> 0
               else 5
          _ -> let temp =
                     case 3 of
                      4 -> if  2 > 3  then
                             let deep =


                                      8 in deep

                                       +
                                       1
                             else 0
                      _ -> 5
                   in temp + 3 in
      let final = 
                  if b > 10 
                  then case b of
                         11 -> let final_temp =



                                         15 in
                                     if final_temp >

                                           10
                                           then final_temp
                                           +     2
                                     else 0
                         _ -> 100
                  else let default = 50 in default - 5 in
      final + a


|}
  in
  let result = input |> Main.parse in
  assert_equal expected_data (Result.get_ok result)

let giant_merged_test2 _ =
  let expected_data =
    [
      Impl.Top_declaration
        {
          Declaration.type_part_data = None;
          body_part =
            {
              Declaration.name = ~?"mega_test3";
              params = [];
              expr =
                ~?(Expr.Expr_let
                     {
                       binding =
                         {
                           bind_type = None;
                           bind_body =
                             {
                               name = ~?"start";
                               body =
                                 Expr.Expr_binop
                                   {
                                     Expr.name = "+";
                                     operands =
                                       (Expr.Expr_int 3, Expr.Expr_int 2);
                                   };
                             };
                         };
                       body =
                         Expr.Expr_pattern
                           {
                             Expr.expr = Expr.Expr_ident "start";
                             pattern_data_items =
                               [
                                 {
                                   Expr.pattern = Pattern.P_int 5;
                                   expr =
                                     Expr.Expr_if_then_else
                                       {
                                         if_exp =
                                           Expr.Expr_binop
                                             {
                                               Expr.name = ">";
                                               operands =
                                                 ( Expr.Expr_int 3,
                                                   Expr.Expr_int 2 );
                                             };
                                         then_exp =
                                           Expr.Expr_pattern
                                             {
                                               Expr.expr = Expr.Expr_int 10;
                                               pattern_data_items =
                                                 [
                                                   {
                                                     Expr.pattern =
                                                       Pattern.P_int 10;
                                                     expr =
                                                       Expr.Expr_let
                                                         {
                                                           binding =
                                                             {
                                                               bind_type = None;
                                                               bind_body =
                                                                 {
                                                                   name =
                                                                     ~?"temp";
                                                                   body =
                                                                     Expr_int 8;
                                                                 };
                                                             };
                                                           body =
                                                             Expr.Expr_pattern
                                                               {
                                                                 Expr.expr =
                                                                   Expr
                                                                   .Expr_ident
                                                                     "temp";
                                                                 pattern_data_items =
                                                                   [
                                                                     {
                                                                       Expr
                                                                       .pattern =
                                                                         Pattern
                                                                         .P_int
                                                                           8;
                                                                       expr =
                                                                         Expr
                                                                         .Expr_let
                                                                           {
                                                                             binding =
                                                                               {
                                                                                bind_type =
                                                                                None;
                                                                                bind_body =
                                                                                {
                                                                                name =
                                                                                ~?"inner";
                                                                                body =
                                                                                Expr_int
                                                                                6;
                                                                                };
                                                                               };
                                                                             body =
                                                                               Expr
                                                                               .Expr_let
                                                                                {
                                                                                binding =
                                                                                {
                                                                                bind_type =
                                                                                None;
                                                                                bind_body =
                                                                                {
                                                                                name =
                                                                                ~?"sum";
                                                                                body =
                                                                                Expr
                                                                                .Expr_binop
                                                                                {
                                                                                Expr
                                                                                .name =
                                                                                "+";
                                                                                operands =
                                                                                ( 
                                                                                Expr
                                                                                .Expr_ident
                                                                                "inner",
                                                                                Expr
                                                                                .Expr_int
                                                                                4
                                                                                );
                                                                                };
                                                                                };
                                                                                };
                                                                                body =
                                                                                Expr
                                                                                .Expr_if_then_else
                                                                                {
                                                                                if_exp =
                                                                                Expr
                                                                                .Expr_binop
                                                                                {
                                                                                Expr
                                                                                .name =
                                                                                ">";
                                                                                operands =
                                                                                ( 
                                                                                Expr
                                                                                .Expr_ident
                                                                                "sum",
                                                                                Expr
                                                                                .Expr_int
                                                                                7
                                                                                );
                                                                                };
                                                                                then_exp =
                                                                                Expr
                                                                                .Expr_pattern
                                                                                {
                                                                                Expr
                                                                                .expr =
                                                                                Expr
                                                                                .Expr_ident
                                                                                "sum";
                                                                                pattern_data_items =
                                                                                [
                                                                                {
                                                                                Expr
                                                                                .pattern =
                                                                                Pattern
                                                                                .P_int
                                                                                10;
                                                                                expr =
                                                                                Expr
                                                                                .Expr_int
                                                                                15;
                                                                                };
                                                                                {
                                                                                Expr
                                                                                .pattern =
                                                                                Pattern
                                                                                .P_int
                                                                                8;
                                                                                expr =
                                                                                Expr
                                                                                .Expr_int
                                                                                12;
                                                                                };
                                                                                {
                                                                                Expr
                                                                                .pattern =
                                                                                Pattern
                                                                                .P_anything;
                                                                                expr =
                                                                                Expr
                                                                                .Expr_let
                                                                                {
                                                                                binding =
                                                                                {
                                                                                bind_type =
                                                                                None;
                                                                                bind_body =
                                                                                {
                                                                                name =
                                                                                ~?"x";
                                                                                body =
                                                                                Expr_int
                                                                                3;
                                                                                };
                                                                                };
                                                                                body =
                                                                                Expr
                                                                                .Expr_binop
                                                                                {
                                                                                Expr
                                                                                .name =
                                                                                "+";
                                                                                operands =
                                                                                ( 
                                                                                Expr
                                                                                .Expr_ident
                                                                                "x",
                                                                                Expr
                                                                                .Expr_int
                                                                                5
                                                                                );
                                                                                };
                                                                                };
                                                                                };
                                                                                ];
                                                                                };
                                                                                else_exp =
                                                                                Expr
                                                                                .Expr_int
                                                                                0;
                                                                                };
                                                                                };
                                                                           };
                                                                     };
                                                                     {
                                                                       Expr
                                                                       .pattern =
                                                                         Pattern
                                                                         .P_anything;
                                                                       expr =
                                                                         Expr
                                                                         .Expr_int
                                                                           0;
                                                                     };
                                                                   ];
                                                               };
                                                         };
                                                   };
                                                   {
                                                     Expr.pattern =
                                                       Pattern.P_anything;
                                                     expr = Expr.Expr_int 0;
                                                   };
                                                 ];
                                             };
                                         else_exp =
                                           Expr.Expr_pattern
                                             {
                                               Expr.expr = Expr.Expr_int 5;
                                               pattern_data_items =
                                                 [
                                                   {
                                                     Expr.pattern =
                                                       Pattern.P_int 5;
                                                     expr =
                                                       Expr.Expr_let
                                                         {
                                                           binding =
                                                             {
                                                               bind_type = None;
                                                               bind_body =
                                                                 {
                                                                   name =
                                                                     ~?"res";
                                                                   body =
                                                                     Expr_int 7;
                                                                 };
                                                             };
                                                           body =
                                                             Expr.Expr_let
                                                               {
                                                                 binding =
                                                                   {
                                                                     bind_type =
                                                                       None;
                                                                     bind_body =
                                                                       {
                                                                         name =
                                                                           ~?"check";
                                                                         body =
                                                                           Expr
                                                                           .Expr_binop
                                                                             {
                                                                               Expr
                                                                               .name =
                                                                                "+";
                                                                               operands =
                                                                                ( 
                                                                                Expr
                                                                                .Expr_ident
                                                                                "res",
                                                                                Expr
                                                                                .Expr_int
                                                                                8
                                                                                );
                                                                             };
                                                                       };
                                                                   };
                                                                 body =
                                                                   Expr
                                                                   .Expr_pattern
                                                                     {
                                                                       Expr.expr =
                                                                         Expr
                                                                         .Expr_ident
                                                                           "check";
                                                                       pattern_data_items =
                                                                         [
                                                                           {
                                                                             Expr
                                                                             .pattern =
                                                                               Pattern
                                                                               .P_int
                                                                                15;
                                                                             expr =
                                                                               Expr
                                                                               .Expr_let
                                                                                {
                                                                                binding =
                                                                                {
                                                                                bind_type =
                                                                                None;
                                                                                bind_body =
                                                                                {
                                                                                name =
                                                                                ~?"final";
                                                                                body =
                                                                                Expr_int
                                                                                20;
                                                                                };
                                                                                };
                                                                                body =
                                                                                Expr
                                                                                .Expr_binop
                                                                                {
                                                                                Expr
                                                                                .name =
                                                                                "+";
                                                                                operands =
                                                                                ( 
                                                                                Expr
                                                                                .Expr_ident
                                                                                "final",
                                                                                Expr
                                                                                .Expr_int
                                                                                1
                                                                                );
                                                                                };
                                                                                };
                                                                           };
                                                                           {
                                                                             Expr
                                                                             .pattern =
                                                                               Pattern
                                                                               .P_anything;
                                                                             expr =
                                                                               Expr
                                                                               .Expr_let
                                                                                {
                                                                                binding =
                                                                                {
                                                                                bind_type =
                                                                                None;
                                                                                bind_body =
                                                                                {
                                                                                name =
                                                                                ~?"alt";
                                                                                body =
                                                                                Expr_int
                                                                                10;
                                                                                };
                                                                                };
                                                                                body =
                                                                                Expr
                                                                                .Expr_binop
                                                                                {
                                                                                Expr
                                                                                .name =
                                                                                "+";
                                                                                operands =
                                                                                ( 
                                                                                Expr
                                                                                .Expr_ident
                                                                                "alt",
                                                                                Expr
                                                                                .Expr_int
                                                                                2
                                                                                );
                                                                                };
                                                                                };
                                                                           };
                                                                         ];
                                                                     };
                                                               };
                                                         };
                                                   };
                                                   {
                                                     Expr.pattern =
                                                       Pattern.P_anything;
                                                     expr = Expr.Expr_int 0;
                                                   };
                                                 ];
                                             };
                                       };
                                 };
                                 {
                                   Expr.pattern = Pattern.P_int 3;
                                   expr =
                                     Expr.Expr_pattern
                                       {
                                         Expr.expr = Expr.Expr_int 8;
                                         pattern_data_items =
                                           [
                                             {
                                               Expr.pattern = Pattern.P_int 8;
                                               expr =
                                                 Expr.Expr_let
                                                   {
                                                     binding =
                                                       {
                                                         bind_type = None;
                                                         bind_body =
                                                           {
                                                             name = ~?"deep";
                                                             body = Expr_int 12;
                                                           };
                                                       };
                                                     body =
                                                       Expr.Expr_if_then_else
                                                         {
                                                           if_exp =
                                                             Expr.Expr_binop
                                                               {
                                                                 Expr.name = ">";
                                                                 operands =
                                                                   ( Expr
                                                                     .Expr_ident
                                                                       "deep",
                                                                     Expr
                                                                     .Expr_int
                                                                       10 );
                                                               };
                                                           then_exp =
                                                             Expr.Expr_let
                                                               {
                                                                 binding =
                                                                   {
                                                                     bind_type =
                                                                       None;
                                                                     bind_body =
                                                                       {
                                                                         name =
                                                                           ~?"ultra";
                                                                         body =
                                                                           Expr
                                                                           .Expr_binop
                                                                             {
                                                                               Expr
                                                                               .name =
                                                                                "+";
                                                                               operands =
                                                                                ( 
                                                                                Expr
                                                                                .Expr_ident
                                                                                "deep",
                                                                                Expr
                                                                                .Expr_int
                                                                                3
                                                                                );
                                                                             };
                                                                       };
                                                                   };
                                                                 body =
                                                                   Expr
                                                                   .Expr_pattern
                                                                     {
                                                                       Expr.expr =
                                                                         Expr
                                                                         .Expr_ident
                                                                           "ultra";
                                                                       pattern_data_items =
                                                                         [
                                                                           {
                                                                             Expr
                                                                             .pattern =
                                                                               Pattern
                                                                               .P_int
                                                                                15;
                                                                             expr =
                                                                               Expr
                                                                               .Expr_let
                                                                                {
                                                                                binding =
                                                                                {
                                                                                bind_type =
                                                                                None;
                                                                                bind_body =
                                                                                {
                                                                                name =
                                                                                ~?"mega";
                                                                                body =
                                                                                Expr_int
                                                                                25;
                                                                                };
                                                                                };
                                                                                body =
                                                                                Expr
                                                                                .Expr_binop
                                                                                {
                                                                                Expr
                                                                                .name =
                                                                                "+";
                                                                                operands =
                                                                                ( 
                                                                                Expr
                                                                                .Expr_ident
                                                                                "mega",
                                                                                Expr
                                                                                .Expr_int
                                                                                5
                                                                                );
                                                                                };
                                                                                };
                                                                           };
                                                                           {
                                                                             Expr
                                                                             .pattern =
                                                                               Pattern
                                                                               .P_anything;
                                                                             expr =
                                                                               Expr
                                                                               .Expr_int
                                                                                0;
                                                                           };
                                                                         ];
                                                                     };
                                                               };
                                                           else_exp =
                                                             Expr.Expr_let
                                                               {
                                                                 binding =
                                                                   {
                                                                     bind_type =
                                                                       None;
                                                                     bind_body =
                                                                       {
                                                                         name =
                                                                           ~?"simple";
                                                                         body =
                                                                           Expr_int
                                                                             7;
                                                                       };
                                                                   };
                                                                 body =
                                                                   Expr
                                                                   .Expr_binop
                                                                     {
                                                                       Expr.name =
                                                                         "+";
                                                                       operands =
                                                                         ( Expr
                                                                           .Expr_ident
                                                                             "simple",
                                                                           Expr
                                                                           .Expr_int
                                                                             1
                                                                         );
                                                                     };
                                                               };
                                                         };
                                                   };
                                             };
                                             {
                                               Expr.pattern = Pattern.P_anything;
                                               expr =
                                                 Expr.Expr_let
                                                   {
                                                     binding =
                                                       {
                                                         bind_type = None;
                                                         bind_body =
                                                           {
                                                             name = ~?"basic";
                                                             body = Expr_int 5;
                                                           };
                                                       };
                                                     body =
                                                       Expr.Expr_binop
                                                         {
                                                           Expr.name = "+";
                                                           operands =
                                                             ( Expr.Expr_ident
                                                                 "basic",
                                                               Expr.Expr_int 2
                                                             );
                                                         };
                                                   };
                                             };
                                           ];
                                       };
                                 };
                                 {
                                   Expr.pattern = Pattern.P_anything;
                                   expr =
                                     Expr.Expr_if_then_else
                                       {
                                         if_exp =
                                           Expr.Expr_binop
                                             {
                                               Expr.name = ">";
                                               operands =
                                                 ( Expr.Expr_int 2,
                                                   Expr.Expr_int 1 );
                                             };
                                         then_exp =
                                           Expr.Expr_let
                                             {
                                               binding =
                                                 {
                                                   bind_type = None;
                                                   bind_body =
                                                     {
                                                       name = ~?"wild";
                                                       body = Expr_int 15;
                                                     };
                                                 };
                                               body =
                                                 Expr.Expr_pattern
                                                   {
                                                     Expr.expr =
                                                       Expr.Expr_ident "wild";
                                                     pattern_data_items =
                                                       [
                                                         {
                                                           Expr.pattern =
                                                             Pattern.P_int 15;
                                                           expr =
                                                             Expr.Expr_let
                                                               {
                                                                 binding =
                                                                   {
                                                                     bind_type =
                                                                       None;
                                                                     bind_body =
                                                                       {
                                                                         name =
                                                                           ~?"path1";
                                                                         body =
                                                                           Expr
                                                                           .Expr_binop
                                                                             {
                                                                               Expr
                                                                               .name =
                                                                                "+";
                                                                               operands =
                                                                                ( 
                                                                                Expr
                                                                                .Expr_ident
                                                                                "wild",
                                                                                Expr
                                                                                .Expr_int
                                                                                5
                                                                                );
                                                                             };
                                                                       };
                                                                   };
                                                                 body =
                                                                   Expr
                                                                   .Expr_if_then_else
                                                                     {
                                                                       if_exp =
                                                                         Expr
                                                                         .Expr_binop
                                                                           {
                                                                             Expr
                                                                             .name =
                                                                               ">";
                                                                             operands =
                                                                               ( 
                                                                               Expr
                                                                               .Expr_ident
                                                                                "path1",
                                                                               Expr
                                                                               .Expr_int
                                                                                10
                                                                               );
                                                                           };
                                                                       then_exp =
                                                                         Expr
                                                                         .Expr_let
                                                                           {
                                                                             binding =
                                                                               {
                                                                                bind_type =
                                                                                None;
                                                                                bind_body =
                                                                                {
                                                                                name =
                                                                                ~?"choice";
                                                                                body =
                                                                                Expr_int
                                                                                30;
                                                                                };
                                                                               };
                                                                             body =
                                                                               Expr
                                                                               .Expr_binop
                                                                                {
                                                                                Expr
                                                                                .name =
                                                                                "+";
                                                                                operands =
                                                                                ( 
                                                                                Expr
                                                                                .Expr_ident
                                                                                "choice",
                                                                                Expr
                                                                                .Expr_int
                                                                                2
                                                                                );
                                                                                };
                                                                           };
                                                                       else_exp =
                                                                         Expr
                                                                         .Expr_int
                                                                           0;
                                                                     };
                                                               };
                                                         };
                                                         {
                                                           Expr.pattern =
                                                             Pattern.P_anything;
                                                           expr =
                                                             Expr.Expr_let
                                                               {
                                                                 binding =
                                                                   {
                                                                     bind_type =
                                                                       None;
                                                                     bind_body =
                                                                       {
                                                                         name =
                                                                           ~?"path2";
                                                                         body =
                                                                           Expr_int
                                                                             8;
                                                                       };
                                                                   };
                                                                 body =
                                                                   Expr
                                                                   .Expr_binop
                                                                     {
                                                                       Expr.name =
                                                                         "+";
                                                                       operands =
                                                                         ( Expr
                                                                           .Expr_ident
                                                                             "path2",
                                                                           Expr
                                                                           .Expr_int
                                                                             3
                                                                         );
                                                                     };
                                                               };
                                                         };
                                                       ];
                                                   };
                                             };
                                         else_exp =
                                           Expr.Expr_let
                                             {
                                               binding =
                                                 {
                                                   bind_type = None;
                                                   bind_body =
                                                     {
                                                       name = ~?"default";
                                                       body = Expr_int 100;
                                                     };
                                                 };
                                               body =
                                                 Expr.Expr_pattern
                                                   {
                                                     Expr.expr =
                                                       Expr.Expr_ident "default";
                                                     pattern_data_items =
                                                       [
                                                         {
                                                           Expr.pattern =
                                                             Pattern.P_int 100;
                                                           expr =
                                                             Expr.Expr_let
                                                               {
                                                                 binding =
                                                                   {
                                                                     bind_type =
                                                                       None;
                                                                     bind_body =
                                                                       {
                                                                         name =
                                                                           ~?"final_sum";
                                                                         body =
                                                                           Expr
                                                                           .Expr_binop
                                                                             {
                                                                               Expr
                                                                               .name =
                                                                                "+";
                                                                               operands =
                                                                                ( 
                                                                                Expr
                                                                                .Expr_ident
                                                                                "default",
                                                                                Expr
                                                                                .Expr_int
                                                                                50
                                                                                );
                                                                             };
                                                                       };
                                                                   };
                                                                 body =
                                                                   Expr
                                                                   .Expr_binop
                                                                     {
                                                                       Expr.name =
                                                                         "+";
                                                                       operands =
                                                                         ( Expr
                                                                           .Expr_ident
                                                                             "final_sum",
                                                                           Expr
                                                                           .Expr_int
                                                                             10
                                                                         );
                                                                     };
                                                               };
                                                         };
                                                         {
                                                           Expr.pattern =
                                                             Pattern.P_anything;
                                                           expr =
                                                             Expr.Expr_let
                                                               {
                                                                 binding =
                                                                   {
                                                                     bind_type =
                                                                       None;
                                                                     bind_body =
                                                                       {
                                                                         name =
                                                                           ~?"last";
                                                                         body =
                                                                           Expr_int
                                                                             20;
                                                                       };
                                                                   };
                                                                 body =
                                                                   Expr
                                                                   .Expr_binop
                                                                     {
                                                                       Expr.name =
                                                                         "+";
                                                                       operands =
                                                                         ( Expr
                                                                           .Expr_ident
                                                                             "last",
                                                                           Expr
                                                                           .Expr_int
                                                                             5
                                                                         );
                                                                     };
                                                               };
                                                         };
                                                       ];
                                                   };
                                             };
                                       };
                                 };
                               ];
                           };
                     });
            };
        };
    ]
  in
  let input =
    {|
  
mega_test3 = 
 let start = 3 + 2 in
   case start of
     5 -> if 3 > 2 then
             case 10 of
               10 -> let temp = 8 in
                     case temp of
                       8 -> let inner = 6 in
                            let sum = inner + 4 in
                            if sum > 7 then
                              case sum of
                                10 -> 15
                                8 -> 12
                                _ -> let x = 3 in x + 5
                            else 0
                       _ -> 0
               _ -> 0
          else 
             case 5 of
               5 -> let res = 7 in 
                    let check = res + 8 in
                    case check of
                      15 -> let final = 20 in final + 1
                      _ -> let alt = 10 in alt + 2
               _ -> 0
     3 -> case 8 of 
            8 -> let deep = 12 in
                 if deep > 10 then
                   let ultra = deep + 3 in
                   case ultra of
                     15 -> let mega = 25 in mega + 5
                     _ -> 0
                 else 
                   let simple = 7 in simple + 1
            _ -> let basic = 5 in basic + 2
     _ -> if 2 > 1 then
            let wild = 15 in
            case wild of
              15 -> let path1 = wild + 5 in
                   if path1 > 10 then 
                     let choice = 30 in choice + 2
                   else 0
              _ -> let path2 = 8 in path2 + 3
          else
            let default = 100 in
            case default of
              100 -> let final_sum = default + 50 in final_sum + 10
              _ -> let last = 20 in last + 5
  
  
  |}
  in
  let result = input |> Main.parse in
  assert_equal expected_data (Result.get_ok result)

let giant_merged_test3 _ =
  let input =
    {|
    
  ultimate_mega_test = 
  let initial = 3 + 7 in
  let setup = 10 in
    case initial of
      10 -> let branch1 = 5 in
            case branch1 of
              5 -> if branch1 > 3 then
                     case setup of
                       10 -> let deep1 = 7 in
                            let deep2 = deep1 + 3 in
                              case deep2 of
                                10 -> if deep2 > 8 then
                                       let ultra1 = 15 in
                                         case ultra1 of
                                           15 -> let mega1 = ultra1 + 5 in
                                                if mega1 > 10 then
                                                  case mega1 of
                                                    20 -> let final1 = 30 in
                                                         case final1 of
                                                           30 -> let result1 = 40 in result1 + 5
                                                           _ -> let alt1 = 25 in alt1 + 2
                                                    _ -> let backup1 = 12 in backup1 + 3
                                                else 0
                                           _ -> let fallback1 = 8 in fallback1 + 1
                                     else let simple1 = 5 in simple1 + 2
                                _ -> let escape1 = 3 in escape1 + 1
                       _ -> let default1 = 6 in default1 + 2
                   else 
                     case 7 of
                       7 -> let other1 = 9 in
                           if other1 > 5 then
                             case other1 of
                               9 -> let path1 = 12 in
                                   if path1 > 10 then path1 + 3
                                   else let alt2 = 8 in alt2 + 2
                               _ -> let escape2 = 4 in escape2 + 1
                           else 0
                       _ -> 0
              _ -> let default2 = 11 in default2 + 4
      5 -> case setup + 5 of
             15 -> let branch2 = 8 in
                  if branch2 > 5 then
                    case branch2 of
                      8 -> let deep3 = 13 in
                          case deep3 of
                            13 -> if deep3 > 10 then
                                   let ultra2 = 18 in
                                   case ultra2 of
                                     18 -> let mega2 = 25 in
                                          if mega2 > 20 then mega2 + 5
                                          else let alt3 = 15 in alt3 + 2
                                     _ -> let escape3 = 10 in escape3 + 3
                                 else 0
                            _ -> let default3 = 7 in default3 + 1
                      _ -> let escape4 = 6 in escape4 + 2
                  else 
                    let simple2 = 9 in
                    case simple2 of
                      9 -> let final2 = 14 in final2 + 3
                      _ -> let alt4 = 11 in alt4 + 2
             _ -> let default4 = 16 in 
                  case default4 of
                    16 -> let last1 = 20 in
                          if last1 > 15 then last1 + 4
                          else let alt5 = 12 in alt5 + 1
                    _ -> let escape5 = 7 in escape5 + 2
      _ -> if initial > 8 then
             let wild1 = 22 in
             case wild1 of
               22 -> let deep4 = 27 in
                    if deep4 > 25 then
                      case deep4 of
                        27 -> let ultra3 = 32 in
                             case ultra3 of
                               32 -> let mega3 = 40 in
                                    if mega3 > 35 then
                                      let final3 = 45 in final3 + 5
                                    else
                                      let alt6 = 30 in alt6 + 3
                               _ -> let escape6 = 25 in escape6 + 2
                        _ -> let default5 = 20 in default5 + 4
                    else
                      let simple3 = 18 in
                      case simple3 of
                        18 -> let path2 = 23 in path2 + 2
                        _ -> let alt7 = 16 in alt7 + 1
               _ -> let default6 = 15 in default6 + 3
           else
             let final4 = 12 in
             case final4 of
               12 -> if final4 > 10 then
                      let ultimate = 50 in
                      case ultimate of
                        50 -> let mega_final = 60 in
                             if mega_final > 55 then mega_final + 10
                             else let last_chance = 40 in last_chance + 5
                        _ -> let escape_final = 30 in escape_final + 4
                    else
                      let simple4 = 25 in simple4 + 3
               _ -> let very_last = 35 in very_last + 5
    
    
    |}
  in
  let result = input |> Main.parse in
  assert_bool "Couldn't be parsed" (Result.is_ok result)

let one_more _ =
  let input =
    {|

test2 a b c = 
    let x = 3 in
    let y = 4 in
    let z = 2 in
    let result = x + a in 
    let result2 = case b of 
              2 -> 2 + 3
              _ -> 3 
    in 
    concat "a" ""
  
|}
  in
  let result = input |> Main.parse in
  assert_bool "Couldn't be parsed" (Result.is_ok result)

let test_access2 _ =
  let input = {|record_example = x.a|} in
  let result = input |> Main.parse in
  assert_bool "Couldn't be parsed" (Result.is_ok result)

let test_access3 _ =
  let input = {|record_example = kek.a.b.c.d.e.f.g.h + 1|} in
  let result = input |> Main.parse in
  assert_bool "Couldn't be parsed" (Result.is_ok result)

(* let test_tuple_pm _ =
  let expect_data =
    [
      Impl.Top_declaration
        {
          Declaration.type_part_data =
            Some
              {
                Declaration.name = ~?"tuplePM";
                type_alias =
                  {
                    Typedef.Impl.parameters = [];
                    body = Typedef.Kind.Tkind_concrete ~?"Int";
                  };
              };
          body_part =
            {
              Declaration.name = ~?"tuplePM";
              params = [];
              expr =
                ~?(Expr.Expr_pattern
                     {
                       Expr.expr =
                         Expr.Expr_tuple
                           [ Expr.Expr_int 1; Expr.Expr_int 2; Expr.Expr_int 3 ];
                       pattern_data_items =
                         [
                           {
                             Expr.pattern =
                               Pattern.P_tuple
                                 [
                                   Pattern.P_int 1;
                                   Pattern.P_int 2;
                                   Pattern.P_int 3;
                                 ];
                             expr = Expr.Expr_int 100;
                           };
                           {
                             Expr.pattern =
                               Pattern.P_tuple
                                 [
                                   Pattern.P_var "a";
                                   Pattern.P_var "b";
                                   Pattern.P_anything;
                                 ];
                             expr =
                               Expr.Expr_binop
                                 {
                                   Expr.name = "+";
                                   operands =
                                     (Expr.Expr_ident "a", Expr.Expr_ident "b");
                                 };
                           };
                           {
                             Expr.pattern = Pattern.P_anything;
                             expr = Expr.Expr_int 0;
                           };
                         ];
                     });
            };
        };
    ]
  in
  let input =
    {|
tuplePM: Int
tuplePM = case (1, 2, 3) of
  (1, 2, 3) -> 100
  (a, b, _) -> a + b
  _ -> 0|}
  in
  let result = Main.parse input in
  assert_equal expect_data (Result.get_ok result) *)

let test_nested_ctor_pm _ =
  let expect_data =
    [
      Impl.Top_declaration
        {
          Declaration.type_part_data = None;
          body_part =
            {
              Declaration.name = ~?"nestedPm";
              params = [];
              expr =
                ~?(Expr.Expr_pattern
                     {
                       Expr.expr = Expr.Expr_ident "value";
                       pattern_data_items =
                         [
                           {
                             Expr.pattern =
                               Pattern.P_ctor
                                 ( "Just",
                                   [
                                     Pattern.P_ctor
                                       ("Just", [ Pattern.P_var "x" ]);
                                   ] );
                             expr = Expr.Expr_ident "x";
                           };
                           {
                             Expr.pattern =
                               Pattern.P_ctor ("Just", [ Pattern.P_anything ]);
                             expr = Expr.Expr_int 0;
                           };
                           {
                             Expr.pattern = Pattern.P_ctor ("Nothing", []);
                             expr =
                               Expr.Expr_unop
                                 { name = ~?"-"; operand = Expr.Expr_int 1 };
                           };
                           {
                             Expr.pattern = Pattern.P_anything;
                             expr = Expr.Expr_int 999;
                           };
                         ];
                     });
            };
        };
    ]
  in
  let input =
    {|
nestedPm = case value of
  Just (Just x) -> x
  Just _ -> 0
  Nothing -> -1
  _ -> 999|}
  in
  let result = Main.parse input in
  assert_equal expect_data (Result.get_ok result)

let test_complex_cons_pm _ =
  let expect_data =
    [
      Impl.Top_declaration
        {
          Declaration.type_part_data = None;
          body_part =
            {
              Declaration.name = ~?"listSum";
              params = [];
              expr =
                ~?(Expr.Expr_pattern
                     {
                       Expr.expr = Expr.Expr_ident "myList";
                       pattern_data_items =
                         [
                           {
                             Expr.pattern = Pattern.P_list [];
                             expr = Expr.Expr_int 0;
                           };
                           {
                             Expr.pattern =
                               Pattern.P_cons
                                 (Pattern.P_var "x", Pattern.P_list []);
                             expr = Expr.Expr_ident "x";
                           };
                           {
                             Expr.pattern =
                               Pattern.P_cons
                                 ( Pattern.P_var "x",
                                   Pattern.P_cons
                                     (Pattern.P_var "y", Pattern.P_anything) );
                             expr =
                               Expr.Expr_binop
                                 {
                                   Expr.name = "+";
                                   operands =
                                     (Expr.Expr_ident "x", Expr.Expr_ident "y");
                                 };
                           };
                           {
                             Expr.pattern = Pattern.P_anything;
                             expr =
                               Expr.Expr_unop
                                 { name = ~?"-"; operand = Expr.Expr_int 1 };
                           };
                         ];
                     });
            };
        };
    ]
  in
  let input =
    {|
listSum = case myList of
  [] -> 0
  x :: [] -> x
  x :: y :: _ -> x + y
  _ -> -1|}
  in
  let result = Main.parse input in
  assert_equal expect_data (Result.get_ok result)

let test_pipe _ =
  let expect_data =
    [
      Impl.Top_declaration
        {
          Declaration.type_part_data = None;
          body_part =
            {
              Declaration.name = ~?"result";
              params = [];
              expr =
                ~?(Expr.Expr_apply
                     {
                       Expr.fn = Expr.Expr_ident "abcd";
                       arg =
                         Expr.Expr_apply
                           {
                             Expr.fn =
                               Expr.Expr_apply
                                 {
                                   Expr.fn = Expr.Expr_ident "plus";
                                   arg = Expr.Expr_int 1;
                                 };
                             arg = Expr.Expr_int 5;
                           };
                     });
            };
        };
    ]
  in
  let input = {| result = 5 |> plus 1 |> abcd |} in
  let result = input |> Main.parse in
  assert_equal expect_data (Result.get_ok result)

let suite =
  [
    (* "test_decl_string" >:: test_decl_string; *)
    (*  "test_let_in_binop" >:: test_let_in_binop;
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
    "test_apply" >:: test_apply;
    "test_apply_long" >:: test_apply_long;
    "decls_wih_a_lot_of_gaps_and_newlines_between"
    >:: decls_wih_a_lot_of_gaps_and_newlines_between;
    "decl_case_of" >:: decl_case_of;
    "decl_case_of_plus_case_of" >:: decl_case_of_plus_case_of;
    "let_name_equal_expr" >:: let_name_equal_expr;
    "ifthenelse_test" >:: ifthenelse_test;
    "giant_merged_test" >:: giant_merged_test;
    "giant_merged_test2" >:: giant_merged_test2;
    "giant_merged_test3" >:: giant_merged_test3;
    "one_more" >:: one_more;
    "test_access2" >:: test_access2;
    "test_nested_ctor_pm" >:: test_nested_ctor_pm;
    "test_complex_cons_pm" >:: test_complex_cons_pm;
    "test_pipe" >:: test_pipe;
  ]

(*

test = { 1 + 2 }

test = { 
  1 
  
  
  
  + 2 
} 

*)
