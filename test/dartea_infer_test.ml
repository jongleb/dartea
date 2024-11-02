open OUnit2
open Ast.Kind.Frontend
open Data
open Located
module Main = Parse.Main

let test_a_plus_5 _ =
  let expect_type = Typed.Type.TInt in
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

let test_id _ =
  let expect_type = Typed.Type.TInt in
  let expr =
    Expr.Expr_apply { fn = Expr.Expr_ident "id"; arg = Expr.Expr_int 3 }
  in
  let result = Typed.Infer.infer expr in
  assert_equal expect_type result

let test_pattern_matching_returning_ignore_exhaustive _ =
  let expr =
    Expr.Expr_pattern
      {
        expr = Expr_constr { name = "Just"; arguments = [ Expr_string "" ] };
        pattern_data_items =
          [
            { pattern = P_ctor ("None", []); expr = Expr_int 0 };
            { pattern = P_ctor ("Just", [ P_int 2 ]); expr = Expr_int 1 };
          ];
      }
  in
  assert_raises (Failure "Unification failed for int and string") (fun () ->
      Typed.Infer.infer expr)

let test_pattern_matching_returning_ignore_exhaustive_2 _ =
  let expr =
    Expr.Expr_pattern
      {
        expr = Expr_constr { name = "Just"; arguments = [ Expr_string "" ] };
        pattern_data_items =
          [
            { pattern = P_ctor ("None", []); expr = Expr_string "sdfds" };
            { pattern = P_ctor ("Just", [ P_int 2 ]); expr = Expr_int 1 };
          ];
      }
  in
  assert_raises (Failure "Unification failed for int and string") (fun () ->
      Typed.Infer.infer expr)

(* let test_pattern_matching_returning_ignore_exhaustive_3 _ =
   let expr =
     Expr.Expr_pattern
       {
         expr = Expr_constr { name = "Just"; arguments = [ Expr_string "" ] };
         pattern_data_items =
           [
             { pattern = P_ctor ("None", []); expr = Expr_int 0 };
             { pattern = P_ctor ("Just", [ P_anything ]); expr = Expr_int 1 };
           ];
       }
   in
   assert_equal (Typed.Infer.infer expr) Typed.Type.TInt *)

let test_pattern_matching_returning_ignore_exhaustive_3 _ =
  let expr =
    Expr.Expr_pattern
      {
        expr = Expr_constr { name = "Just"; arguments = [ Expr_string "" ] };
        pattern_data_items =
          [
            { pattern = P_ctor ("None", []); expr = Expr_int 0 };
            { pattern = P_ctor ("Just", [ P_anything ]); expr = Expr_int 1 };
          ];
      }
  in
  assert_equal (Typed.Infer.infer expr) Typed.Type.TInt

let test_pattern_matching_returning_ignore_exhaustive_resolve_type_while_matching_case_3
    _ =
  let ctx =
    Typed.(
      Infer.(
        State.reset ();
        let typs = ("test_var_", Type.Scheme ([], new_var "a")) :: typs in
        let f acc (v, scheme) = Map.add v scheme acc in
        List.fold_left f Map.empty typs))
  in
  let expr =
    Expr.Expr_pattern
      {
        expr = Expr_ident "test_var_";
        pattern_data_items =
          [
            { pattern = P_ctor ("None", []); expr = Expr_int 0 };
            { pattern = P_ctor ("Just", [ P_int 3 ]); expr = Expr_int 1 };
            { pattern = P_ctor ("Just", [ P_str "word" ]); expr = Expr_int 1 };
          ];
      }
  in
  assert_raises (Failure "Unification failed for string and int") (fun () ->
      let s, ty = Typed.Infer.infer' expr ctx in
      Typed.Infer.apply_typ ty s)

let test_pattern_matching_returning_ignore_exhaustive_resolve_type_while_matching_case_4
    _ =
  let ctx =
    Typed.(
      Infer.(
        State.reset ();
        let typs = ("test_var_", Type.Scheme ([], new_var "a")) :: typs in
        let f acc (v, scheme) = Map.add v scheme acc in
        List.fold_left f Map.empty typs))
  in
  let expr =
    Expr.Expr_pattern
      {
        expr = Expr_ident "test_var_";
        pattern_data_items =
          [
            { pattern = P_ctor ("None", []); expr = Expr_int 0 };
            { pattern = P_ctor ("Just", [ P_int 3 ]); expr = Expr_int 1 };
          ];
      }
  in
  let s, ty = Typed.Infer.infer' expr ctx in
  let result = Typed.Infer.apply_typ ty s in
  assert_equal result Typed.Type.TInt

let test_pattern_matching_returning_ignore_exhaustive_try_invalid_reuse _ =
  let v = ref None in
  let ctx =
    Typed.(
      Infer.(
        State.reset ();
        v := Some Typed.Infer.(new_var "a");
        let typs = ("test_var_", Type.Scheme ([], Option.get !v)) :: typs in
        let typs =
          ( "maybeMap",
            Type.Scheme
              ( [ "'a"; "'b" ],
                TFun
                  ( TFun (TVar "'a", TVar "'b"),
                    TFun
                      ( TCustom ("Maybe", [ TVar "'a" ]),
                        TCustom ("Maybe", [ TVar "'b" ]) ) ) ) )
          :: typs
        in
        let typs =
          ("notAnonymys", Type.Scheme ([], TFun (TStr, TStr))) :: typs
        in
        let f acc (v, scheme) = Map.add v scheme acc in
        List.fold_left f Map.empty typs))
  in
  let expr =
    Expr.Expr_let
      {
        Expr.binding =
          {
            bind_type = None;
            bind_body =
              {
                name = ~?"_";
                body =
                  Expr.Expr_pattern
                    {
                      expr = Expr_ident "test_var_";
                      pattern_data_items =
                        [
                          { pattern = P_ctor ("None", []); expr = Expr_int 0 };
                          {
                            pattern = P_ctor ("Just", [ P_int 3 ]);
                            expr = Expr_int 1;
                          };
                          {
                            pattern = P_ctor ("Just", [ P_int 2 ]);
                            expr = Expr_int 1;
                          };
                        ];
                    };
              };
          };
        body =
          Expr.Expr_apply
            {
              fn =
                Expr.Expr_apply
                  {
                    fn = Expr.Expr_ident "maybeMap";
                    arg = Expr.Expr_ident "notAnonymys";
                  };
              arg = Expr.Expr_ident "test_var_";
            };
      }
  in
  assert_raises (Failure "Unification failed for string and int") (fun () ->
      let s, ty = Typed.Infer.infer' expr ctx in
      let _ = Typed.Infer.apply_typ ty s in
      ())

let test_constr_infer _ =
  let var = ref None in
  let ctx =
    Typed.(
      Infer.(
        State.reset ();
        var := Some Typed.(Infer.(new_var "a"));
        let typs = ("test_var_", Type.Scheme ([], Option.get !var)) :: typs in
        let f acc (v, scheme) = Map.add v scheme acc in
        List.fold_left f Map.empty typs))
  in
  let expr =
    Expr.Expr_constr { name = "Just"; arguments = [ Expr_ident "test_var_" ] }
  in
  let s, ty = Typed.Infer.infer' expr ctx in
  let result = Typed.Infer.apply_typ ty s in
  assert_equal result (Typed.Type.TCustom ("Maybe", [ Option.get !var ]))

let test_expr exp_str ctx expected ?on_success ?(negate = false) ?withfail () =
  let input =
    {|
toplevel: Int                           
toplevel =|} ^ exp_str
  in
  let result = input |> Main.parse in
  match result with
  | Ok [ Impl.Top_declaration d ] -> (
      match withfail with
      | Some r ->
          assert_raises r (fun () ->
              let s, ty = Typed.Infer.infer' d.body_part.expr.thing ctx in
              ignore @@ Typed.Infer.apply_typ ty s)
      | None -> (
          let s, ty = Typed.Infer.infer' d.body_part.expr.thing ctx in
          let result = Typed.Infer.apply_typ ty s in
          match on_success with
          | Some cb -> cb result
          | None ->
              if negate then
                assert_bool "unexpected equal" @@ not (result = expected)
              else assert_equal result expected))
  | Error e -> raise e
  | _ -> assert false

let test_expr' exp_str ctx expected = test_expr exp_str ctx expected ()

let test_expr_wrong exp_str ctx expected withfail =
  test_expr exp_str ctx expected ~withfail ()

let test_pattern_matching_returning_exhaustive_4 _ =
  let ctx =
    Typed.(
      Infer.(
        State.reset ();
        let typs = ("a", Type.Scheme ([], new_var "a")) :: typs in
        let f acc (v, scheme) = Map.add v scheme acc in
        List.fold_left f Map.empty typs))
  in
  let exp_str =
    {|case a of
  E (C) -> 1
  F (A) -> 2
  G 2   -> 3
  E (D) -> 4
  _ -> 5|}
  in
  test_expr' exp_str ctx Typed.Type.TInt

let test_pattern_matching_returning_exhaustive_5 _ =
  let ctx =
    Typed.(
      Infer.(
        State.reset ();
        let typs = ("a", Type.Scheme ([], new_var "a")) :: typs in
        let typs = ("b", Type.Scheme ([], TTup [ TInt; TStr ])) :: typs in
        let f acc (v, scheme) = Map.add v scheme acc in
        List.fold_left f Map.empty typs))
  in
  let input =
    {|
toplevel: Int                           
toplevel = case a of
  Just ((1, _)) -> let v = int_of_string "600" in pow v 100
  Just ((_, 2)) -> fst b
  Just ((k, 2)) -> pow k 100
  None   -> length (concat "ab" "cd")
  _ -> 5|}
  in
  let result = input |> Main.parse in
  let _expr =
    match result with
    | Ok [ Impl.Top_declaration d ] ->
        let s, ty = Typed.Infer.infer' d.body_part.expr.thing ctx in
        let result = Typed.Infer.apply_typ ty s in
        assert_equal result Typed.Type.TInt
    | Error e -> raise e
    | _ -> assert false
  in

  ()

(* assert_equal (Typed.Infer.infer expr) Typed.Type.TInt *)

(* let expr =
     Expr.Expr_pattern
       {
         expr =
           Expr_constr
             {
               name = "E";
               arguments = [ Expr_constr { name = "C"; arguments = [] } ];
             };
         pattern_data_items =
           [
             { pattern = P_ctor ("E", [ P_ctor ("C", []) ]); expr = Expr_int 1 };
             { pattern = P_ctor ("F", [ P_ctor ("A", []) ]); expr = Expr_int 1 };
             { pattern = P_ctor ("G", [ P_int 2 ]); expr = Expr_int 1 };
             { pattern = P_ctor ("E", [ P_ctor ("D", []) ]); expr = Expr_int 1 };
             {
               pattern = P_ctor ("F", [ P_ctor ("B", [ P_str "Word" ]) ]);
               expr = Expr_int 1;
             };
           ];
       }
   in
   assert_equal (Typed.Infer.infer expr) Typed.Type.TInt *)

let test_unify _ =
  let open Typed.Infer in
  let typ = Typed.Type.(TCustom ("Maybe", [ TInt ])) in
  let typ2 = Typed.Type.TVar "a" in
  let _ = unify typ typ2 in
  assert_equal 1 1

let test_pattern_matching_returning_ignore_exhaustive_resolve_type_while_matching_case_4_2
    _ =
  let ctx =
    Typed.(
      Infer.(
        State.reset ();
        let typs = ("test_var_", Type.Scheme ([], new_var "a")) :: typs in
        let f acc (v, scheme) = Map.add v scheme acc in
        List.fold_left f Map.empty typs))
  in
  let expr =
    Expr.Expr_pattern
      {
        expr = Expr_ident "test_var_";
        pattern_data_items =
          [
            { pattern = P_ctor ("None", []); expr = Expr_int 0 };
            (* {
                 pattern = P_ctor ("Just", [ P_ctor ("Just", [ P_int 3 ]) ]);
                 expr = Expr_int 1;
               };
               { pattern = P_ctor ("Just", [ P_anything ]); expr = Expr_int 6 }; *)
          ];
      }
  in
  let s, ty = Typed.Infer.infer' expr ctx in
  let result = Typed.Infer.apply_typ ty s in
  assert_equal result Typed.Type.TInt

let test_specialization_tree _ =
  let actions = [ 1; 2; 3 ] in
  let patterns =
    Typed.Pattern.Typed.
      [
        [
          {
            typ = Typed.Type.TCustom ("Maybe", [ TInt; TInt ]);
            pattern =
              P_T_ctor
                ( "Just",
                  [
                    { typ = TInt; pattern = P_T_int 1 };
                    { typ = TInt; pattern = P_T_anything };
                  ] );
          };
        ];
        [
          {
            typ = TCustom ("Maybe", [ TInt; TInt ]);
            pattern =
              P_T_ctor
                ( "Just",
                  [
                    { typ = TInt; pattern = P_T_anything };
                    { typ = TInt; pattern = P_T_int 2 };
                  ] );
          };
        ];
        [
          {
            typ = TCustom ("Maybe", [ TTup [ TInt; TInt ] ]);
            pattern = P_T_ctor ("None", []);
          };
        ];
      ]
  in
  let node = Typed.Pattern.Compile_state.{ patterns; actions } in
  let result =
    Typed.Pattern.specialization node
      {
        typ = Typed.Type.TCustom ("Maybe", [ TInt; TInt ]);
        pattern =
          P_T_ctor
            ( "Just",
              [
                { typ = TInt; pattern = P_T_int 1 };
                { typ = TInt; pattern = P_T_anything };
              ] );
      }
  in

  assert_bool "actions are not equal"
    (List.equal Int.equal result.actions [ 1; 2 ]);

  let expected_patterns =
    Typed.(
      Pattern.Typed.
        [
          [
            { typ = TInt; pattern = P_T_int 1 };
            { typ = TInt; pattern = P_T_anything };
          ];
          [
            { typ = TInt; pattern = P_T_anything };
            { typ = TInt; pattern = P_T_int 2 };
          ];
        ])
  in
  let patterns_equal a b = List.equal Typed.Pattern.Typed.equal a b in

  assert_bool "patterns are not equal"
    (List.equal patterns_equal result.patterns expected_patterns)

let test_defaulting_tree _ =
  let actions = [ 1; 2 ] in
  let patterns =
    Typed.(
      Pattern.(
        Typed.
          [
            [
              { typ = TInt; pattern = P_T_int 1 };
              { typ = TInt; pattern = P_T_anything };
            ];
            [
              { typ = TInt; pattern = P_T_anything };
              { typ = TInt; pattern = P_T_int 2 };
            ];
          ]))
  in
  let node = { Typed.Pattern.Compile_state.patterns; actions } in
  let result = Typed.Pattern.defaulting node in
  assert_bool "actions are not equal"
    (List.equal Int.equal result.actions [ 2 ]);
  let expected_patterns =
    Typed.(
      Pattern.Typed.
        [
          [
            { typ = TInt; pattern = P_T_anything };
            { typ = TInt; pattern = P_T_int 2 };
          ];
        ])
  in
  let patterns_equal a b = List.equal Typed.Pattern.Typed.equal a b in
  assert_bool "patterns are not equal"
    (List.equal patterns_equal result.patterns expected_patterns)

let test_no_errors_compile _ =
  let actions = [ 1; 2; 3 ] in
  let patterns =
    Typed.Pattern.Typed.
      [
        [
          {
            typ = Typed.Type.TCustom ("Maybe", [ TInt; TInt ]);
            pattern =
              P_T_ctor
                ( "Just",
                  [
                    { typ = TInt; pattern = P_T_int 1 };
                    { typ = TInt; pattern = P_T_anything };
                  ] );
          };
        ];
        [
          {
            typ = TCustom ("Maybe", [ TInt; TInt ]);
            pattern =
              P_T_ctor
                ( "Just",
                  [
                    { typ = TInt; pattern = P_T_anything };
                    { typ = TInt; pattern = P_T_int 2 };
                  ] );
          };
        ];
        [
          {
            typ = TCustom ("Maybe", [ TTup [ TInt; TInt ] ]);
            pattern = P_T_ctor ("None", []);
          };
        ];
      ]
  in
  let node = Typed.Pattern.Compile_state.{ patterns; actions } in
  let result = Typed.Pattern.compile node in
  assert_bool "exhaustive" @@ not @@ Typed.Pattern.is_exhaustive' result

let test_detupling _ =
  let pattern =
    Typed.Pattern.Typed.
      {
        typ =
          Typed.Type.TTup
            [
              TTup
                [
                  TTup [ TInt; TInt ]; TTup [ TInt; TTup [ TInt; TInt ] ]; TInt;
                ];
            ];
        pattern =
          P_T_tuple
            [
              {
                pattern =
                  P_T_tuple
                    [
                      { pattern = P_T_int 1; typ = TInt };
                      { pattern = P_T_int 2; typ = TInt };
                    ];
                typ = TTup [ TInt; TInt ];
              };
              {
                pattern =
                  P_T_tuple
                    [
                      { pattern = P_T_int 3; typ = TInt };
                      {
                        pattern =
                          P_T_tuple
                            [
                              { pattern = P_T_int 4; typ = TInt };
                              { pattern = P_T_int 5; typ = TInt };
                            ];
                        typ = TTup [ TInt; TTup [ TInt; TInt ] ];
                      };
                    ];
                typ = TTup [ TInt; TInt ];
              };
              { pattern = P_T_int 6; typ = TInt };
            ];
      }
  in
  let result = Typed.Pattern.Matrix.detuple pattern pattern in
  let expected =
    Typed.Pattern.Typed.
      [
        { pattern = P_T_int 1; typ = TInt };
        { pattern = P_T_int 2; typ = TInt };
        { pattern = P_T_int 3; typ = TInt };
        { pattern = P_T_int 4; typ = TInt };
        { pattern = P_T_int 5; typ = TInt };
        { pattern = P_T_int 6; typ = TInt };
      ]
  in
  let nextrow =
    Typed.Pattern.Typed.
      {
        pattern =
          P_T_tuple
            [
              { pattern = P_T_anything; typ = TTup [ TInt; TInt ] };
              {
                pattern = P_T_anything;
                typ = TTup [ TInt; TTup [ TInt; TInt ] ];
              };
              { pattern = P_T_anything; typ = TInt };
            ];
        typ =
          TTup [ TTup [ TInt; TInt ]; TTup [ TInt; TTup [ TInt; TInt ] ]; TInt ];
      }
  in
  let nextresult =
    Typed.Pattern.Typed.
      [
        { pattern = P_T_anything; typ = TInt };
        { pattern = P_T_anything; typ = TInt };
        { pattern = P_T_anything; typ = TInt };
        { pattern = P_T_anything; typ = TInt };
        { pattern = P_T_anything; typ = TInt };
        { pattern = P_T_anything; typ = TInt };
      ]
  in
  assert_bool "patterns are not equal"
    (List.equal Typed.Pattern.Typed.equal result expected);
  assert_bool "patterns are not equal"
    (List.equal Typed.Pattern.Typed.equal
       (Typed.Pattern.Matrix.detuple pattern nextrow)
       nextresult)

let test_no_errors_compile_exhaustive_2 _ =
  let actions = [ 1; 2; 3; 4 ] in
  let patterns =
    Typed.Pattern.Typed.
      [
        [
          {
            typ = Typed.Type.(TCustom ("Maybe", [ TTup [ TInt; TInt ] ]));
            pattern =
              P_T_ctor
                ( "Just",
                  [
                    {
                      pattern =
                        P_T_tuple
                          [
                            { typ = TInt; pattern = P_T_int 1 };
                            { typ = TInt; pattern = P_T_anything };
                          ];
                      typ = TTup [ TInt; TInt ];
                    };
                  ] );
          };
        ];
        [
          {
            typ = Typed.Type.(TCustom ("Maybe", [ TTup [ TInt; TInt ] ]));
            pattern =
              P_T_ctor
                ( "Just",
                  [
                    {
                      pattern =
                        P_T_tuple
                          [
                            { typ = TInt; pattern = P_T_anything };
                            { typ = TInt; pattern = P_T_int 2 };
                          ];
                      typ = TTup [ TInt; TInt ];
                    };
                  ] );
          };
        ];
        [
          {
            typ = Typed.Type.(TCustom ("Maybe", [ TTup [ TInt; TInt ] ]));
            pattern =
              P_T_ctor
                ( "Just",
                  [
                    {
                      pattern =
                        P_T_tuple
                          [
                            { typ = TInt; pattern = P_T_anything };
                            { typ = TInt; pattern = P_T_anything };
                          ];
                      typ = TTup [ TInt; TInt ];
                    };
                  ] );
          };
        ];
        [
          {
            typ = Typed.Type.(TCustom ("Maybe", [ TTup [ TInt; TInt ] ]));
            pattern = P_T_ctor ("None", []);
          };
        ];
      ]
  in
  let node = Typed.Pattern.Compile_state.{ patterns; actions } in
  let result = Typed.Pattern.compile node in
  assert_bool "not exhaustive" @@ Typed.Pattern.is_exhaustive' result

let infer_check_exhaustive _ =
  let ctx =
    Typed.(
      Infer.(
        State.reset ();
        let typs = ("test_var_", Type.Scheme ([], new_var "a")) :: typs in
        let f acc (v, scheme) = Map.add v scheme acc in
        List.fold_left f Map.empty typs))
  in
  let expr =
    Expr.Expr_pattern
      {
        expr = Expr_ident "test_var_";
        pattern_data_items =
          [
            {
              pattern = P_ctor ("Just", [ P_tuple [ P_int 1; P_anything ] ]);
              expr = Expr_int 1;
            };
            {
              pattern = P_ctor ("Just", [ P_tuple [ P_anything; P_int 2 ] ]);
              expr = Expr_int 2;
            };
            {
              pattern = P_ctor ("Just", [ P_tuple [ P_anything; P_anything ] ]);
              expr = Expr_int 3;
            };
            { pattern = P_ctor ("None", []); expr = Expr_int 4 };
          ];
      }
  in
  let s, ty = Typed.Infer.infer' expr ctx in
  let result = Typed.Infer.apply_typ ty s in
  assert_equal result Typed.Type.TInt

let infer_check_not_exhaustive _ =
  let ctx =
    Typed.(
      Infer.(
        State.reset ();
        let typs = ("test_var_", Type.Scheme ([], new_var "a")) :: typs in
        let f acc (v, scheme) = Map.add v scheme acc in
        List.fold_left f Map.empty typs))
  in
  let expr =
    Expr.Expr_pattern
      {
        expr = Expr_ident "test_var_";
        pattern_data_items =
          [
            {
              pattern = P_ctor ("Just", [ P_tuple [ P_int 1; P_anything ] ]);
              expr = Expr_int 1;
            };
            {
              pattern = P_ctor ("Just", [ P_tuple [ P_anything; P_int 2 ] ]);
              expr = Expr_int 2;
            };
            { pattern = P_ctor ("None", []); expr = Expr_int 3 };
          ];
      }
  in
  assert_raises (Failure "Not exhaustive") (fun () ->
      let s, ty = Typed.Infer.infer' expr ctx in
      let result = Typed.Infer.apply_typ ty s in
      assert_equal result Typed.Type.TInt)

let infer_check_exhaustive_p_var _ =
  let ctx =
    Typed.(
      Infer.(
        State.reset ();
        let typs = ("test_var_", Type.Scheme ([], new_var "a")) :: typs in
        let f acc (v, scheme) = Map.add v scheme acc in
        List.fold_left f Map.empty typs))
  in
  let expr =
    Expr.Expr_pattern
      {
        expr = Expr_ident "test_var_";
        pattern_data_items =
          [
            { pattern = P_ctor ("Just", [ P_int 1 ]); expr = Expr_int 1 };
            {
              pattern = P_ctor ("Just", [ P_var "abc" ]);
              expr =
                Expr.Expr_apply
                  {
                    fn = Expr.Expr_ident "length";
                    arg =
                      Expr_apply
                        {
                          fn =
                            Expr_apply
                              {
                                fn = Expr.Expr_ident "concat";
                                arg = Expr_ident "abc";
                              };
                          arg = Expr_ident "abc";
                        };
                  };
            };
            { pattern = P_ctor ("None", []); expr = Expr_int 4 };
          ];
      }
  in
  assert_raises (Failure "Unification failed for string and int") (fun () ->
      let s, ty = Typed.Infer.infer' expr ctx in
      let result = Typed.Infer.apply_typ ty s in
      assert_equal result Typed.Type.TInt)

let test_expr_access_with_type_infer _ =
  let ctx =
    Typed.(
      Infer.(
        State.reset ();
        let typs =
          ( "a",
            Type.Scheme
              ( [],
                Type.TRecord
                  (Type.TRowExtend ("abc", Type.TInt, Type.TRowEmpty)) ) )
          :: typs
        in
        let f acc (v, scheme) = Map.add v scheme acc in
        List.fold_left f Map.empty typs))
  in
  let exp_str = {|a.abc|} in
  test_expr' exp_str ctx TInt

let test_expr_access_with_type_infer_negate _ =
  let ctx =
    Typed.(
      Infer.(
        State.reset ();
        let typs =
          ( "a",
            Type.Scheme
              ( [],
                Type.TRecord
                  (Type.TRowExtend ("abc", Type.TInt, Type.TRowEmpty)) ) )
          :: typs
        in
        let f acc (v, scheme) = Map.add v scheme acc in
        List.fold_left f Map.empty typs))
  in
  let exp_str = {|a.abc|} in
  test_expr exp_str ctx TStr ~negate:true ()

let test_expr_small_program _ =
  let ctx =
    Typed.(
      Infer.(
        State.reset ();
        let typs =
          ( "a",
            Type.Scheme
              ( [],
                Type.TRecord
                  (Type.TRowExtend ("abc", Type.TInt, Type.TRowEmpty)) ) )
          :: typs
        in
        let f acc (v, scheme) = Map.add v scheme acc in
        List.fold_left f Map.empty typs))
  in
  let exp_str = {|
    let a = 3 in
    let b = 5 in
    let c = {  }
  |} in
  test_expr exp_str ctx TStr ~negate:true ()

let suite =
  [
    "test_a_plus_5" >:: test_a_plus_5;
    "test_id" >:: test_id;
    "test_pattern_matching_returning_ignore_exhaustive"
    >:: test_pattern_matching_returning_ignore_exhaustive;
    "test_pattern_matching_returning_ignore_exhaustive_2"
    >:: test_pattern_matching_returning_ignore_exhaustive_2;
    "test_pattern_matching_returning_ignore_exhaustive_3"
    >:: test_pattern_matching_returning_ignore_exhaustive_3;
    "test_pattern_matching_returning_exhaustive_4"
    >:: test_pattern_matching_returning_exhaustive_4;
    "test_pattern_matching_returning_ignore_exhaustive_resolve_type_while_matching_case_3"
    >:: test_pattern_matching_returning_ignore_exhaustive_resolve_type_while_matching_case_3;
    (* "test_pattern_matching_returning_ignore_exhaustive_resolve_type_while_matching_case_4"
       >:: test_pattern_matching_returning_ignore_exhaustive_resolve_type_while_matching_case_4; *)
    (* "test_pattern_matching_returning_ignore_exhaustive_try_invalid_reuse"
       >:: test_pattern_matching_returning_ignore_exhaustive_try_invalid_reuse; *)
    (* "test_pattern_matching_returning_ignore_exhaustive_resolve_type_while_matching_case_4_2"
       >:: test_pattern_matching_returning_ignore_exhaustive_resolve_type_while_matching_case_4_2; *)
    (* "test_constr_infer" >:: test_constr_infer;
       "test_unify" >:: test_unify;
       "test_specialization_tree" >:: test_specialization_tree;
       "test_defaulting_tree" >:: test_defaulting_tree;
       "test_no_errors_compile" >:: test_no_errors_compile;
       "test_detupling" >:: test_detupling;
       "test_no_errors_compile_exhaustive_2"
       >:: test_no_errors_compile_exhaustive_2;*)
    "infer_check_exhaustive" >:: infer_check_exhaustive;
    "infer_check_exhaustive_p_var" >:: infer_check_exhaustive_p_var;
    "test_pattern_matching_returning_exhaustive_5"
    >:: test_pattern_matching_returning_exhaustive_5;
    "test_expr_access_with_type_infer" >:: test_expr_access_with_type_infer;
    "test_expr_access_with_type_infer_negate"
    >:: test_expr_access_with_type_infer_negate;
  ]
(* CCImmutArray.iteri
   (fun idx pat ->
     CCImmutArray.iteri
       (fun idx2 pat ->
         prerr_endline
         @@ Printf.sprintf "%n:%n:%s" idx idx2
              (Typed.Infer.Pattern_typed.show pat))
       pat)
   result.patterns *)
