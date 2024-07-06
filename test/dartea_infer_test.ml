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

let test_pattern_matching_returning_exhaustive_4 _ =
  let expr =
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
  assert_equal (Typed.Infer.infer expr) Typed.Type.TInt

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

(* let _ =
   let open Typed in
   let open Infer in
   let open Type in
   (* let fn_apply (s1, ty1) =
        let s2, ty2 = infer case_expr (apply_ctx ctx s1) in
        let s4 = unify (apply_typ ty1 s1) (apply_typ match_ s0) in
        (s1 ++ s4, apply_typ ty2 s4)
      in *)
   (* let a =
        Typed.Infer.(
          let open Type in
          Map.of_seq (List.to_seq [ ("var", TStr) ]))
      in

      let b =
        Typed.Infer.(
          let open Type in
          Map.empty)
      in *)
   (* let case_a =
        Expr.{ pattern = P_ctor ("Just", [ P_int 0 ]); expr = Expr_string "Zero" }
      in
      let case_b =
        Expr.{ pattern = P_ctor ("None", []); expr = Expr_string "Zero" }
      in
      let case_c =
        Expr.
          {
            pattern = P_ctor ("Just", [ P_var "v" ]);
            expr = Expr_string "More then null";
          } *)
   let s0, match_ = (Map.empty, TCustom ("Maybe", [ TInt ])) in
   let s1, ty1 =

     ( Map.of_seq (List.to_seq [ ("val2", TBool); ("val3", TInt) ]),
       TCustom ("Maybe", [ TVar "val" ]) )
   in
   let s2, ty2 =
     (Map.of_seq (List.to_seq [ ("val4", TBool); ("val5", TInt) ]), TStr)
   in
   let s4 = unify (apply_typ ty1 s1) (apply_typ match_ s0) in
   let result, type_result = (s1 ++ s2 ++ s4, apply_typ ty2 s4) in
   Map.iter
     (fun k v ->
       print_endline "k";
       print_endline k;
       print_endline "v";
       print_endline @@ show v)
     result;
   print_endline @@ show type_result *)

let test_specialization_tree _ =
  let arguments = [ "$" ] in
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
  let node = Typed.Pattern.Compile_state.{ arguments; patterns; actions } in
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
  assert_bool "arguments are not equal"
    (List.equal String.equal result.arguments [ "[$].0" ]);

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
  let arguments = [ "[$].0" ] in
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
  let node = { Typed.Pattern.Compile_state.arguments; patterns; actions } in
  let result = Typed.Pattern.defaulting node in
  assert_bool "arguments are not equal"
    (List.equal String.equal result.arguments [ "[$].0" ]);
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
  let arguments = [ "$" ] in
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
  let node = Typed.Pattern.Compile_state.{ arguments; patterns; actions } in
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
  let arguments = [ "$" ] in
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
  let node = Typed.Pattern.Compile_state.{ arguments; patterns; actions } in
  let result = Typed.Pattern.compile node in
  assert_bool "not exhaustive" @@ Typed.Pattern.is_exhaustive' result

let suite =
  [
    (* "test_a_plus_5" >:: test_a_plus_5;
       "test_id" >:: test_id; *)
    (* "test_pattern_matching_returning_ignore_exhaustive"
       >:: test_pattern_matching_returning_ignore_exhaustive;
       "test_pattern_matching_returning_ignore_exhaustive_2"
       >:: test_pattern_matching_returning_ignore_exhaustive_2;
       "test_pattern_matching_returning_ignore_exhaustive_3"
       >:: test_pattern_matching_returning_ignore_exhaustive_3;
       "test_pattern_matching_returning_exhaustive_4"
       >:: test_pattern_matching_returning_exhaustive_4;
       "test_pattern_matching_returning_ignore_exhaustive_resolve_type_while_matching_case_3"
       >:: test_pattern_matching_returning_ignore_exhaustive_resolve_type_while_matching_case_3;
       "test_pattern_matching_returning_ignore_exhaustive_resolve_type_while_matching_case_4"
       >:: test_pattern_matching_returning_ignore_exhaustive_resolve_type_while_matching_case_4; *)
    (* "test_pattern_matching_returning_ignore_exhaustive_try_invalid_reuse"
       >:: test_pattern_matching_returning_ignore_exhaustive_try_invalid_reuse; *)
    (* "test_pattern_matching_returning_ignore_exhaustive_resolve_type_while_matching_case_4_2"
       >:: test_pattern_matching_returning_ignore_exhaustive_resolve_type_while_matching_case_4_2; *)
    (* "test_constr_infer" >:: test_constr_infer; *)
    (* "test_pattern_trie1" >:: test_pattern_trie1; *)
    (* "test_unify" >:: test_unify; *)
    "test_specialization_tree" >:: test_specialization_tree;
    "test_defaulting_tree" >:: test_defaulting_tree;
    "test_no_errors_compile" >:: test_no_errors_compile;
    "test_detupling" >:: test_detupling;
    "test_no_errors_compile_exhaustive_2"
    >:: test_no_errors_compile_exhaustive_2;
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
