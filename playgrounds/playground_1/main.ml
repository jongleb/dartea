let typs =
  let open Typed in
  let open Type in
  [
    ("pow", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
    ("=", Scheme ([ "'a" ], TFun (TVar "'a", TFun (TVar "'a", TBool))));
    ("<>", Scheme ([ "'a" ], TFun (TVar "'a", TFun (TVar "'a", TBool))));
    ("&&", Scheme ([], TFun (TBool, TFun (TBool, TBool))));
    ("||", Scheme ([], TFun (TBool, TFun (TBool, TBool))));
    ("+", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
    ("plus", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
    ("concat", Scheme ([], TFun (TStr, TFun (TStr, TStr))));
    ("length", Scheme ([], TFun (TStr, TInt)));
    ("int_to_string", Scheme ([], TFun (TInt, TStr)));
    ("int_of_string", Scheme ([], TFun (TStr, TInt)));
    (* while operators aren't supported *)
    ("-", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
    ("*", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
    ("/", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
    ("id", Scheme ([ "'a" ], TFun (TVar "'a", TVar "'a")));
    ( "const",
      Scheme ([ "'a"; "'b" ], TFun (TVar "'a", TFun (TVar "'b", TVar "'a"))) );
    ( "pair",
      Scheme
        ( [ "'a"; "'b" ],
          TFun (TVar "'a", TFun (TVar "'b", TTup [ TVar "'a"; TVar "'b" ])) ) );
    ( "fst",
      Scheme ([ "'a"; "'b" ], TFun (TTup [ TVar "'a"; TVar "'b" ], TVar "'a"))
    );
    ( "snd",
      Scheme ([ "'a"; "'b" ], TFun (TTup [ TVar "'a"; TVar "'b" ], TVar "'b"))
    );
    ( "|>",
      Scheme
        ( [ "'a"; "'b" ],
          TFun (TVar "'a", TFun (TFun (TVar "'a", TVar "'b"), TVar "'b")) ) );
  ]

let run_playground typs_ =
  let result = File_loader.Files.current_folder "playgrounds/elm_code" in
  let parsed =
    List.map
      (fun i -> Parse.Main.parse i.File_loader.Files.Elm_file.content)
      result
  in
  let canonicalized =
    List.map
      (fun x -> Result.map (List.map Canonical.Impl.of_frontend) x)
      parsed
  in
  let typed =
    List.map
      (fun x ->
        Result.map
          (List.map (fun x ->
               match x with
               | Canonical.Impl.Top_declaration x ->
                   let ctx =
                     Typed.(
                       Infer.(
                         State.reset ();
                         let typs =
                           List.fold_left
                             (fun acc next ->
                               ( Data.Located.unwrap next,
                                 Type.Scheme ([], new_var "a") )
                               :: acc)
                             typs_ x.params
                         in
                         let f acc (v, scheme) = Map.add v scheme acc in
                         List.fold_left f Map.empty typs))
                   in
                   let s, ty = Typed.Infer.infer' x.expr ctx in
                   Typed.Infer.apply_typ ty s))
          x)
      canonicalized
  in
  List.iter (fun x -> match x with Ok _ -> () | Error x -> raise x) typed;
  prerr_endline "Success!"

let () = run_playground typs
