module Module_map = struct
  open Base

  module T = struct
    type t = string [@@deriving sexp, compare]
  end

  include T
  include Base.Comparable.Make (T)
end

let compile path =
  let std =
    let open Infer in
    let open Type in
    [
      ("pow", Typed.Type.Scheme ([], TFun (TInt, TFun (TInt, TInt))));
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
      ("-", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
      ("*", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
      ("/", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
      ("id", Scheme ([ "'a" ], TFun (TVar "'a", TVar "'a")));
      ( "const",
        Scheme ([ "'a"; "'b" ], TFun (TVar "'a", TFun (TVar "'b", TVar "'a")))
      );
      ( "pair",
        Scheme
          ( [ "'a"; "'b" ],
            TFun (TVar "'a", TFun (TVar "'b", TTup [ TVar "'a"; TVar "'b" ])) )
      );
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
  in
  let open File_loader.Files in
  let open Base.Result.Let_syntax in
  let result = current_folder path in
  let result =
    List.map
      (fun Elm_file.{ path; content } ->
        let open Ast.Kind.Frontend.Module in
        Parse.Main.parse content)
      result
  in
  let canonicalized =
    List.map (fun x -> x >>| List.map Canonical.Impl.of_frontend) result
  in
  let initial_ctx =
    let f acc (v, scheme) = Infer.Infer_proc.Map.add v scheme acc in
    List.fold_left f Infer.Infer_proc.Map.empty std
  in

  let typed =
    List.map
      (fun x ->
        Result.map
          (fun declarations ->
            Infer.Infer_proc.State.reset ();
            Infer.Infer_proc.infer_toplevel declarations initial_ctx)
          x)
      canonicalized
  in

  List.iter
    (fun x ->
      match x with
      | Ok (_, decls) ->
          List.iter
            (fun (decl : Typed.Declaration.t) ->
              Printf.printf "%s : %s\n" decl.name.thing
                (Infer.Infer_proc.string_of_typ decl.typ))
            decls
      | Error x -> raise x)
    typed;
  prerr_endline "Success!"
