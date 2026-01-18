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
    List.map
      (fun x ->
        x >>| fun impl_list ->
        let frontend_module = Frontend.Module.of_impl impl_list in
        Canonical.Module.of_frontend frontend_module)
      result
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

  let check_exhaustiveness (arity_env : int Infer.Infer_proc.Map.t) (decl : Typed.Declaration.t) =
    let open Typed.Expr in
    let rec check_expr (expr : Typed.Expr.t) =
      match expr.expr with
      | Expr_pattern pattern_match ->
          let patterns =
            List.map
              (fun (case : expr_pattern_case) -> case.pattern)
              pattern_match.pattern_data_items
          in
          let warnings =
            if not (After_typed.Exhaustive.is_exhaustive arity_env patterns) then
              [
                Printf.sprintf "Warning: non-exhaustive pattern match in %s"
                  decl.name.thing;
              ]
            else []
          in
          let scrutinee_warnings = check_expr pattern_match.expr in
          let cases_warnings =
            List.concat_map
              (fun (case : expr_pattern_case) -> check_expr case.expr)
              pattern_match.pattern_data_items
          in
          warnings @ scrutinee_warnings @ cases_warnings
      | Expr_let let_expr ->
          check_expr let_expr.binding.bind_body.body @ check_expr let_expr.body
      | Expr_if_then_else ite ->
          check_expr ite.if_exp @ check_expr ite.then_exp
          @ check_expr ite.else_exp
      | Expr_apply apply -> check_expr apply.fn @ check_expr apply.arg
      | Expr_binop binop ->
          let left, right = binop.operands in
          check_expr left @ check_expr right
      | Expr_constr constr -> List.concat_map check_expr constr.arguments
      | Expr_list exprs -> List.concat_map check_expr exprs
      | Expr_lambda lambda -> check_expr lambda.body
      | Expr_access access -> check_expr access.expr
      | Expr_record rows ->
          List.concat_map
            (fun (row : expr_record_row) -> check_expr row.value)
            rows
      | Expr_ident _ | Expr_accessor _ | Expr_record_extend _
      | Expr_record_select _ | Expr_record_empty | Expr_char _ | Expr_string _
      | Expr_int _ | Expr_float _ ->
          []
    in
    check_expr decl.body
  in

  let warnings =
    List.concat_map
      (fun x ->
        match x with
        | Ok (Infer.Infer_proc.{ arity_env; declarations; _ }) ->
            List.concat_map (check_exhaustiveness arity_env) declarations
        | Error _ -> [])
      typed
  in
  List.iter (fun x -> x |> Printf.sprintf "%s\n" |> prerr_endline) warnings;

  List.iter
    (fun x ->
      match x with
      | Ok (Infer.Infer_proc.{ declarations; _ }) ->
          List.iter
            (fun (decl : Typed.Declaration.t) ->
              Printf.printf "%s : %s\n" decl.name.thing
                (Infer.Infer_proc.string_of_typ decl.typ))
            declarations
      | Error x -> raise x)
    typed;
  prerr_endline "Success!"
