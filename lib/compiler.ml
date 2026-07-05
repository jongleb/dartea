module Module_map = struct
  open Base

  module T = struct
    type t = string [@@deriving sexp, compare]
  end

  include T
  include Base.Comparable.Make (T)
end

let std =
  let open Infer in
  let open Type in
  [
      ("pow", Typed.Type.Scheme ([], TFun (TInt, TFun (TInt, TInt))));
      ("=", Scheme ([ "'a" ], TFun (TVar "'a", TFun (TVar "'a", TBool))));
      ("<>", Scheme ([ "'a" ], TFun (TVar "'a", TFun (TVar "'a", TBool))));

      ("==", Scheme ([ "'a" ], TFun (TVar "'a", TFun (TVar "'a", TBool))));
      (">", Scheme ([], TFun (TInt, TFun (TInt, TBool))));
      ("<", Scheme ([], TFun (TInt, TFun (TInt, TBool))));
      ("&&", Scheme ([], TFun (TBool, TFun (TBool, TBool))));
      ("||", Scheme ([], TFun (TBool, TFun (TBool, TBool))));
      ("+", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
      ("plus", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
      ("concat", Scheme ([], TFun (TStr, TFun (TStr, TStr))));
      ("++", Scheme ([], TFun (TStr, TFun (TStr, TStr))));
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

let initial_ctx =
  let f acc (v, scheme) = Infer.Infer_proc.Map.add v scheme acc in
  List.fold_left f Infer.Infer_proc.Map.empty std

let compile_string (content : string) : string =
  match Parse.Main.parse content with
  | Error e -> raise e
  | Ok impl_list ->
      let frontend_module = Frontend.Module.of_impl impl_list in
      let canonical = Canonical.Module.of_frontend frontend_module in
      Infer.Infer_proc.State.reset ();
      let result = Infer.Infer_proc.infer_toplevel canonical initial_ctx in
      let optimized = After_typed.Optimize.optimize result.declarations in
      let sorted = After_typed.Dependency_sort.sort_declarations optimized in
      let constructors =
        List.map
          (fun (c : Infer.Infer_proc.ctor_info) -> (c.name, c.arity))
          result.constructors
      in
      let js =
        Codegen.Js_of_optimized.program_with_helpers ~constructors sorted
      in
      Codegen.Js_of_optimized.runtime_prelude
      ^ Codegen.Js_to_string.program_to_string js

let compile path =
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

  let check_exhaustiveness (arity_env : int Infer.Infer_proc.Map.t)
      (decl : Typed.Declaration.t) =
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
            if not (After_typed.Exhaustive.is_exhaustive arity_env patterns)
            then
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
        | Ok Infer.Infer_proc.{ arity_env; declarations; _ } ->
            List.concat_map (check_exhaustiveness arity_env) declarations
        | Error _ -> [])
      typed
  in
  List.iter (fun x -> x |> Printf.sprintf "%s\n" |> prerr_endline) warnings;

  let optimized =
    List.map
      (fun x ->
        Result.map
          (fun (result : Infer.Infer_proc.infer_result) ->
            After_typed.Optimize.optimize result.declarations)
          x)
      typed
  in

  (* Check for errors in optimized code *)
  List.iter (fun x -> match x with Ok _ -> () | Error x -> raise x) optimized;

  let sorted =
    List.map
      (fun x -> Result.map After_typed.Dependency_sort.sort_declarations x)
      optimized
  in

  let constructors_per_file =
    List.map
      (Result.map (fun (r : Infer.Infer_proc.infer_result) ->
           List.map
             (fun (c : Infer.Infer_proc.ctor_info) -> (c.name, c.arity))
             r.constructors))
      typed
  in

  let js_programs =
    List.filter_map
      (fun (sorted_r, ctors_r) ->
        match (sorted_r, ctors_r) with
        | Ok declarations, Ok constructors ->
            Some
              (Codegen.Js_of_optimized.program_with_helpers ~constructors
                 declarations)
        | Ok declarations, Error _ ->
            Some (Codegen.Js_of_optimized.program_with_helpers declarations)
        | Error _, _ -> None)
      (List.combine sorted constructors_per_file)
  in

  List.iter
    (fun js_program ->
      let js_code =
        Codegen.Js_of_optimized.runtime_prelude
        ^ Codegen.Js_to_string.program_to_string js_program
      in
      Printf.printf "\n=== Generated JavaScript ===\n%s\n" js_code)
    js_programs;

  prerr_endline "Success!"
