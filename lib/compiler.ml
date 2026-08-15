module Module_map = struct
  open Base

  module T = struct
    type t = string [@@deriving sexp, compare]
  end

  include T
  include Base.Comparable.Make (T)
end

let initial_ctx =
  let f acc (v, scheme) =
    Infer.Infer_proc.Name_map.add (Data.Name.local v) scheme acc
  in
  List.fold_left f Infer.Infer_proc.Name_map.empty Builtins.values

module type BACKEND = sig
  val emit :
    constructors:(Data.Name.t * int) list ->
    siblings:(Data.Name.t * (Data.Name.t * int) list) list ->
    Optimized.Declaration.t list ->
    string
end

module Js_backend : BACKEND = struct
  let emit ~constructors ~siblings decls =
    Codegen.Js_of_optimized.emit ~constructors ~siblings decls
end

module Make (B : BACKEND) = struct
  let compile_string (content : string) : string =
    match Parse.Main.parse content with
    | Error e -> raise e
    | Ok impl_list ->
        let frontend_module = Frontend.Module.of_impl impl_list in
        let canonical = Canonical.Module.of_frontend frontend_module in
        Infer.Infer_proc.State.reset ();
        let result = Infer.Infer_proc.infer_toplevel ~imports:[] canonical initial_ctx in
        let optimized = After_typed.Optimize.optimize result.declarations in
        let sorted = After_typed.Dependency_sort.sort_declarations optimized in
        let constructors =
          List.map
            (fun (c : Infer.Infer_proc.ctor_info) -> (c.name, c.arity))
            result.constructors
        in
        let siblings =
          Infer.Infer_proc.Name_map.bindings result.siblings_env
        in
        B.emit ~constructors ~siblings sorted
end

include Make (Js_backend)

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
            Infer.Infer_proc.infer_toplevel ~imports:[] declarations initial_ctx)
          x)
      canonicalized
  in

  let check_exhaustiveness
      (siblings_env : (Data.Name.t * int) list Infer.Infer_proc.Name_map.t)
      (decl : Typed.Declaration.t) =
    let open Typed.Expr in
    let rec check_expr (expr : Typed.Expr.t) =
      match expr.expr with
      | Expr_pattern pattern_match ->
          let patterns =
            List.map
              (fun (case : expr_pattern_case) ->
                After_typed.Typed_to_optimized.pattern_of_typed case.pattern)
              pattern_match.pattern_data_items
          in
          let warnings =
            if After_typed.Exhaustive.is_exhaustive siblings_env patterns then []
            else
              [
                Printf.sprintf "Warning: non-exhaustive pattern match in %s"
                  decl.name.thing;
              ]
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
      | Expr_record_select _ | Expr_record_empty | Expr_unit | Expr_char _
      | Expr_string _ | Expr_int _ | Expr_float _ ->
          []
    in
    check_expr decl.body
  in

  let warnings =
    List.concat_map
      (fun x ->
        match x with
        | Ok Infer.Infer_proc.{ siblings_env; declarations; _ } ->
            List.concat_map (check_exhaustiveness siblings_env) declarations
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
           ( List.map
               (fun (c : Infer.Infer_proc.ctor_info) -> (c.name, c.arity))
               r.constructors,
             Infer.Infer_proc.Name_map.bindings r.siblings_env )))
      typed
  in

  let js_sources =
    List.filter_map
      (fun (sorted_r, ctors_r) ->
        match (sorted_r, ctors_r) with
        | Ok declarations, Ok (constructors, siblings) ->
            Some
              (Codegen.Js_of_optimized.emit ~constructors ~siblings declarations)
        | Ok declarations, Error _ ->
            Some (Codegen.Js_of_optimized.emit declarations)
        | Error _, _ -> None)
      (List.combine sorted constructors_per_file)
  in

  List.iter
    (fun js_code ->
      Printf.printf "\n=== Generated JavaScript ===\n%s\n" js_code)
    js_sources;

  prerr_endline "Success!"
