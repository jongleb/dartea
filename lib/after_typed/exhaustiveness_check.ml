let warnings
    (siblings_env : (Data.Name.t * int) list Infer.Infer_proc.Name_map.t)
    (decl : Typed.Declaration.t) =
  let open Typed.Expr in
  let rec in_expression (expr : Typed.Expr.t) =
    match expr.expr with
    | Expr_pattern pattern_match ->
        let patterns =
          List.map
            (fun (case : expr_pattern_case) ->
              Typed_to_optimized.pattern_of_typed case.pattern)
            pattern_match.pattern_data_items
        in
        let here =
          match Exhaustive.is_exhaustive siblings_env patterns with
          | true -> []
          | false ->
              [
                Printf.sprintf "Warning: non-exhaustive pattern match in %s"
                  decl.name.thing;
              ]
        in
        here
        @ in_expression pattern_match.expr
        @ List.concat_map
            (fun (case : expr_pattern_case) -> in_expression case.expr)
            pattern_match.pattern_data_items
    | Expr_let let_expr ->
        in_expression let_expr.binding.bind_body.body
        @ in_expression let_expr.body
    | Expr_if_then_else ite ->
        in_expression ite.if_exp @ in_expression ite.then_exp
        @ in_expression ite.else_exp
    | Expr_apply apply -> in_expression apply.fn @ in_expression apply.arg
    | Expr_binop binop ->
        let left, right = binop.operands in
        in_expression left @ in_expression right
    | Expr_constr constr -> List.concat_map in_expression constr.arguments
    | Expr_list exprs -> List.concat_map in_expression exprs
    | Expr_lambda lambda -> in_expression lambda.body
    | Expr_access access -> in_expression access.expr
    | Expr_record rows ->
        List.concat_map
          (fun (row : expr_record_row) -> in_expression row.value)
          rows
    | Expr_ident _ | Expr_accessor _ | Expr_record_extend _
    | Expr_record_select _ | Expr_record_empty | Expr_unit
    | Expr_kernel _ | Expr_char _ | Expr_string _ | Expr_int _
    | Expr_float _ ->
        []
  in
  in_expression decl.body
