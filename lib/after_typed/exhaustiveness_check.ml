let warnings
    (siblings_env : (Data.Name.t * int) list Data.Name.Map.t)
    (decl : Typed.Declaration.t) =
  let open Typed.Expr in
  let rec in_expression (expr : Typed.Expr.t) =
    match expr.expr with
    | Expr_pattern pattern_match ->
        let branches = pattern_match.pattern_data_items in
        let patterns = List.map (fun (case : expr_pattern_case) -> case.pattern) branches in
        let missing =
          match Exhaustive.counterexample siblings_env patterns with
          | None -> []
          | Some unhandled ->
              [ Reporting.Warning.missing_patterns ~region:expr.region [ unhandled ] ]
        in
        let redundant =
          List.map
            (fun index ->
              let branch = List.nth branches index in
              Reporting.Warning.redundant_pattern ~region:branch.pattern_region
                (index + 1))
            (Exhaustive.redundant_clauses siblings_env patterns)
        in
        missing @ redundant
        @ in_expression pattern_match.expr
        @ List.concat_map
            (fun (case : expr_pattern_case) -> in_expression case.expr)
            branches
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
    | Expr_cons { head; tail } -> in_expression head @ in_expression tail
    | Expr_tuple items -> List.concat_map in_expression items
    | Expr_record_update { record; fields } ->
        in_expression record
        @ List.concat_map
            (fun (row : expr_record_row) -> in_expression row.value)
            fields
    | Expr_lambda lambda -> in_expression lambda.body
    | Expr_access access -> in_expression access.expr
    | Expr_record rows ->
        List.concat_map
          (fun (row : expr_record_row) -> in_expression row.value)
          rows
    | Expr_ident _ | Expr_accessor _ | Expr_record_extend _
    | Expr_record_select _ | Expr_record_empty | Expr_unit | Expr_kernel _
    | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _ ->
        []
  in
  in_expression decl.body
