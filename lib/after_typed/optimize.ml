module T = Typed

let constant_fold_binop (name : string) (e1 : T.Expr.t) (e2 : T.Expr.t) : T.Expr.expr option =
  match (name, e1.expr, e2.expr) with
  | "+", T.Expr.Expr_int a, T.Expr.Expr_int b -> Some (T.Expr.Expr_int (a + b))
  | "-", T.Expr.Expr_int a, T.Expr.Expr_int b -> Some (T.Expr.Expr_int (a - b))
  | "*", T.Expr.Expr_int a, T.Expr.Expr_int b -> Some (T.Expr.Expr_int (a * b))
  | "/", T.Expr.Expr_int a, T.Expr.Expr_int b when b <> 0 ->
      Some (T.Expr.Expr_int (a / b))
  | "concat", T.Expr.Expr_string a, T.Expr.Expr_string b ->
      Some (T.Expr.Expr_string (a ^ b))
  | _ -> None

let rec optimize_expr (e : T.Expr.t) : T.Expr.t =
  let typ = e.typ in
  let expr =
    match e.expr with
    | T.Expr.Expr_binop { name; operands = e1, e2 } ->
        let e1' = optimize_expr e1 in
        let e2' = optimize_expr e2 in
        (match constant_fold_binop name e1' e2' with
         | Some folded -> folded
         | None -> T.Expr.Expr_binop { name; operands = (e1', e2') })

    | T.Expr.Expr_if_then_else { if_exp; then_exp; else_exp } ->
        let if_exp' = optimize_expr if_exp in
        let then_exp' = optimize_expr then_exp in
        let else_exp' = optimize_expr else_exp in
        (match if_exp'.expr with
         | T.Expr.Expr_constr { name = "True"; _ } ->
             then_exp'.expr
         | T.Expr.Expr_constr { name = "False"; _ } ->
             else_exp'.expr
         | _ ->
             T.Expr.Expr_if_then_else
               { if_exp = if_exp'; then_exp = then_exp'; else_exp = else_exp' })

    | T.Expr.Expr_let { binding; body } ->
        let bind_body =
          {
            T.Expr.name = binding.bind_body.name;
            body = optimize_expr binding.bind_body.body;
          }
        in
        let binding = { T.Expr.bind_body } in
        T.Expr.Expr_let { binding; body = optimize_expr body }

    | T.Expr.Expr_constr { name; arguments } ->
        T.Expr.Expr_constr { name; arguments = List.map optimize_expr arguments }

    | T.Expr.Expr_record rows ->
        T.Expr.Expr_record
          (List.map
             (fun { T.Expr.name; value } ->
               { T.Expr.name; value = optimize_expr value })
             rows)

    | T.Expr.Expr_apply { fn; arg } ->
        let fn' = optimize_expr fn in
        let arg' = optimize_expr arg in
        T.Expr.Expr_apply { fn = fn'; arg = arg' }

    | T.Expr.Expr_lambda { params; body } ->
        T.Expr.Expr_lambda { params; body = optimize_expr body }

    | T.Expr.Expr_pattern { expr; pattern_data_items } ->
        T.Expr.Expr_pattern
          {
            expr = optimize_expr expr;
            pattern_data_items =
              List.map
                (fun { T.Expr.pattern; expr } ->
                  { T.Expr.pattern; expr = optimize_expr expr })
                pattern_data_items;
          }

    | T.Expr.Expr_access { expr; field } ->
        T.Expr.Expr_access { expr = optimize_expr expr; field }

    | T.Expr.Expr_list es ->
        T.Expr.Expr_list (List.map optimize_expr es)

    | T.Expr.Expr_int _ | T.Expr.Expr_float _ | T.Expr.Expr_char _
    | T.Expr.Expr_string _ | T.Expr.Expr_ident _ | T.Expr.Expr_accessor _
    | T.Expr.Expr_record_extend _ | T.Expr.Expr_record_select _
    | T.Expr.Expr_record_empty as expr ->
        expr
  in
  { T.Expr.typ; expr }

let optimize_declaration (d : T.Declaration.t) : T.Declaration.t =
  { d with body = optimize_expr d.body }

let optimize (decls : T.Declaration.t list) : Optimized.Declaration.t list =
  let optimized_typed = List.map optimize_declaration decls in
  Typed_to_optimized.convert optimized_typed
