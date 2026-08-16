module O = Optimized

let transform (e : O.Expr.t) ~(f : O.Expr.t -> O.Expr.t) : O.Expr.t =
  let expr =
    match e.expr with
    | Expr_constr { name; arguments } ->
        O.Expr.Expr_constr { name; arguments = List.map f arguments }
    | Expr_binop { name; operands = left, right } ->
        let left = f left in
        O.Expr.Expr_binop { name; operands = (left, f right) }
    | Expr_let { binding; body } ->
        let bound_value = f binding.bind_body.body in
        O.Expr.Expr_let
          {
            binding =
              { bind_body = { binding.bind_body with body = bound_value } };
            body = f body;
          }
    | Expr_if_then_else { if_exp; then_exp; else_exp } ->
        let if_exp = f if_exp in
        let then_exp = f then_exp in
        O.Expr.Expr_if_then_else { if_exp; then_exp; else_exp = f else_exp }
    | Expr_record rows ->
        O.Expr.Expr_record
          (List.map
             (fun (row : O.Expr.expr_record_row) ->
               { row with value = f row.value })
             rows)
    | Expr_apply { fn; arg } ->
        let fn = f fn in
        O.Expr.Expr_apply { fn; arg = f arg }
    | Expr_pattern { expr; pattern_data_items } ->
        let scrutinee = f expr in
        O.Expr.Expr_pattern
          {
            expr = scrutinee;
            pattern_data_items =
              List.map
                (fun (case : O.Expr.expr_pattern_case) ->
                  { case with expr = f case.expr })
                pattern_data_items;
          }
    | Expr_access { expr; field } -> O.Expr.Expr_access { expr = f expr; field }
    | Expr_lambda { params; body } ->
        O.Expr.Expr_lambda { params; body = f body }
    | Expr_list items -> O.Expr.Expr_list (List.map f items)
    | Expr_cons { head; tail } ->
        O.Expr.Expr_cons { head = f head; tail = f tail }
    | Expr_tuple items -> O.Expr.Expr_tuple (List.map f items)
    | Expr_record_update { record; fields } ->
        O.Expr.Expr_record_update
          {
            record = f record;
            fields =
              List.map
                (fun (row : O.Expr.expr_record_row) ->
                  { row with value = f row.value })
                fields;
          }
    | Expr_kernel (Kernel_unary { kernel; argument }) ->
        O.Expr.Expr_kernel (Kernel_unary { kernel; argument = f argument })
    | Expr_kernel (Kernel_binary { kernel; left; right }) ->
        O.Expr.Expr_kernel
          (Kernel_binary { kernel; left = f left; right = f right })
    | ( Expr_ident _ | Expr_accessor _ | Expr_record_extend _
      | Expr_record_select _ | Expr_record_empty | Expr_unit
      | Expr_kernel (Kernel_value _)
      | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _ ) as leaf ->
        leaf
  in
  { e with expr }

let list (e : O.Expr.t) : O.Expr.t list =
  let visited = ref [] in
  let collect child =
    visited := child :: !visited;
    child
  in
  let (_ : O.Expr.t) = transform e ~f:collect in
  List.rev !visited

