module O = Optimized
module Names = Set.Make (String)

let union_map f items =
  List.fold_left (fun acc item -> Names.union acc (f item)) Names.empty items

let transform_children ~(f : O.Expr.t -> O.Expr.t) (e : O.Expr.t) : O.Expr.t =
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
    | ( Expr_ident _ | Expr_accessor _ | Expr_record_extend _
      | Expr_record_select _ | Expr_record_empty | Expr_char _ | Expr_string _
      | Expr_int _ | Expr_float _ ) as leaf ->
        leaf
  in
  { e with expr }

let children (e : O.Expr.t) : O.Expr.t list =
  let visited = ref [] in
  let collect child =
    visited := child :: !visited;
    child
  in
  let (_: O.Expr.t) = transform_children e ~f:collect in
  List.rev !visited

let rec bound_by_pattern (p : O.Pattern.t) : Names.t =
  match p.pattern with
  | P_T_var name -> Names.singleton name
  | P_T_record fields -> Names.of_list fields
  | P_T_tuple items | P_T_list items -> union_map bound_by_pattern items
  | P_T_cons (head, tail) ->
      Names.union (bound_by_pattern head) (bound_by_pattern tail)
  | P_T_ctor (_, arguments) -> union_map bound_by_pattern arguments
  | P_T_anything | P_T_unit | P_T_chr _ | P_T_str _ | P_T_int _ -> Names.empty

let bound_by_lambda (params : O.Expr.expr_lambda_param list) : Names.t =
  Names.of_list
    (List.map
       (fun (p : O.Expr.expr_lambda_param) -> Data.Located.unwrap p.name)
       params)

let bound_by_declaration (d : O.Declaration.t) : Names.t =
  Names.of_list
    (List.map
       (fun (p : O.Declaration.param) -> Data.Located.unwrap p.name)
       d.params)

let rec free_variables ~(bound : Names.t) (e : O.Expr.t) : Names.t =
  match e.expr with
  | Expr_ident name ->
      if Names.mem name bound then Names.empty else Names.singleton name
  | Expr_let { binding; body } ->
      let bound_in_body =
        Names.add (Data.Located.unwrap binding.bind_body.name) bound
      in
      Names.union
        (free_variables ~bound binding.bind_body.body)
        (free_variables ~bound:bound_in_body body)
  | Expr_lambda { params; body } ->
      free_variables ~bound:(Names.union bound (bound_by_lambda params)) body
  | Expr_pattern { expr; pattern_data_items } ->
      let free_in_case (case : O.Expr.expr_pattern_case) =
        let bound = Names.union bound (bound_by_pattern case.pattern) in
        free_variables ~bound case.expr
      in
      Names.union
        (free_variables ~bound expr)
        (union_map free_in_case pattern_data_items)
  | Expr_binop { name; operands = left, right } ->
      let operands =
        Names.union (free_variables ~bound left) (free_variables ~bound right)
      in
      if Names.mem name bound then operands else Names.add name operands
  | _ -> union_map (free_variables ~bound) (children e)

let free_in_declaration (d : O.Declaration.t) : Names.t =
  free_variables ~bound:(bound_by_declaration d) d.body
