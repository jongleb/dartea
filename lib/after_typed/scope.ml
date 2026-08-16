module O = Optimized
module Names = Data.Name.Set

let union_map f items =
  List.fold_left (fun acc item -> Names.union acc (f item)) Names.empty items

let rec bound_by_pattern (p : O.Pattern.t) : Names.t =
  match p.pattern with
  | P_T_var name -> Names.singleton (Data.Name.local name)
  | P_T_record fields -> Names.of_list (List.map Data.Name.local fields)
  | P_T_tuple items | P_T_list items -> union_map bound_by_pattern items
  | P_T_alias (inner, name) ->
      Names.add (Data.Name.local name) (bound_by_pattern inner)
  | P_T_cons (head, tail) ->
      Names.union (bound_by_pattern head) (bound_by_pattern tail)
  | P_T_ctor (_, arguments) -> union_map bound_by_pattern arguments
  | P_T_anything | P_T_unit | P_T_chr _ | P_T_str _ | P_T_int _ -> Names.empty

let bound_by_lambda (params : O.Expr.expr_lambda_param list) : Names.t =
  Names.of_list
    (List.map
       (fun (p : O.Expr.expr_lambda_param) ->
         Data.Name.local (Data.Located.unwrap p.name))
       params)

let bound_by_declaration (d : O.Declaration.t) : Names.t =
  Names.of_list
    (List.map
       (fun (p : O.Declaration.param) ->
         Data.Name.local (Data.Located.unwrap p.name))
       d.params)

let rec free_variables ~(bound : Names.t) (e : O.Expr.t) : Names.t =
  match e.expr with
  | Expr_ident name ->
      if Names.mem name bound then Names.empty else Names.singleton name
  | Expr_let { binding; body } ->
      let bound_in_body =
        Names.add
          (Data.Name.local (Data.Located.unwrap binding.bind_body.name))
          bound
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
      let operator = Data.Name.local (Data.Operator.lexeme name) in
      if Names.mem operator bound then operands
      else Names.add operator operands
  | _ -> union_map (free_variables ~bound) (Subexpressions.list e)

let free_in_declaration (d : O.Declaration.t) : Names.t =
  free_variables ~bound:(bound_by_declaration d) d.body

let rec referenced_in_pattern (p : O.Pattern.t) : Names.t =
  match p.pattern with
  | P_T_ctor (name, arguments) ->
      Names.add name (union_map referenced_in_pattern arguments)
  | P_T_tuple items | P_T_list items -> union_map referenced_in_pattern items
  | P_T_alias (inner, _) -> referenced_in_pattern inner
  | P_T_cons (head, tail) ->
      Names.union (referenced_in_pattern head) (referenced_in_pattern tail)
  | P_T_var _ | P_T_record _ | P_T_anything | P_T_unit | P_T_chr _ | P_T_str _
  | P_T_int _ ->
      Names.empty

let rec referenced_in_expression (e : O.Expr.t) : Names.t =
  let children = union_map referenced_in_expression (Subexpressions.list e) in
  match e.expr with
  | Expr_ident name -> Names.add name children
  | Expr_constr { name; _ } -> Names.add name children
  | Expr_binop { name; _ } ->
      Names.add (Data.Name.local (Data.Operator.lexeme name)) children
  | Expr_pattern { pattern_data_items; _ } ->
      List.fold_left
        (fun acc (case : O.Expr.expr_pattern_case) ->
          Names.union acc (referenced_in_pattern case.pattern))
        children pattern_data_items
  | _ -> children

let referenced_in_declarations (decls : O.Declaration.t list) : Names.t =
  union_map (fun (d : O.Declaration.t) -> referenced_in_expression d.body) decls
