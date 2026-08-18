type t = expr Data.Located.t [@@deriving show]

and expr =
  | Expr_char of string
  | Expr_string of string
  | Expr_int of int
  | Expr_float of float
  | Expr_list of t list
  | Expr_cons of expr_cons
  | Expr_tuple of t list
  | Expr_let of expr_let
  | Expr_if_then_else of expr_if_then_else
  | Expr_record_update of expr_record_update
  | Expr_apply of expr_apply
  | Expr_ident of Data.Name.t
  | Expr_pattern of expr_pattern
  | Expr_accessor of string Data.Located.t
  | Expr_access of expr_access
  | Expr_record_extend of string
  | Expr_record_select of string
  | Expr_record_empty
  | Expr_unit
  | Expr_kernel of Data.Kernel.t
  | Expr_lambda of expr_lambda
[@@deriving show]

and expr_lambda = { params : string Data.Located.t list; body : t }
[@@deriving show]

and expr_cons = { head : t; tail : t } [@@deriving show]

and expr_let_binding_type = { name : string; content : Typedef.Impl.t }
[@@deriving show]

and expr_let_binding_body = { name : string Data.Located.t; body : t }
[@@deriving show]

and expr_let_binding = {
  bind_type : expr_let_binding_type option;
  bind_body : expr_let_binding_body;
}
[@@deriving show]

and expr_let = { binding : expr_let_binding; body : t } [@@deriving show]

and expr_if_then_else = { if_exp : t; then_exp : t; else_exp : t }
[@@deriving show]

and expr_record_row = { name : string; value : t } [@@deriving show]

and expr_record_update = { record : t; fields : expr_record_row list }
[@@deriving show]

and expr_apply = { fn : t; arg : t } [@@deriving show]
and expr_pattern_case = { pattern : Pattern.t; expr : t } [@@deriving show]

and expr_pattern = { expr : t; pattern_data_items : expr_pattern_case list }
[@@deriving show]

and expr_access = { expr : t; field : string Data.Located.t } [@@deriving show]

let unary_function = function "-" -> "negate" | operator -> operator

let of_frontend (expr : Frontend.Expr.t) : t =
  let rec go (written : Frontend.Expr.t) =
    let same expr = Data.Located.at written.region expr in
    match written.thing with
    | Frontend.Expr.Expr_float value -> same (Expr_float value)
    | Expr_string text -> same (Expr_string text)
    | Expr_int value -> same (Expr_int value)
    | Expr_char letter -> same (Expr_char letter)
    | Expr_unit -> same Expr_unit
    | Expr_accessor field -> same (Expr_accessor field)
    | Expr_access { expr; field } -> same (Expr_access { expr = go expr; field })
    | Expr_list items -> same (Expr_list (List.map go items))
    | Expr_cons { head; tail } ->
        same (Expr_cons { head = go head; tail = go tail })
    | Expr_tuple items -> same (Expr_tuple (List.map go items))
    | Expr_record_update { record; fields } ->
        same
          (Expr_record_update
             {
               record = go record;
               fields =
                 List.map
                   (fun (row : Frontend.Expr.expr_record_row) ->
                     { name = row.name; value = go row.value })
                   fields;
             })
    | Expr_binop { name; operands = left, right } ->
        let operator = Data.Located.at written.region (Expr_ident (Data.Name.local name)) in
        same
          (Expr_apply
             {
               fn =
                 Data.Located.at written.region
                   (Expr_apply { fn = operator; arg = go left });
               arg = go right;
             })
    | Expr_let
        {
          binding = { bind_body = { name; body = bind_body }; bind_type };
          body;
        } ->
        same
          (Expr_let
             {
               binding =
                 {
                   bind_type =
                     Option.map
                       (fun (annotation : Frontend.Expr.expr_let_binding_type) ->
                         {
                           name = annotation.name;
                           content = Typedef.Impl.of_frontend annotation.content;
                         })
                       bind_type;
                   bind_body = { name; body = go bind_body };
                 };
               body = go body;
             })
    | Expr_apply { fn; arg } -> same (Expr_apply { fn = go fn; arg = go arg })
    | Expr_ident name -> same (Expr_ident (Data.Name.local name))
    | Expr_if_then_else { if_exp; then_exp; else_exp } ->
        same
          (Expr_if_then_else
             { if_exp = go if_exp; then_exp = go then_exp; else_exp = go else_exp })
    | Expr_pattern { expr; pattern_data_items } ->
        same
          (Expr_pattern
             {
               expr = go expr;
               pattern_data_items =
                 List.map
                   (fun (case : Frontend.Expr.expr_pattern_case) ->
                     {
                       pattern = Pattern.of_frontend case.pattern;
                       expr = go case.expr;
                     })
                   pattern_data_items;
             })
    | Expr_record rows ->
        List.fold_left
          (fun built (row : Frontend.Expr.expr_record_row) ->
            let extend =
              Data.Located.at row.value.region (Expr_record_extend row.name)
            in
            same
              (Expr_apply
                 {
                   fn =
                     Data.Located.at written.region
                       (Expr_apply { fn = extend; arg = go row.value });
                   arg = built;
                 }))
          (same Expr_record_empty) rows
    | Expr_lambda { params; body } ->
        same (Expr_lambda { params; body = go body })
    | Expr_constr_fixed name -> same (Expr_ident (Data.Name.local name))
    | Expr_qualified { qualifier; name } ->
        same
          (Expr_ident (Data.Name.global ~module_name:qualifier ~exported_name:name))
    | Expr_unop { name; operand } ->
        let negate =
          Data.Located.at name.region
            (Expr_ident (Data.Name.local (unary_function name.thing)))
        in
        same (Expr_apply { fn = negate; arg = go operand })
  in
  go expr
