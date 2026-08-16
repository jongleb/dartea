type t =
  | Expr_char of string (* elm type: Chr ES.String, NEED IMPLEMENT *)
  | Expr_string of string (* Chr ES.String *)
  | Expr_int of int
  | Expr_float of float (* EF.Float *)
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
(*Let [A.Located Def] Expr *)

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

module Str_set = Set.Make (String)

type canonicalize_env = { constrs : Str_set.t }

let unary_function = function "-" -> "negate" | operator -> operator

let of_frontend expr =
  let rec go = function
    | Frontend.Expr.Expr_float i -> Expr_float i
    | Expr_string s -> Expr_string s
    | Expr_int i -> Expr_int i
    | Expr_char c -> Expr_char c
    | Expr_unit -> Expr_unit
    | Expr_accessor field -> Expr_accessor field
    | Expr_access { expr; field } -> Expr_access { expr = go expr; field }
    | Expr_list list -> Expr_list (List.map go list)
    | Expr_cons { head; tail } -> Expr_cons { head = go head; tail = go tail }
    | Expr_tuple items -> Expr_tuple (List.map go items)
    | Expr_record_update { record; fields } ->
        Expr_record_update
          {
            record = go record;
            fields =
              List.map
                (fun (row : Frontend.Expr.expr_record_row) ->
                  { name = row.name; value = go row.value })
                fields;
          }
    | Frontend.Expr.Expr_binop { name; operands = a, b } ->
        Expr_apply
          {
            fn =
              Expr_apply
                { fn = Expr_ident (Data.Name.local name); arg = go a };
            arg = go b;
          }
    | Expr_let
        {
          binding = { bind_body = { name; body = bind_body }; bind_type };
          body;
        } ->
        Expr_let
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
          }
    | Expr_apply { fn; arg } -> Expr_apply { fn = go fn; arg = go arg }
    | Expr_ident i -> Expr_ident (Data.Name.local i)
    | Expr_if_then_else { if_exp; then_exp; else_exp } ->
        Expr_if_then_else
          { if_exp = go if_exp; then_exp = go then_exp; else_exp = go else_exp }
    | Expr_pattern { expr; pattern_data_items } ->
        Expr_pattern
          {
            expr = go expr;
            pattern_data_items =
              List.map
                (fun Frontend.Expr.{ pattern; expr } ->
                  { pattern = Pattern.of_frontend pattern; expr = go expr })
                pattern_data_items;
          }
    | Expr_record r ->
        List.fold_left
          (fun acc next ->
            Expr_apply
              {
                fn =
                  Expr_apply
                    {
                      fn = Expr_record_extend next.Frontend.Expr.name;
                      arg = go next.value;
                    };
                arg = acc;
              })
          Expr_record_empty r
    | Expr_lambda { params; body } -> Expr_lambda { params; body = go body }
    | Expr_constr_fixed name -> Expr_ident (Data.Name.local name)
    | Expr_qualified { qualifier; name } ->
        Expr_ident
          (Data.Name.global ~module_name:qualifier ~exported_name:name)
    | Expr_unop { name; operand } ->
        Expr_apply
          {
            fn = Expr_ident (Data.Name.local (unary_function (Data.Located.unwrap name)));
            arg = go operand;
          }
  in
  go expr
