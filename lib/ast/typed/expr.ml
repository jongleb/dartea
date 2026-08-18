type t = { typ : Type.t; expr : expr } [@@deriving show]

and expr =
  | Expr_constr of expr_constr
  | Expr_binop of expr_binop
  | Expr_let of expr_let
  | Expr_if_then_else of expr_if_then_else
  | Expr_record of expr_record_row list
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
  | Expr_char of string (* elm type: Chr ES.String, NEED IMPLEMENT *)
  | Expr_string of string (* Chr ES.String *)
  | Expr_int of int
  | Expr_float of float (* EF.Float *)
  | Expr_list of t list
  | Expr_cons of expr_cons
  | Expr_tuple of t list
[@@deriving show]

and expr_lambda_param = { name : string Data.Located.t; typ : Type.t }
[@@deriving show]

and expr_lambda = { params : expr_lambda_param list; body : t }
[@@deriving show]

and expr_cons = { head : t; tail : t } [@@deriving show]

and expr_constr = { name : Data.Name.t; arguments : t list } [@@deriving show]
(*ConstructorValue { qualifiedness : PossiblyQualified, name : VarName }*)

and expr_binop = { name : Data.Operator.t; operands : t * t } [@@deriving show]
(*  Binops [(Expr, A.Located Name)] Expr *)

and expr_let_binding_type = { name : string (* content : Typedef.Impl.t *) }
[@@deriving show]

and expr_let_binding_body = { name : string Data.Located.t; body : t }
[@@deriving show]

and expr_let_binding = {
  (* bind_type : expr_let_binding_type option; *)
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

let rec zonk (e : t) =
  let inner = zonk in
  let settled =
    match e.expr with
    | Expr_constr constr ->
        Expr_constr { constr with arguments = List.map inner constr.arguments }
    | Expr_binop binop ->
        let left, right = binop.operands in
        Expr_binop { binop with operands = (inner left, inner right) }
    | Expr_let { binding = { bind_body = { name; body } }; body = rest } ->
        Expr_let
          {
            binding = { bind_body = { name; body = inner body } };
            body = inner rest;
          }
    | Expr_if_then_else { if_exp; then_exp; else_exp } ->
        Expr_if_then_else
          {
            if_exp = inner if_exp;
            then_exp = inner then_exp;
            else_exp = inner else_exp;
          }
    | Expr_record rows ->
        Expr_record (List.map (fun row -> { row with value = inner row.value }) rows)
    | Expr_apply { fn; arg } -> Expr_apply { fn = inner fn; arg = inner arg }
    | Expr_pattern { expr; pattern_data_items } ->
        Expr_pattern
          {
            expr = inner expr;
            pattern_data_items =
              List.map
                (fun (case : expr_pattern_case) ->
                  { pattern = Pattern.zonk case.pattern; expr = inner case.expr })
                pattern_data_items;
          }
    | Expr_access { expr; field } -> Expr_access { expr = inner expr; field }
    | Expr_lambda { params; body } ->
        Expr_lambda
          {
            params =
              List.map
                (fun (param : expr_lambda_param) -> { param with typ = Type.zonk param.typ })
                params;
            body = inner body;
          }
    | Expr_list items -> Expr_list (List.map inner items)
    | Expr_cons { head; tail } -> Expr_cons { head = inner head; tail = inner tail }
    | Expr_tuple items -> Expr_tuple (List.map inner items)
    | Expr_record_update { record; fields } ->
        Expr_record_update
          {
            record = inner record;
            fields =
              List.map
                (fun (row : expr_record_row) -> { row with value = inner row.value })
                fields;
          }
    | ( Expr_ident _ | Expr_accessor _ | Expr_record_extend _
      | Expr_record_select _ | Expr_record_empty | Expr_unit | Expr_kernel _
      | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _ ) as leaf ->
        leaf
  in
  { typ = Type.zonk e.typ; expr = settled }

