type t = { typ : Type.t; expr : expr } [@@deriving show]

and expr =
  | Expr_constr of expr_constr
  | Expr_binop of expr_binop
  | Expr_let of expr_let
  | Expr_if_then_else of expr_if_then_else
  | Expr_record of expr_record_row list
  | Expr_apply of expr_apply
  | Expr_ident of Data.Name.t
  | Expr_pattern of expr_pattern
  | Expr_accessor of string Data.Located.t
  | Expr_access of expr_access
  | Expr_record_extend of string
  | Expr_record_select of string
  | Expr_record_empty
  | Expr_unit
  | Expr_lambda of expr_lambda
  | Expr_char of string (* elm type: Chr ES.String, NEED IMPLEMENT *)
  | Expr_string of string (* Chr ES.String *)
  | Expr_int of int
  | Expr_float of float (* EF.Float *)
  | Expr_list of t list
[@@deriving show]

and expr_lambda_param = { name : string Data.Located.t; typ : Type.t }
[@@deriving show]

and expr_lambda = { params : expr_lambda_param list; body : t }
[@@deriving show]

and expr_constr = { name : Data.Name.t; arguments : t list } [@@deriving show]
(*ConstructorValue { qualifiedness : PossiblyQualified, name : VarName }*)

and expr_binop = { name : string; operands : t * t } [@@deriving show]
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
and expr_apply = { fn : t; arg : t } [@@deriving show]
and expr_pattern_case = { pattern : Pattern.t; expr : t } [@@deriving show]

and expr_pattern = { expr : t; pattern_data_items : expr_pattern_case list }
[@@deriving show]

and expr_access = { expr : t; field : string Data.Located.t } [@@deriving show]

module Str_set = Set.Make (String)

type canonicalize_env = { constrs : Str_set.t }
