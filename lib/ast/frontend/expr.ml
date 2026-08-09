type t =
  | Expr_char of string (* elm type: Chr ES.String, NEED IMPLEMENT *)
  | Expr_string of string (* Chr ES.String *)
  | Expr_int of int
  | Expr_float of float (* EF.Float *)
  | Expr_unit
  | Expr_list of t list
  | Expr_constr of expr_constr
  | Expr_binop of expr_binop
  | Expr_let of expr_let
  | Expr_if_then_else of expr_if_then_else
  | Expr_record of expr_record_row list
  | Expr_apply of expr_apply
  | Expr_constr_fixed of string
  | Expr_ident of string
  | Expr_qualified of expr_qualified
  | Expr_pattern of expr_pattern
  | Expr_accessor of string Data.Located.t
  | Expr_access of expr_access
  | Expr_unop of expr_unop
  | Expr_lambda of expr_lambda
[@@deriving show]

and expr_lambda = { params : string Data.Located.t list; body : t }
[@@deriving show]

and expr_unop = { name : string Data.Located.t; operand : t } [@@deriving show]

and expr_qualified = { qualifier : string; name : string } [@@deriving show]
and expr_constr = { name : string; arguments : t list } [@@deriving show]
(*ConstructorValue { qualifiedness : PossiblyQualified, name : VarName }*)

and expr_binop = { name : string; operands : t * t } [@@deriving show]
(*  Binops [(Expr, A.Located Name)] Expr *)

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
and expr_apply = { fn : t; arg : t } [@@deriving show]
and expr_pattern_case = { pattern : Pattern.t; expr : t } [@@deriving show]

and expr_pattern = { expr : t; pattern_data_items : expr_pattern_case list }
[@@deriving show]

and expr_access = { expr : t; field : string Data.Located.t } [@@deriving show]

let make_qualified lexeme =
  match String.rindex_opt lexeme '.' with
  | Some i ->
      Expr_qualified
        {
          qualifier = String.sub lexeme 0 i;
          name = String.sub lexeme (i + 1) (String.length lexeme - i - 1);
        }
  | None -> Expr_ident lexeme

let make_expr_let ~bindings body =
  List.fold_right (fun binding body -> Expr_let { body; binding }) bindings body

let make_expr_lambda ~params body =
  match params with [] -> body | _ -> Expr_lambda { params; body }

let make_expr_apply ~args fn =
  Non_empty_list.reduce
    ~f:(fun fn arg -> Expr_apply { fn; arg })
    Non_empty_list.(fn :: args)
