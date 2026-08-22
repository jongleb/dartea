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
  | Expr_kernel of expr_kernel
  | Expr_lambda of expr_lambda
  | Expr_char of string
  | Expr_string of string
  | Expr_int of int
  | Expr_float of float
  | Expr_list of t list
  | Expr_cons of expr_cons
  | Expr_tuple of t list
[@@deriving show]

and expr_kernel =
  | Kernel_value of Data.Kernel.t
  | Kernel_unary of { kernel : Data.Kernel.unary; argument : t }
  | Kernel_binary of { kernel : Data.Kernel.binary; left : t; right : t }
[@@deriving show]

and expr_lambda_param = { name : string Data.Located.t; typ : Type.t }
[@@deriving show]

and expr_lambda = { params : expr_lambda_param list; body : t }
[@@deriving show]

and expr_cons = { head : t; tail : t } [@@deriving show]

and expr_constr = { name : Data.Name.t; arguments : t list } [@@deriving show]

and expr_binop = { name : Data.Operator.t; operands : t * t } [@@deriving show]

and expr_let_binding_type = { name : string } [@@deriving show]

and expr_let_binding_body = { name : string Data.Located.t; body : t }
[@@deriving show]

and expr_let_binding = { bind_body : expr_let_binding_body } [@@deriving show]

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

let ident_of e =
  match e.expr with
  | Expr_ident name -> Some name
  | Expr_constr _ | Expr_binop _ | Expr_let _ | Expr_if_then_else _
  | Expr_record _ | Expr_record_update _ | Expr_apply _ | Expr_pattern _
  | Expr_accessor _ | Expr_access _ | Expr_record_extend _
  | Expr_record_select _ | Expr_record_empty | Expr_unit | Expr_kernel _
  | Expr_lambda _ | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _
  | Expr_list _ | Expr_cons _ | Expr_tuple _ ->
      None

let record_extend_of e =
  match e.expr with
  | Expr_record_extend field -> Some field
  | Expr_constr _ | Expr_binop _ | Expr_let _ | Expr_if_then_else _
  | Expr_record _ | Expr_record_update _ | Expr_apply _ | Expr_ident _
  | Expr_pattern _ | Expr_accessor _ | Expr_access _ | Expr_record_select _
  | Expr_record_empty | Expr_unit | Expr_kernel _ | Expr_lambda _ | Expr_char _
  | Expr_string _ | Expr_int _ | Expr_float _ | Expr_list _ | Expr_cons _
  | Expr_tuple _ ->
      None

let lambda_of e =
  match e.expr with
  | Expr_lambda lambda -> Some lambda
  | Expr_constr _ | Expr_binop _ | Expr_let _ | Expr_if_then_else _
  | Expr_record _ | Expr_record_update _ | Expr_apply _ | Expr_ident _
  | Expr_pattern _ | Expr_accessor _ | Expr_access _ | Expr_record_extend _
  | Expr_record_select _ | Expr_record_empty | Expr_unit | Expr_kernel _
  | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _ | Expr_list _
  | Expr_cons _ | Expr_tuple _ ->
      None

let spine expression =
  let rec collect arguments e =
    match e.expr with
    | Expr_apply { fn; arg } -> collect (arg :: arguments) fn
    | Expr_constr _ | Expr_binop _ | Expr_let _ | Expr_if_then_else _
    | Expr_record _ | Expr_record_update _ | Expr_ident _ | Expr_pattern _
    | Expr_accessor _ | Expr_access _ | Expr_record_extend _
    | Expr_record_select _ | Expr_record_empty | Expr_unit | Expr_kernel _
    | Expr_lambda _ | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _
    | Expr_list _ | Expr_cons _ | Expr_tuple _ ->
        (e, arguments)
  in
  collect [] expression
