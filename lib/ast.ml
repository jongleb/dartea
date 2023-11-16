open Sexplib.Std

type type_part = { decl_name : string; type_alias : type_alias_data }
[@@deriving show]
(** fixme: rename it *)

and record_data = { key : string; value : type_alias_data } [@@deriving show]

and record_typ = { values : record_data list; row_type : string option }
[@@deriving show]

and function_typ = { arguments : type_alias_data list } [@@deriving show]

and constr_typ = { constr_name : string; params : body_exprs list }
[@@deriving show]

and binop = { op_id : string; params : body_exprs * body_exprs }
[@@deriving show]

and let_expr_item_type_part = { name : string; content : type_alias_data }
[@@deriving show]

and let_expr_item_body_part = { name : string; body : body_exprs }
[@@deriving show]

and let_expr_item = {
  type_part : let_expr_item_type_part option;
  body_part : let_expr_item_body_part;
}
[@@deriving show]

and let_expr = { let_expr_items : let_expr_item list; in_ : body_exprs }
[@@deriving show]

and if_then_else = {
  if_exp : body_exprs;
  then_exp : body_exprs;
  else_exp : body_exprs option;
}
[@@deriving show]

and record_value_item = { name : string; value : body_exprs } [@@deriving show]

and apply_constr = { ident : body_exprs; args : body_exprs list }
[@@deriving show]

and body_exprs =
  | String_constr of string
  | Int_constr of int
  | Float_constr of float
  | List_constr of body_exprs list
  | Constr of constr_typ
  | Binop of binop
  | Let of let_expr
  | If_then_else of if_then_else
  | Record of record_value_item list
  | Apply of apply_constr
  | Ident of string
[@@deriving show]

and body_part = { name : string; expr : body_exprs } [@@deriving show]

and delcraration = { type_part_data : type_part option; body_part : body_part }
[@@deriving show]
(** fixme: rename it *)

and type_alias_kind =
  | Concrete of string
  | Type_var of string
  | Record of record_typ
  | Tuples of type_alias_data list
  | Function of function_typ
[@@deriving show]

and type_alias_data = {
  params : type_alias_data list;
  content : type_alias_kind;
}
[@@deriving show]

type type_alias_desc = {
  data : type_alias_data;
  params : string list;
  id : string;
}
[@@deriving show]

type type_constr = { id : string; data : type_alias_data list }
[@@deriving show]

type type_dec = {
  id : string;
  constrs : type_constr list;
  params : string list;
}
[@@deriving show]

type elm_ast =
  | Type_alias of type_alias_desc
  | Type_dec of type_dec
  | Declaration of delcraration  (** fixme: rename it *)
[@@deriving show]
