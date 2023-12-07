type t = { type_part_data : type_part option; body_part : body_part }
[@@deriving show]

and type_part = { name : string; type_alias : Typedef.t } [@@deriving show]
(** fixme: rename it *)

and body_part = { name : string; expr : Expr.t } [@@deriving show]
