type t = { type_part_data : type_part option; body_part : body_part }
[@@deriving show]

and type_part = { name : string Data.Located.t; type_alias : Typedef.Impl.t }
[@@deriving show]
(** fixme: rename it *)

and body_part = {
  name : string Data.Located.t;
  expr : Expr.t;
  params : string Data.Located.t list;
}
[@@deriving show]
