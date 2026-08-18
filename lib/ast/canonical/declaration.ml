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

let of_frontend_type_part (tp : Frontend.Declaration.type_part) : type_part =
  {
    name = tp.Frontend.Declaration.name;
    type_alias = Typedef.Impl.of_frontend tp.type_alias;
  }

let of_frontend_body_part (bp : Frontend.Declaration.body_part) : body_part =
  {
    name = bp.Frontend.Declaration.name;
    expr = Expr.of_frontend bp.expr;
    params = bp.params;
  }

let of_frontend (decl : Frontend.Declaration.t) : t =
  {
    type_part_data =
      Option.map of_frontend_type_part decl.Frontend.Declaration.type_part_data;
    body_part = of_frontend_body_part decl.body_part;
  }
