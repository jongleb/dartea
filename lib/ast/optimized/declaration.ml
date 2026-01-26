type param = { name : string Data.Located.t; typ : Type.t } [@@deriving show]

type t = {
  name : string Data.Located.t;
  params : param list;
  body : Expr.t;
  typ : Type.t;
}
[@@deriving show]
