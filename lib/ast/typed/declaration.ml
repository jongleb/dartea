type param = { name : string Data.Located.t; typ : Type.t } [@@deriving show]

type t = {
  name : string Data.Located.t;
  params : param list;
  body : Expr.t;
  typ : Type.t;
}
[@@deriving show]

let zonk (decl : t) =
  {
    decl with
    params =
      List.map
        (fun (param : param) -> { param with typ = Type.zonk param.typ })
        decl.params;
    body = Expr.zonk decl.body;
    typ = Type.zonk decl.typ;
  }
