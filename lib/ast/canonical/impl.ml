type t =
  | Top_declaration of {
      name : string Data.Located.t;
      expr : Expr.t;
      params : string Data.Located.t list;
    }

let of_frontend = function
  | Frontend.Impl.Top_declaration d ->
      Top_declaration
        {
          name = d.body_part.name;
          expr = Expr.of_frontend d.body_part.expr.thing;
          params = d.body_part.params;
        }
  | _ -> failwith ""
