type t = Top_declaration of Declaration.t

let of_frontend = function
  | Frontend.Impl.Top_declaration d ->
      Top_declaration
        {
          type_part_data = None;
          body_part =
            {
              name = d.body_part.name;
              expr =
                Data.Located.mk
                  (Expr.of_frontend d.body_part.expr.thing)
                  d.body_part.expr.loc;
              params = d.body_part.params;
            };
        }
  | _ -> failwith ""
