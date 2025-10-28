type t = Top_declaration of Declaration.t

let of_frontend = function
  | Frontend.Impl.Top_declaration d ->
      Top_declaration (Declaration.of_frontend d)
  | _ -> failwith ""
