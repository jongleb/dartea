type t = Top_declaration of Declaration.t | Type_alias of Typealias.t

let of_frontend = function
  | Frontend.Impl.Top_declaration d ->
      Top_declaration (Declaration.of_frontend d)
  | Frontend.Impl.Type_alias ta -> Type_alias (Typealias.of_frontend ta)
  | _ -> failwith "Not implemented"
