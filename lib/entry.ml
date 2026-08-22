let declaration = "main"

type t = {
  module_name : string;
  declaration : string;
  typ : Typed.Type.t;
  region : Data.Region.t;
}

let of_declarations ~module_name (declarations : Typed.Declaration.t list) =
  List.find_map
    (fun (found : Typed.Declaration.t) ->
      if String.equal (Data.Located.unwrap found.name) declaration then
        Some
          {
            module_name;
            declaration;
            typ = Typed.Type.zonk found.typ;
            region = found.name.region;
          }
      else None)
    declarations
