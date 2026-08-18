module String_map = Map.Make (String)

type t = {
  imports : Import_thing.t list;
  type_aliases : Typealias.t String_map.t;
  type_declarations : Typedecl.t String_map.t;
  top_declarations : Declaration.t list;
  exports : Exposing.t;
  name : string Data.Located.t option;
}

let of_impl impl_list =
  let collected =
    List.fold_left
      (fun acc next ->
        match next with
        | Impl.Import thing -> { acc with imports = thing :: acc.imports }
        | Impl.Type_alias ta ->
            {
              acc with
              type_aliases = String_map.add ta.name.thing ta acc.type_aliases;
            }
        | Impl.Type_dec td ->
            {
              acc with
              type_declarations =
                String_map.add td.name.thing td acc.type_declarations;
            }
        | Impl.Top_declaration td ->
            {
              acc with
              top_declarations = td :: acc.top_declarations;
            }
        | Impl.Export e -> { acc with exports = e }
        | Impl.ModuleName name -> { acc with name = Some name })
      {
        imports = [];
        type_aliases = String_map.empty;
        type_declarations = String_map.empty;
        top_declarations = [];
        exports = Exposing.Open;
        name = None;
      }
      impl_list
  in
  {
    collected with
    imports = List.rev collected.imports;
    top_declarations = List.rev collected.top_declarations;
  }
