module String_map = Map.Make (String)

type t = {
  imports : Import_thing.t list;
  type_aliases : Typealias.t String_map.t;
  type_declarations : Typedecl.t String_map.t;
  top_declarations : Declaration.t String_map.t;
  exports : Exposing.t;
  name : string Data.Located.t;
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
                String_map.add td.name td acc.type_declarations;
            }
        | Impl.Top_declaration td ->
            {
              acc with
              top_declarations =
                String_map.add td.body_part.name.thing td acc.top_declarations;
            }
        | Impl.Export e -> { acc with exports = e }
        | Impl.ModuleName name -> { acc with name })
      {
        imports = [];
        type_aliases = String_map.empty;
        type_declarations = String_map.empty;
        top_declarations = String_map.empty;
        exports = Exposing.Open;
        name = Data.Located.(~?"");
      }
      impl_list
  in
  { collected with imports = List.rev collected.imports }
