module Names = Scope.Names

let name_of (d : Canonical.Declaration.t) =
  Data.Name.local (Data.Located.unwrap d.body_part.name)

let in_dependency_order ~(declaration : 'a -> Canonical.Declaration.t)
    (members : 'a list) : 'a list list =
  let name member = name_of (declaration member) in
  let declared = Names.of_list (List.map name members) in
  let depends_on member =
    Names.inter (Scope.free_in_declaration (declaration member)) declared
  in
  Data.Components.strongly_connected ~name ~depends_on members
  |> List.map Data.Components.members
