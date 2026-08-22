let named (declaration : Optimized.Declaration.t) =
  Data.Name.local (Data.Located.unwrap declaration.name)

let alive ~roots declarations =
  let uses =
    List.map
      (fun declaration ->
        (named declaration, Scope.free_in_declaration declaration))
      declarations
  in
  let rec grown needed =
    let reached =
      List.fold_left
        (fun found (name, free) ->
          if Scope.Names.mem name found then Scope.Names.union found free
          else found)
        needed uses
    in
    if Scope.Names.equal reached needed then needed else grown reached
  in
  let needed = grown (Scope.Names.of_list roots) in
  List.filter
    (fun declaration -> Scope.Names.mem (named declaration) needed)
    declarations
