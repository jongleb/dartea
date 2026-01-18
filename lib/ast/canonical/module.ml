module String_map = Map.Make (String)

type t = {
  type_aliases : Typealias.t String_map.t;
  type_declarations : Typedecl.t String_map.t;
  top_declarations : Declaration.t String_map.t;
}

let of_frontend (frontend_module : Frontend.Module.t) : t =
  let type_aliases =
    Frontend.Module.String_map.fold
      (fun name ta acc -> String_map.add name (Typealias.of_frontend ta) acc)
      frontend_module.type_aliases String_map.empty
  in
  let type_declarations =
    Frontend.Module.String_map.fold
      (fun name td acc -> String_map.add name (Typedecl.of_frontend td) acc)
      frontend_module.type_declarations String_map.empty
  in
  let top_declarations =
    Frontend.Module.String_map.fold
      (fun name d acc -> String_map.add name (Declaration.of_frontend d) acc)
      frontend_module.top_declarations String_map.empty
  in
  { type_aliases; type_declarations; top_declarations }
