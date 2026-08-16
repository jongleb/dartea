module Names = Set.Make (String)
module By_name = Map.Make (String)

type exported_type = Alias | Ctors_hidden | Ctors_exposed of Names.t
type t = { terms : Names.t; types : exported_type By_name.t }

let declared_names map =
  Module.String_map.fold (fun name _ acc -> Names.add name acc) map Names.empty

let ctors_of_type (m : Module.t) type_name =
  Module.String_map.find_opt type_name m.type_declarations
  |> Option.map (fun (td : Typedecl.t) ->
         Names.of_list
           (List.map
              (fun (ctor : Typedecl.type_ctor) -> Data.Name.base ctor.id)
              td.ctors))
  |> Option.value ~default:Names.empty

let declared_terms (m : Module.t) =
  Module.String_map.fold
    (fun type_name _ acc -> Names.union acc (ctors_of_type m type_name))
    m.type_declarations
    (declared_names m.top_declarations)

let declared_types (m : Module.t) =
  Names.union
    (declared_names m.type_declarations)
    (declared_names m.type_aliases)

let type_names (exports : t) =
  By_name.fold (fun name _ acc -> Names.add name acc) exports.types Names.empty

let of_module (m : Module.t) : t =
  let exported_type ~ctors_exposed name =
    let ctors = ctors_of_type m name in
    match (Names.is_empty ctors, ctors_exposed) with
    | true, _ -> Alias
    | false, false -> Ctors_hidden
    | false, true -> Ctors_exposed ctors
  in
  match m.exports with
  | Exposed.All ->
      let expose_type ~ctors_exposed name _ types =
        By_name.add name (exported_type ~ctors_exposed name) types
      in
      {
        terms = declared_terms m;
        types =
          Module.String_map.fold
            (expose_type ~ctors_exposed:true)
            m.type_declarations By_name.empty
          |> Module.String_map.fold
               (expose_type ~ctors_exposed:false)
               m.type_aliases;
      }
  | Only items ->
      let add acc (item : Exposed.item) =
        match item with
        | Value name -> { acc with terms = Names.add name acc.terms }
        | Type { name; ctors_exposed } ->
            let exported = exported_type ~ctors_exposed name in
            let terms =
              match exported with
              | Ctors_exposed ctors -> Names.union acc.terms ctors
              | Alias when Module.String_map.mem name m.top_declarations ->
                  Names.add name acc.terms
              | Alias | Ctors_hidden -> acc.terms
            in
            { terms; types = By_name.add name exported acc.types }
      in
      List.fold_left add { terms = Names.empty; types = By_name.empty } items
