module String_map = Map.Make (String)

type t = {
  name : string;
  imports : Import.t list;
  exports : Exposed.t;
  type_aliases : Typealias.t String_map.t;
  type_declarations : Typedecl.t String_map.t;
  top_declarations : Declaration.t String_map.t;
}

let record_constructor (alias : Frontend.Typealias.t) :
    Frontend.Declaration.t option =
  let open Frontend in
  match alias.typedef with
  | { Typedef.Impl.parameters = []; body = Typedef.Kind.Tkind_record record }
    when Option.is_none record.Typedef.Type_record.row_type ->
      let at value = { alias.name with Data.Located.thing = value } in
      let rows = record.Typedef.Type_record.values in
      let params =
        List.mapi (fun index _ -> at ("$a" ^ string_of_int index)) rows
      in
      let value =
        Expr.Expr_record
          (List.map2
             (fun (row : Typedef.Type_record_row.t) param ->
               {
                 Expr.name = Data.Located.unwrap row.name;
                 value = Expr.Expr_ident (Data.Located.unwrap param);
               })
             rows params)
      in
      let aliased =
        {
          Typedef.Impl.parameters =
            List.map
              (fun param ->
                {
                  Typedef.Impl.parameters = [];
                  body = Typedef.Kind.Tkind_var param;
                })
              alias.params;
          body = Typedef.Kind.Tkind_concrete alias.name;
        }
      in
      let signature =
        match rows with
        | [] -> aliased
        | rows ->
            {
              Typedef.Impl.parameters = [];
              body =
                Typedef.Kind.Tkind_function
                  {
                    Typedef.Type_function.arguments =
                      List.map
                        (fun (row : Typedef.Type_record_row.t) -> row.body)
                        rows
                      @ [ aliased ];
                  };
            }
      in
      Some
        {
          Declaration.type_part_data =
            Some { Declaration.name = alias.name; type_alias = signature };
          body_part =
            { Declaration.name = alias.name; expr = at value; params };
        }
  | _ -> None

let of_frontend ~fallback_name (frontend_module : Frontend.Module.t) : t =
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
    |> Frontend.Module.String_map.fold
         (fun name alias acc ->
           match record_constructor alias with
           | None -> acc
           | Some declaration ->
               String_map.add name (Declaration.of_frontend declaration) acc)
         frontend_module.type_aliases
  in
  {
    name =
      Option.map Data.Located.unwrap frontend_module.name
      |> Option.value ~default:fallback_name;
    imports = List.map Import.of_frontend frontend_module.imports;
    exports = Exposed.of_frontend frontend_module.exports;
    type_aliases;
    type_declarations;
    top_declarations;
  }
