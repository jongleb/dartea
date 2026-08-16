module Exports = Canonical.Exports

type value = { name : Data.Name.t; scheme : Typed.Type.scheme }

type t = {
  module_name : string;
  values : value list;
  types : Canonical.Typedecl.t list;
  type_aliases : Canonical.Typealias.t list;
}

let arity (value : value) =
  let rec arrows (ty : Typed.Type.t) =
    match ty with TFun (_, result) -> 1 + arrows result | _ -> 0
  in
  let (Typed.Type.Scheme (_, ty)) = value.scheme in
  arrows ty

let qualifying ~module_name ~declared_types name =
  match name with
  | Data.Name.Local text when Exports.Names.mem text declared_types ->
      Data.Name.global ~module_name ~exported_name:text
  | Data.Name.Local _ | Data.Name.Global _ -> name

let rec inferred_type ~qualify (ty : Typed.Type.t) : Typed.Type.t =
  let go = inferred_type ~qualify in
  match ty with
  | TCustom (name, arguments) -> TCustom (qualify name, List.map go arguments)
  | TFun (parameter, result) -> TFun (go parameter, go result)
  | TTup items -> TTup (List.map go items)
  | TRecord row -> TRecord (go row)
  | TRowExtend (label, field, rest) -> TRowExtend (label, go field, go rest)
  | (TVar _ | TInt | TFloat | TChar | TBool | TStr | TUnit | TRowEmpty) as leaf ->
      leaf

let rec written_type ~qualify (t : Canonical.Typedef.Impl.t) :
    Canonical.Typedef.Impl.t =
  let open Canonical.Typedef in
  let body =
    match t.body with
    | Kind.Tkind_concrete written ->
        Kind.Tkind_concrete (Data.Located.map qualify written)
    | Kind.Tkind_record { values; row_type } ->
        Kind.Tkind_record
          {
            values =
              List.map
                (fun (row : Type_record_row.t) ->
                  { row with body = written_type ~qualify row.body })
                values;
            row_type;
          }
    | Kind.Tkind_tuple items ->
        Kind.Tkind_tuple (List.map (written_type ~qualify) items)
    | Kind.Tkind_function { arguments } ->
        Kind.Tkind_function
          { arguments = List.map (written_type ~qualify) arguments }
    | (Kind.Tkind_var _ | Kind.Tkind_unit) as leaf -> leaf
  in
  { parameters = List.map (written_type ~qualify) t.parameters; body }

let of_module ~(values : (string * Typed.Type.scheme) list)
    (m : Canonical.Module.t) : t =
  let module_name = m.name in
  let exported name = Data.Name.global ~module_name ~exported_name:name in
  let qualify =
    qualifying ~module_name ~declared_types:(Exports.declared_types m)
  in
  let exports = Exports.of_module m in
  let exported_typedecl name exposure (td : Canonical.Typedecl.t) =
    let ctors =
      match exposure with
      | Exports.Ctors_hidden -> []
      | Exports.Alias | Exports.Ctors_exposed _ ->
          List.map
            (fun (ctor : Canonical.Typedecl.type_ctor) ->
              {
                Canonical.Typedecl.id = exported (Data.Name.base ctor.id);
                data = List.map (written_type ~qualify) ctor.data;
              })
            td.ctors
    in
    { td with name = exported name; ctors }
  in
  let exported_alias name (ta : Canonical.Typealias.t) =
    { ta with name = exported name; typedef = written_type ~qualify ta.typedef }
  in
  let exported_value (name, Typed.Type.Scheme (parameters, ty)) =
    {
      name = exported name;
      scheme = Typed.Type.Scheme (parameters, inferred_type ~qualify ty);
    }
  in
  let types, type_aliases =
    Exports.By_name.fold
      (fun name exposure (types, aliases) ->
        match
          ( Canonical.Module.String_map.find_opt name m.type_declarations,
            Canonical.Module.String_map.find_opt name m.type_aliases )
        with
        | Some td, _ -> (exported_typedecl name exposure td :: types, aliases)
        | None, Some ta -> (types, exported_alias name ta :: aliases)
        | None, None -> (types, aliases))
      exports.types ([], [])
  in
  {
    module_name;
    values =
      List.filter (fun (name, _) -> Exports.Names.mem name exports.terms) values
      |> List.map exported_value;
    types = List.rev types;
    type_aliases = List.rev type_aliases;
  }
