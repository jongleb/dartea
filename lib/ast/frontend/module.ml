module String_map = Map.Make (String)

type t = {
  imports : Import_thing.t list;
  type_aliases : Typealias.t String_map.t;
  type_declarations : Typedecl.t String_map.t;
  top_declarations : Declaration.t list;
  exports : Exposing.t;
  name : string Data.Located.t option;
}

let port_home = "Elm.Kernel.Port"

let wiring (typedef : Typedef.Impl.t) =
  let named name = Expr.Expr_qualified { qualifier = port_home; name } in
  match typedef.body with
  | Typedef.Kind.Tkind_function { arguments = taken :: _; _ } -> (
      match taken.body with
      | Typedef.Kind.Tkind_function _ -> named "incoming"
      | Typedef.Kind.Tkind_concrete _ | Typedef.Kind.Tkind_var _
      | Typedef.Kind.Tkind_record _ | Typedef.Kind.Tkind_tuple _
      | Typedef.Kind.Tkind_unit ->
          named "outgoing")
  | Typedef.Kind.Tkind_function { arguments = []; _ }
  | Typedef.Kind.Tkind_concrete _ | Typedef.Kind.Tkind_var _
  | Typedef.Kind.Tkind_record _ | Typedef.Kind.Tkind_tuple _
  | Typedef.Kind.Tkind_unit ->
      named "outgoing"

let wired (port : Port_thing.t) =
  let region = port.name.region in
  let at thing = { Data.Located.thing; region } in
  let given = "given" in
  let call fn arg = at (Expr.Expr_apply { fn; arg }) in
  {
    Declaration.type_part_data =
      Some { Declaration.name = port.name; type_alias = port.typedef };
    body_part =
      {
        Declaration.name = port.name;
        params = [ at given ];
        expr =
          call
            (call
               (at (wiring port.typedef))
               (at (Expr.Expr_string (Data.Located.unwrap port.name))))
            (at (Expr.Expr_ident given));
      };
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
        | Impl.ModuleName name -> { acc with name = Some name }
        | Impl.Port port ->
            {
              acc with
              top_declarations = wired port :: acc.top_declarations;
            })
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
