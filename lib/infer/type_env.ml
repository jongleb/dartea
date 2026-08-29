open Typed
open Typed.Type
module Name_map = Data.Name.Map

type t = {
  types : Canonical.Typedecl.t Name_map.t;
  constructors :
    (Canonical.Typedecl.t * Canonical.Typedecl.type_ctor) Name_map.t;
  aliases : (Typed.Type.t Variable.t list * Typed.Type.t) Name_map.t;
}

let concrete_type = Primitives.concrete_type

module Written = Map.Make (String)

let source_variables () =
  let known = ref Written.empty in
  fun written ->
    match Written.find_opt written !known with
    | Some variable -> variable
    | None ->
        let variable = Variable.fresh (Data.Constraint.of_written written) in
        known := Written.add written variable !known;
        variable

let typedef_to_type ~variables (impl : Canonical.Typedef.Impl.t) =
  let open Canonical.Typedef in
  let rec conv (i : Impl.t) : Type.t =
    let args = List.map conv i.parameters in
    match i.body with
    | Kind.Tkind_var v -> TVar (variables v.thing)
    | Kind.Tkind_concrete c -> concrete_type c.thing args
    | Kind.Tkind_tuple types -> TTup (List.map conv types)
    | Kind.Tkind_function fn ->
        function_of (List.map conv fn.arguments) ~result:(conv fn.result)
    | Kind.Tkind_unit -> TUnit
    | Kind.Tkind_record fields ->
        let base =
          match fields.row_type with
          | Some row_var -> TVar (variables row_var.thing)
          | None -> TRowEmpty
        in
        let row_type =
          List.fold_right
            (fun (row : Type_record_row.t) acc ->
              TRowExtend (row.name.thing, conv row.body, acc))
            fields.values base
        in
        TRecord row_type
  in
  conv impl

let written_type impl = typedef_to_type ~variables:(source_variables ()) impl

let typedecl_payloads (typedef : Canonical.Typedecl.t) =
  let variables = source_variables () in
  ( List.map variables typedef.params,
    List.map
      (fun (ctor : Canonical.Typedecl.type_ctor) ->
        (ctor.id, List.map (typedef_to_type ~variables) ctor.data))
      typedef.ctors )

let build ~(imports : Interface.t list) (module_ : Canonical.Module.t) : t =
  let declared_here =
    Canonical.Module.String_map.fold
      (fun _ typedecl collected -> typedecl :: collected)
      module_.type_declarations []
  in
  let imported = List.concat_map (fun (i : Interface.t) -> i.types) imports in
  let visible = Primitives.types @ imported @ declared_here in
  let add_type collected (typedecl : Canonical.Typedecl.t) =
    Name_map.add typedecl.name typedecl collected
  in
  let add_constructors collected (typedecl : Canonical.Typedecl.t) =
    List.fold_left
      (fun collected (ctor : Canonical.Typedecl.type_ctor) ->
        Name_map.add ctor.id (typedecl, ctor) collected)
      collected typedecl.ctors
  in
  let add_alias name (alias : Canonical.Typealias.t) collected =
    let variables = source_variables () in
    Name_map.add name
      (List.map variables alias.params, typedef_to_type ~variables alias.typedef)
      collected
  in
  let imported_aliases =
    List.concat_map (fun (i : Interface.t) -> i.type_aliases) imports
    |> List.fold_left
         (fun collected (alias : Canonical.Typealias.t) ->
           add_alias alias.name alias collected)
         Name_map.empty
  in
  let aliases =
    Canonical.Module.String_map.fold
      (fun name alias collected -> add_alias (Data.Name.local name) alias collected)
      module_.type_aliases imported_aliases
  in
  {
    types = List.fold_left add_type Name_map.empty visible;
    constructors = List.fold_left add_constructors Name_map.empty visible;
    aliases;
  }

let rec expand ~region type_env ty =
  let expand = expand ~region type_env in
  match ty with
  | TCustom (name, args) -> begin
      match Name_map.find_opt name type_env.aliases with
      | None -> TCustom (name, List.map expand args)
      | Some (params, alias_body) when List.length params = List.length args ->
          let bindings =
            List.fold_left2
              (fun collected param arg ->
                By_variable.add param (expand arg) collected)
              By_variable.empty params args
          in
          expand (substitute bindings alias_body)
      | Some (params, _) ->
          Reporting.Error.raise_type ~region
            (Reporting.Type_error.Bad_arity
               {
                 thing = A_type;
                 name;
                 expects = List.length params;
                 given = List.length args;
               })
    end
  | TVar _ | TInt | TFloat | TChar | TBool | TStr | TUnit | TRowEmpty | TFun _
  | TTup _ | TRecord _ | TRowExtend _ ->
      map_children expand ty

let expand_written ~region type_env impl =
  expand ~region type_env (written_type impl)

let constructor_scheme type_env (typedef : Canonical.Typedecl.t)
    (ctor : Canonical.Typedecl.type_ctor) =
  let variables = source_variables () in
  let params = List.map variables typedef.params in
  let result_type =
    concrete_type typedef.name (List.map (fun param -> TVar param) params)
  in
  let payload written =
    expand ~region:ctor.region type_env (typedef_to_type ~variables written)
  in
  Scheme (params, function_of (List.map payload ctor.data) ~result:result_type)

let constructor_values (type_env : t) =
  Name_map.fold
    (fun ctor_name (typedef, ctor) collected ->
      Value_env.bind ctor_name
        (constructor_scheme type_env typedef ctor)
        collected)
    type_env.constructors Value_env.primitives

let constructor_of name type_env = Name_map.find_opt name type_env.constructors
let typedecls type_env = List.map snd (Name_map.bindings type_env.types)
