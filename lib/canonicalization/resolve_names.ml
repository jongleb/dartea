module Exports = Canonical.Exports
module Names = Exports.Names
module By_name = Exports.By_name
module Problem = Reporting.Name_error

type error = Reporting.Error.t

module Reported = struct
  module Basic = struct
    type 'a t = { value : 'a; problems : error list }

    let return value = { value; problems = [] }

    let bind reported ~f =
      let next = f reported.value in
      { value = next.value; problems = reported.problems @ next.problems }

    let map = `Define_using_bind
  end

  include Basic
  include Base.Monad.Make (Basic)

  let rejected value ~region problem =
    { value; problems = [ Reporting.Error.name ~region problem ] }

  let both left right =
    bind left ~f:(fun left -> map right ~f:(fun right -> (left, right)))

  let recovered ~fallback ~region = function
    | Ok value -> return value
    | Error problem -> rejected fallback ~region problem

  let collected reported = (reported.value, reported.problems)
end

type namespace = Terms | Types
type dependency = { module_name : string; exports : Exports.t }
type exposing = { nearest : string; earlier : string list }
type scope = { visible : Names.t; sources : exposing By_name.t }

type env = {
  term_scope : scope;
  type_scope : scope;
  qualifiers : dependency By_name.t;
}

let exports_include (exports : Exports.t) namespace name =
  match namespace with
  | Terms -> Names.mem name exports.terms
  | Types -> By_name.mem name exports.types

let everything_exported (exports : Exports.t) =
  [ (Terms, exports.terms); (Types, Exports.type_names exports) ]

let scope_of env = function
  | Terms -> env.term_scope
  | Types -> env.type_scope

let with_scope env namespace scope =
  match namespace with
  | Terms -> { env with term_scope = scope }
  | Types -> { env with type_scope = scope }

let brings_into_scope env brought ~from =
  let expose namespace name env =
    let scope = scope_of env namespace in
    let sources =
      By_name.update name
        (function
          | None -> Some { nearest = from; earlier = [] }
          | Some existing ->
              Some
                {
                  nearest = from;
                  earlier = existing.nearest :: existing.earlier;
                })
        scope.sources
    in
    with_scope env namespace { scope with sources }
  in
  List.fold_left
    (fun env (namespace, names) -> Names.fold (expose namespace) names env)
    env brought

let repeated (declarations : (Data.Region.t * string) list) : error list =
  List.fold_left
    (fun (declared, errors) (region, name) ->
      if Names.mem name declared then
        ( declared,
          Reporting.Error.name ~region (Problem.Duplicate_declaration { name })
          :: errors )
      else (Names.add name declared, errors))
    (Names.empty, []) declarations
  |> snd |> List.rev

let duplicate_declarations (m : Canonical.Module.t) : error list =
  let values =
    List.map
      (fun (d : Canonical.Declaration.t) ->
        (d.body_part.name.region, Data.Located.unwrap d.body_part.name))
      m.top_declarations
  in
  let constructors =
    Canonical.Module.String_map.bindings m.type_declarations
    |> List.concat_map (fun (_, (td : Canonical.Typedecl.t)) ->
           List.map
             (fun (ctor : Canonical.Typedecl.type_ctor) ->
               (ctor.region, Data.Name.base ctor.id))
             td.ctors)
  in
  let type_names =
    (Canonical.Module.String_map.bindings m.type_declarations
    |> List.map (fun (name, (td : Canonical.Typedecl.t)) -> (td.region, name)))
    @ (Canonical.Module.String_map.bindings m.type_aliases
      |> List.map (fun (name, (ta : Canonical.Typealias.t)) -> (ta.region, name)))
  in
  repeated values @ repeated constructors @ repeated type_names

let exposed_names (exports : Exports.t) =
  Names.elements exports.terms @ Names.elements (Exports.type_names exports)

let brought_in_by dependency (item : Canonical.Exposed.item) :
    ((namespace * Names.t) list, Problem.t) result =
  let module_name = dependency.module_name in
  let not_exposed name =
    Problem.Not_exposed
      {
        module_name;
        name;
        near =
          Reporting.Suggest.nearest ~target:name (exposed_names dependency.exports)
          |> List.filteri (fun index _ -> index < 4);
      }
  in
  match item with
  | Value name when Names.mem name dependency.exports.terms ->
      Ok [ (Terms, Names.singleton name) ]
  | Value name -> Error (not_exposed name)
  | Type { name; ctors_exposed } -> begin
      let same_named_term =
        if Names.mem name dependency.exports.terms then
          [ (Terms, Names.singleton name) ]
        else []
      in
      match
        (By_name.find_opt name dependency.exports.types, ctors_exposed)
      with
      | None, _ -> Error (not_exposed name)
      | Some Exports.Ctors_hidden, true ->
          Error (Problem.Ctors_not_exposed { module_name; type_name = name })
      | Some (Exports.Ctors_exposed ctors), true ->
          Ok [ (Types, Names.singleton name); (Terms, ctors) ]
      | Some (Exports.Alias | Ctors_hidden | Ctors_exposed _), false
      | Some Exports.Alias, true ->
          Ok ((Types, Names.singleton name) :: same_named_term)
    end

let environment_of ~(dependencies : Canonical.Module.t list)
    (m : Canonical.Module.t) : env * error list =
  let known =
    List.fold_left
      (fun acc (source : Canonical.Module.t) ->
        By_name.add source.name
          { module_name = source.name; exports = Exports.of_module source }
          acc)
      By_name.empty dependencies
  in
  let from_import env (import : Canonical.Import.t) =
    let module_name = import.module_name in
    let region = import.region in
    match By_name.find_opt module_name known with
    | None ->
        Reported.rejected env ~region
          (Problem.Unknown_module
             {
               qualifier = module_name;
               near =
                 Reporting.Suggest.nearest ~target:module_name
                   (By_name.fold (fun known _ found -> known :: found) known [])
                 |> List.filteri (fun index _ -> index < 4);
             })
    | Some dependency ->
        let qualified =
          {
            env with
            qualifiers =
              By_name.add
                (Option.value import.alias ~default:module_name)
                dependency env.qualifiers;
          }
        in
        let bring env brought =
          brings_into_scope env brought ~from:module_name
        in
        begin
          match import.exposed with
          | Canonical.Exposed.All ->
              Reported.return
                (bring qualified (everything_exported dependency.exports))
          | Only items ->
              let take reported item =
                Reported.bind reported ~f:(fun env ->
                    Reported.recovered ~fallback:env ~region
                      (Result.map (bring env) (brought_in_by dependency item)))
              in
              List.fold_left take (Reported.return qualified) items
        end
  in
  let start =
    {
      term_scope =
        {
          visible =
            List.fold_left
              (fun known name -> Names.add name known)
              (Exports.declared_terms m) Primitives.term_names;
          sources = By_name.empty;
        };
      type_scope =
        {
          visible =
            List.fold_left
              (fun known name -> Names.add name known)
              (Exports.declared_types m) Primitives.type_names;
          sources = By_name.empty;
        };
      qualifiers = By_name.empty;
    }
  in
  let env, errors =
    List.fold_left
      (fun (env, errors) (import : Canonical.Import.t) ->
        let env, problems = Reported.collected (from_import env import) in
        (env, List.rev_append problems errors))
      (start, []) m.imports
  in
  (env, List.rev errors)

let nearest ~target candidates =
  Reporting.Suggest.nearest ~target candidates
  |> List.filteri (fun index _ -> index < 4)

let visible_names env namespace =
  let scope = scope_of env namespace in
  let locals = Names.elements scope.visible in
  let brought = By_name.fold (fun name _ found -> name :: found) scope.sources [] in
  let qualified =
    By_name.fold
      (fun qualifier (dependency : dependency) found ->
        let exported =
          match namespace with
          | Terms -> dependency.exports.terms
          | Types -> Exports.type_names dependency.exports
        in
        Names.fold (fun name found -> (qualifier ^ "." ^ name) :: found) exported found)
      env.qualifiers []
  in
  locals @ brought @ qualified

let starts_with_an_upper_case text =
  String.length text > 0 && Char.equal (Char.uppercase_ascii text.[0]) text.[0]

let missing env namespace ?(prefix = Reporting.Name_error.No_prefix) name =
  let near = nearest ~target:(Data.Name.base name) (visible_names env namespace) in
  match (namespace, Data.Name.base name) with
  | Types, _ -> Problem.Unknown_type { name; prefix; near }
  | Terms, written when starts_with_an_upper_case written ->
      Problem.Unknown_constructor { name; prefix; near }
  | Terms, _ -> Problem.Unbound_value { name; prefix; near }

let refers_to env namespace (name : Data.Name.t) :
    (Data.Name.t, Problem.t) result =
  match name with
  | Local text -> begin
      let scope = scope_of env namespace in
      match
        (Names.mem text scope.visible, By_name.find_opt text scope.sources)
      with
      | true, _ -> Ok name
      | false, None -> Error (missing env namespace name)
      | false, Some { nearest; earlier = [] } ->
          Ok (Data.Name.global ~module_name:nearest ~exported_name:text)
      | false, Some { nearest; earlier } ->
          Error
            (Ambiguous { name = text; modules = List.rev (nearest :: earlier) })
    end
  | Global { module_name = qualifier; exported_name } -> begin
      match By_name.find_opt qualifier env.qualifiers with
      | None -> Error (missing env namespace ~prefix:(Unknown_prefix qualifier) name)
      | Some { module_name; exports }
        when exports_include exports namespace exported_name ->
          Ok (Data.Name.global ~module_name ~exported_name)
      | Some { module_name; exports } ->
          let near =
            Reporting.Suggest.nearest ~target:exported_name
              (Names.elements exports.terms
              @ Names.elements (Exports.type_names exports))
            |> List.filteri (fun index _ -> index < 4)
          in
          Error
            (match namespace with
            | Types ->
                Problem.Unknown_type
                  { name; prefix = Known_prefix module_name; near }
            | Terms when starts_with_an_upper_case exported_name ->
                Problem.Unknown_constructor
                  { name; prefix = Known_prefix module_name; near }
            | Terms ->
                Problem.Unbound_value
                  { name; prefix = Known_prefix module_name; near })
    end

let resolved env namespace (name : Data.Name.t Data.Located.t) =
  Reported.recovered ~fallback:name.thing ~region:name.region
    (refers_to env namespace name.thing)

module Resolution = struct
  type 'a t = 'a Reported.t
  type scope = env

  let return = Reported.return
  let map = Reported.map
  let both = Reported.both
  let binding env (binders : Scope.binders) inner =
    let scope = env.term_scope in
    let visible =
      Scope.Binders.fold
        (fun name _ visible -> Names.add name visible)
        binders.names scope.visible
    in
    Scope.Binders.fold
      (fun name region reported ->
        Reported.bind reported ~f:(fun value ->
            Reported.rejected value ~region (Problem.Duplicate_binder { name })))
      binders.repeated
      (inner { env with term_scope = { scope with visible } })

  let reference env (name : Data.Name.t Data.Located.t) =
    let same expr = Data.Located.at name.region expr in
    match Data.Kernel.referred_to_by name.thing with
    | Known kernel -> Reported.return (same (Canonical.Expr.Expr_kernel kernel))
    | Unknown { module_name; exported_name } ->
        Reported.rejected
          (same (Canonical.Expr.Expr_ident name.thing))
          ~region:name.region
          (Problem.Unknown_kernel { module_name; exported_name })
    | Not_kernel ->
        Reported.map (resolved env Terms name) ~f:(fun resolved ->
            same (Canonical.Expr.Expr_ident resolved))

  let constructor env name = resolved env Terms name
  let type_reference env name = resolved env Types name
end

module Resolving = Scope.Traversal (Resolution)

let declared_arity (t : Canonical.Typedef.Impl.t) =
  match t.body with
  | Canonical.Typedef.Kind.Tkind_function { arguments; _ } ->
      List.length arguments
  | Tkind_var _ | Tkind_concrete _ | Tkind_unit | Tkind_record _ | Tkind_tuple _
    ->
      0

let agreeing_with_its_kernel (declaration : Canonical.Declaration.t) =
  let region = declaration.body_part.name.region in
  match declaration.body_part.expr.thing with
  | Canonical.Expr.Expr_kernel kernel -> begin
      match declaration.type_part_data with
      | None ->
          Reported.rejected declaration ~region
            (Problem.Kernel_needs_annotation
               { name = declaration.body_part.name.thing })
      | Some type_part ->
          let declared = declared_arity type_part.type_alias in
          let kernel = Data.Kernel.arity kernel in
          if declared = kernel then Reported.return declaration
          else
            Reported.rejected declaration ~region
              (Problem.Kernel_arity_mismatch { declared; kernel })
    end
  | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _ | Expr_list _
  | Expr_cons _ | Expr_tuple _ | Expr_let _ | Expr_if_then_else _
  | Expr_record_update _ | Expr_apply _ | Expr_ident _ | Expr_pattern _
  | Expr_accessor _ | Expr_access _ | Expr_record_extend _
  | Expr_record_select _ | Expr_record_empty | Expr_unit | Expr_lambda _ ->
      Reported.return declaration

let resolved_values declarations ~f =
  let resolved, errors =
    List.fold_left
      (fun (resolved, errors) declaration ->
        let value, problems = f declaration in
        (value :: resolved, List.rev_append problems errors))
      ([], []) declarations
  in
  (List.rev resolved, List.rev errors)

let resolved_declarations map ~f =
  let resolved, errors =
    Canonical.Module.String_map.fold
      (fun name value (acc, errors) ->
        let value, problems = f name value in
        ( Canonical.Module.String_map.add name value acc,
          List.rev_append problems errors ))
      map
      (Canonical.Module.String_map.empty, [])
  in
  (resolved, List.rev errors)

let in_module ~(dependencies : Canonical.Module.t list) (m : Canonical.Module.t)
    : (Canonical.Module.t, error list) result =
  let open Reported.Let_syntax in
  let env, import_errors = environment_of ~dependencies m in
  let top_declarations, declaration_errors =
    resolved_values m.top_declarations
      ~f:(fun (d : Canonical.Declaration.t) ->
        Reported.collected
          (let%bind declaration = Resolving.declaration env d in
           agreeing_with_its_kernel declaration))
  in
  let type_declarations, type_errors =
    resolved_declarations m.type_declarations
      ~f:(fun name (td : Canonical.Typedecl.t) ->
        let ctor (ctor : Canonical.Typedecl.type_ctor) =
          let%map data = Resolving.each ctor.data ~f:(Resolving.type_expression env) in
          { ctor with data }
        in
        Reported.collected
          (let%map ctors = Resolving.each td.ctors ~f:ctor in
           { td with ctors }))
  in
  let type_aliases, alias_errors =
    resolved_declarations m.type_aliases
      ~f:(fun name (ta : Canonical.Typealias.t) ->
        Reported.collected
          (let%map typedef = Resolving.type_expression env ta.typedef in
           { ta with typedef }))
  in
  let errors =
    import_errors @ duplicate_declarations m @ declaration_errors @ type_errors
    @ alias_errors
  in
  match errors with
  | [] -> Ok { m with top_declarations; type_declarations; type_aliases }
  | _ -> Error errors
