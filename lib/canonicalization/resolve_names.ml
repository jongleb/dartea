module Exports = Canonical.Exports
module Names = Exports.Names
module By_name = Exports.By_name

type problem =
  | Unknown_module of { qualifier : string }
  | Not_exposed of { module_name : string; name : string }
  | Ctors_not_exposed of { module_name : string; type_name : string }
  | Ambiguous of { name : string; modules : string list }
  | Unknown_kernel of { module_name : string; exported_name : string }
  | Kernel_needs_annotation
  | Kernel_arity_mismatch of { declared : int; kernel : int }
  | Duplicate_declaration of { name : string }
[@@deriving show]

type origin =
  | Import of string
  | Value_declaration of string Data.Located.t
  | Type_declaration of string
  | Type_alias of string
[@@deriving show]

type error = { origin : origin; problem : problem } [@@deriving show]

module Reported = struct
  module Basic = struct
    type 'a t = { value : 'a; problems : problem list }

    let return value = { value; problems = [] }

    let bind reported ~f =
      let next = f reported.value in
      { value = next.value; problems = reported.problems @ next.problems }

    let map = `Define_using_bind
  end

  include Basic
  include Base.Monad.Make (Basic)

  let rejected value problem = { value; problems = [ problem ] }

  let both left right =
    bind left ~f:(fun left -> map right ~f:(fun right -> (left, right)))

  let recovered ~fallback = function
    | Ok value -> return value
    | Error problem -> rejected fallback problem

  let blamed origin reported =
    ( reported.value,
      List.map (fun problem -> { origin; problem }) reported.problems )
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

let binds env binders =
  let scope = env.term_scope in
  {
    env with
    term_scope =
      { scope with visible = Scope.Binders.fold Names.add binders scope.visible };
  }

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

let repeated (declarations : (origin * string) list) : error list =
  List.fold_left
    (fun (declared, errors) (origin, name) ->
      if Names.mem name declared then
        (declared, { origin; problem = Duplicate_declaration { name } } :: errors)
      else (Names.add name declared, errors))
    (Names.empty, []) declarations
  |> snd |> List.rev

let duplicate_declarations (m : Canonical.Module.t) : error list =
  let values =
    List.map
      (fun (d : Canonical.Declaration.t) ->
        (Value_declaration d.body_part.name, Data.Located.unwrap d.body_part.name))
      m.top_declarations
  in
  let constructors =
    Canonical.Module.String_map.bindings m.type_declarations
    |> List.concat_map (fun (type_name, (td : Canonical.Typedecl.t)) ->
           List.map
             (fun (ctor : Canonical.Typedecl.type_ctor) ->
               (Type_declaration type_name, Data.Name.base ctor.id))
             td.ctors)
  in
  let named map ~origin =
    Canonical.Module.String_map.bindings map
    |> List.map (fun (name, _) -> (origin name, name))
  in
  let type_names =
    named m.type_declarations ~origin:(fun name -> Type_declaration name)
    @ named m.type_aliases ~origin:(fun name -> Type_alias name)
  in
  repeated values @ repeated constructors @ repeated type_names

let brought_in_by dependency (item : Canonical.Exposed.item) :
    ((namespace * Names.t) list, problem) result =
  let module_name = dependency.module_name in
  match item with
  | Value name when Names.mem name dependency.exports.terms ->
      Ok [ (Terms, Names.singleton name) ]
  | Value name -> Error (Not_exposed { module_name; name })
  | Type { name; ctors_exposed } -> begin
      let same_named_term =
        if Names.mem name dependency.exports.terms then
          [ (Terms, Names.singleton name) ]
        else []
      in
      match
        (By_name.find_opt name dependency.exports.types, ctors_exposed)
      with
      | None, _ -> Error (Not_exposed { module_name; name })
      | Some Exports.Ctors_hidden, true ->
          Error (Ctors_not_exposed { module_name; type_name = name })
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
    match By_name.find_opt module_name known with
    | None -> Reported.rejected env (Unknown_module { qualifier = module_name })
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
                    Reported.recovered ~fallback:env
                      (Result.map (bring env) (brought_in_by dependency item)))
              in
              List.fold_left take (Reported.return qualified) items
        end
  in
  let start =
    {
      term_scope =
        { visible = Exports.declared_terms m; sources = By_name.empty };
      type_scope =
        { visible = Exports.declared_types m; sources = By_name.empty };
      qualifiers = By_name.empty;
    }
  in
  let env, errors =
    List.fold_left
      (fun (env, errors) (import : Canonical.Import.t) ->
        let env, problems =
          Reported.blamed (Import import.module_name) (from_import env import)
        in
        (env, List.rev_append problems errors))
      (start, []) m.imports
  in
  (env, List.rev errors)

let refers_to env namespace (name : Data.Name.t) :
    (Data.Name.t, problem) result =
  match name with
  | Local text -> begin
      let scope = scope_of env namespace in
      match
        (Names.mem text scope.visible, By_name.find_opt text scope.sources)
      with
      | true, _ | false, None -> Ok name
      | false, Some { nearest; earlier = [] } ->
          Ok (Data.Name.global ~module_name:nearest ~exported_name:text)
      | false, Some { nearest; earlier } ->
          Error
            (Ambiguous { name = text; modules = List.rev (nearest :: earlier) })
    end
  | Global { module_name = qualifier; exported_name } -> begin
      match By_name.find_opt qualifier env.qualifiers with
      | None -> Error (Unknown_module { qualifier })
      | Some { module_name; exports }
        when exports_include exports namespace exported_name ->
          Ok (Data.Name.global ~module_name ~exported_name)
      | Some { module_name; _ } ->
          Error (Not_exposed { module_name; name = exported_name })
    end

let resolved env namespace name =
  Reported.recovered ~fallback:name (refers_to env namespace name)

module Resolution = struct
  type 'a t = 'a Reported.t
  type scope = env

  let return = Reported.return
  let map = Reported.map
  let both = Reported.both
  let extended = binds

  let reference env name =
    match Data.Kernel.referred_to_by name with
    | Known kernel -> Reported.return (Canonical.Expr.Expr_kernel kernel)
    | Unknown { module_name; exported_name } ->
        Reported.rejected (Canonical.Expr.Expr_ident name)
          (Unknown_kernel { module_name; exported_name })
    | Not_kernel ->
        Reported.map (resolved env Terms name) ~f:(fun name ->
            Canonical.Expr.Expr_ident name)

  let constructor env name = resolved env Terms name
  let type_reference env name = resolved env Types name
end

module Resolving = Scope.Traversal (Resolution)

let declared_arity (t : Canonical.Typedef.Impl.t) =
  match t.body with
  | Canonical.Typedef.Kind.Tkind_function { arguments } ->
      List.length arguments - 1
  | Tkind_var _ | Tkind_concrete _ | Tkind_unit | Tkind_record _ | Tkind_tuple _
    ->
      0

let agreeing_with_its_kernel (declaration : Canonical.Declaration.t) =
  match Data.Located.unwrap declaration.body_part.expr with
  | Canonical.Expr.Expr_kernel kernel -> begin
      match declaration.type_part_data with
      | None -> Reported.rejected declaration Kernel_needs_annotation
      | Some type_part ->
          let declared = declared_arity type_part.type_alias in
          let kernel = Data.Kernel.arity kernel in
          if declared = kernel then Reported.return declaration
          else
            Reported.rejected declaration
              (Kernel_arity_mismatch { declared; kernel })
    end
  | _ -> Reported.return declaration

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
        Reported.blamed (Value_declaration d.body_part.name)
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
        Reported.blamed (Type_declaration name)
          (let%map ctors = Resolving.each td.ctors ~f:ctor in
           { td with ctors }))
  in
  let type_aliases, alias_errors =
    resolved_declarations m.type_aliases
      ~f:(fun name (ta : Canonical.Typealias.t) ->
        Reported.blamed (Type_alias name)
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
