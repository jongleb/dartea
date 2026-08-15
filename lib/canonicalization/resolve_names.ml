module Exports = Canonical.Exports
module Names = Exports.Names
module By_name = Exports.By_name

type problem =
  | Unknown_module of { qualifier : string }
  | Not_exposed of { module_name : string; name : string }
  | Ctors_not_exposed of { module_name : string; type_name : string }
  | Ambiguous of { name : string; modules : string list }
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

  let recovered ~fallback = function
    | Ok value -> return value
    | Error problem -> rejected fallback problem

  let each items ~f = all (List.map f items)

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

let binds env names =
  let scope = env.term_scope in
  {
    env with
    term_scope = { scope with visible = Names.union scope.visible names };
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

let duplicate_declarations (m : Canonical.Module.t) : error list =
  let clashing_ctors =
    let declaring_types =
      Canonical.Module.String_map.fold
        (fun type_name (td : Canonical.Typedecl.t) owners ->
          List.fold_left
            (fun owners (ctor : Canonical.Typedecl.type_ctor) ->
              By_name.update (Data.Name.base ctor.id)
                (fun declared ->
                  Some (type_name :: Option.value declared ~default:[]))
                owners)
            owners td.ctors)
        m.type_declarations By_name.empty
    in
    By_name.fold
      (fun name owners errors ->
        match List.rev owners with
        | [] | [ _ ] -> errors
        | _first :: repeated ->
            List.map
              (fun type_name ->
                {
                  origin = Type_declaration type_name;
                  problem = Duplicate_declaration { name };
                })
              repeated
            @ errors)
      declaring_types []
  in
  let clashing_type_names =
    Canonical.Module.String_map.fold
      (fun name _ errors ->
        match Canonical.Module.String_map.find_opt name m.type_declarations with
        | None -> errors
        | Some _ ->
            {
              origin = Type_alias name;
              problem = Duplicate_declaration { name };
            }
            :: errors)
      m.type_aliases []
    |> List.rev
  in
  clashing_ctors @ clashing_type_names

let brought_in_by dependency (item : Canonical.Exposed.item) :
    ((namespace * Names.t) list, problem) result =
  let module_name = dependency.module_name in
  match item with
  | Value name when Names.mem name dependency.exports.terms ->
      Ok [ (Terms, Names.singleton name) ]
  | Value name -> Error (Not_exposed { module_name; name })
  | Type { name; ctors_exposed } -> begin
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
          Ok [ (Types, Names.singleton name) ]
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

let rec bound_by_pattern (p : Canonical.Pattern.t) : Names.t =
  match p with
  | P_var name -> Names.singleton name
  | P_record fields -> Names.of_list fields
  | P_tuple items | P_list items ->
      List.fold_left
        (fun acc item -> Names.union acc (bound_by_pattern item))
        Names.empty items
  | P_cons (head, tail) ->
      Names.union (bound_by_pattern head) (bound_by_pattern tail)
  | P_ctor (_, arguments) ->
      List.fold_left
        (fun acc argument -> Names.union acc (bound_by_pattern argument))
        Names.empty arguments
  | P_anything | P_unit | P_chr _ | P_str _ | P_int _ -> Names.empty

let rec pattern env (p : Canonical.Pattern.t) : Canonical.Pattern.t Reported.t =
  let open Canonical.Pattern in
  let open Reported.Let_syntax in
  match p with
  | P_ctor (name, arguments) ->
      let%map ctor = resolved env Terms name
      and arguments = Reported.each arguments ~f:(pattern env) in
      P_ctor (ctor, arguments)
  | P_tuple items ->
      let%map items = Reported.each items ~f:(pattern env) in
      P_tuple items
  | P_list items ->
      let%map items = Reported.each items ~f:(pattern env) in
      P_list items
  | P_cons (head, tail) ->
      let%map head = pattern env head and tail = pattern env tail in
      P_cons (head, tail)
  | (P_anything | P_var _ | P_record _ | P_unit | P_chr _ | P_str _ | P_int _)
    as leaf ->
      Reported.return leaf

let rec expression env (e : Canonical.Expr.t) : Canonical.Expr.t Reported.t =
  let open Canonical.Expr in
  let open Reported.Let_syntax in
  match e with
  | Expr_ident name ->
      let%map name = resolved env Terms name in
      Expr_ident name
  | Expr_constr { name; arguments } ->
      let%map name = resolved env Terms name
      and arguments = Reported.each arguments ~f:(expression env) in
      Expr_constr { name; arguments }
  | Expr_binop { name; operands = left, right } ->
      let%map left = expression env left and right = expression env right in
      Expr_binop { name; operands = (left, right) }
  | Expr_apply { fn; arg } ->
      let%map fn = expression env fn and arg = expression env arg in
      Expr_apply { fn; arg }
  | Expr_let { binding = { bind_body = { name; body = bound_value } }; body } ->
      let inner = binds env (Names.singleton (Data.Located.unwrap name)) in
      let%map bound_value = expression env bound_value
      and body = expression inner body in
      Expr_let { binding = { bind_body = { name; body = bound_value } }; body }
  | Expr_if_then_else { if_exp; then_exp; else_exp } ->
      let%map if_exp = expression env if_exp
      and then_exp = expression env then_exp
      and else_exp = expression env else_exp in
      Expr_if_then_else { if_exp; then_exp; else_exp }
  | Expr_record rows ->
      let row (row : expr_record_row) =
        let%map value = expression env row.value in
        { row with value }
      in
      let%map rows = Reported.each rows ~f:row in
      Expr_record rows
  | Expr_pattern { expr; pattern_data_items } ->
      let case (item : expr_pattern_case) =
        let inner = binds env (bound_by_pattern item.pattern) in
        let%map pattern = pattern env item.pattern
        and expr = expression inner item.expr in
        { pattern; expr }
      in
      let%map expr = expression env expr
      and pattern_data_items = Reported.each pattern_data_items ~f:case in
      Expr_pattern { expr; pattern_data_items }
  | Expr_lambda { params; body } ->
      let inner =
        binds env (Names.of_list (List.map Data.Located.unwrap params))
      in
      let%map body = expression inner body in
      Expr_lambda { params; body }
  | Expr_access { expr; field } ->
      let%map expr = expression env expr in
      Expr_access { expr; field }
  | Expr_list items ->
      let%map items = Reported.each items ~f:(expression env) in
      Expr_list items
  | ( Expr_accessor _ | Expr_record_extend _ | Expr_record_select _
    | Expr_record_empty | Expr_unit | Expr_char _ | Expr_string _ | Expr_int _
    | Expr_float _ ) as leaf ->
      Reported.return leaf

let rec type_expression env (t : Canonical.Typedef.Impl.t) :
    Canonical.Typedef.Impl.t Reported.t =
  let open Canonical.Typedef in
  let open Reported.Let_syntax in
  let%map parameters = Reported.each t.parameters ~f:(type_expression env)
  and body =
    match t.body with
    | Kind.Tkind_concrete written ->
        let%map name = resolved env Types (Data.Located.unwrap written) in
        Kind.Tkind_concrete { written with Data.Located.thing = name }
    | Kind.Tkind_record { values; row_type } ->
        let row (row : Type_record_row.t) =
          let%map body = type_expression env row.body in
          { row with Type_record_row.body }
        in
        let%map values = Reported.each values ~f:row in
        Kind.Tkind_record { values; row_type }
    | Kind.Tkind_tuple items ->
        let%map items = Reported.each items ~f:(type_expression env) in
        Kind.Tkind_tuple items
    | Kind.Tkind_function { arguments } ->
        let%map arguments = Reported.each arguments ~f:(type_expression env) in
        Kind.Tkind_function { arguments }
    | (Kind.Tkind_var _ | Kind.Tkind_unit) as leaf -> Reported.return leaf
  in
  { Impl.parameters; body }

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
    resolved_declarations m.top_declarations
      ~f:(fun _ (d : Canonical.Declaration.t) ->
        let inner =
          binds env
            (Names.of_list (List.map Data.Located.unwrap d.body_part.params))
        in
        Reported.blamed (Value_declaration d.body_part.name)
          (let%map type_part_data =
             match d.type_part_data with
             | None -> Reported.return None
             | Some (tp : Canonical.Declaration.type_part) ->
                 let%map type_alias = type_expression env tp.type_alias in
                 Some { tp with type_alias }
           and expr =
             let%map expr =
               expression inner (Data.Located.unwrap d.body_part.expr)
             in
             { d.body_part.expr with Data.Located.thing = expr }
           in
           {
             Canonical.Declaration.type_part_data;
             body_part = { d.body_part with expr };
           }))
  in
  let type_declarations, type_errors =
    resolved_declarations m.type_declarations
      ~f:(fun name (td : Canonical.Typedecl.t) ->
        let ctor (ctor : Canonical.Typedecl.type_ctor) =
          let%map data = Reported.each ctor.data ~f:(type_expression env) in
          { ctor with data }
        in
        Reported.blamed (Type_declaration name)
          (let%map ctors = Reported.each td.ctors ~f:ctor in
           { td with ctors }))
  in
  let type_aliases, alias_errors =
    resolved_declarations m.type_aliases
      ~f:(fun name (ta : Canonical.Typealias.t) ->
        Reported.blamed (Type_alias name)
          (let%map typedef = type_expression env ta.typedef in
           { ta with typedef }))
  in
  let errors =
    import_errors @ duplicate_declarations m @ declaration_errors @ type_errors
    @ alias_errors
  in
  match errors with
  | [] -> Ok { m with top_declarations; type_declarations; type_aliases }
  | _ -> Error errors
