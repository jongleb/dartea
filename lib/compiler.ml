module type BACKEND = sig
  val extension : string
  val runtime_modules : (string * string list) list -> (string * string) list

  val platform_kernel : Data.Name.t -> int option
  val through : Optimized.Expr.t -> Optimized.Expr.t list option

  val emit_module :
    blocks:bool ->
    arities:(Data.Name.t * int) list ->
    constructors:(Data.Name.t * int) list ->
    built:(Data.Name.t * int) list ->
    siblings:(Data.Name.t * (Data.Name.t * int) list) list ->
    typedecls:Optimized.Typedecl.t list ->
    imports:string list ->
    exports:Data.Name.t list ->
    Optimized.Declaration.t list ->
    string * (string * string list) list
end

module Js_backend : BACKEND = struct
  let extension = Codegen_js.Of_optimized.extension

  let runtime_modules used =
    List.filter_map
      (fun (module_name, source) ->
        match List.assoc_opt module_name used with
        | None -> None
        | Some helpers ->
            Some (module_name, Codegen_js.Shake.alive ~roots:helpers source))
      Codegen_js.Runtime.files

  let platform_kernel = Codegen_js.Platform_kernel.arity

  let through expression =
    Option.map
      (fun (_, holes) ->
        List.map (fun (hole : Codegen_js.Blocks.hole) -> hole.value) holes)
      (Codegen_js.Blocks.of_expression expression)

  let emit_module ~blocks ~arities ~constructors ~built ~siblings
      ~typedecls ~imports ~exports decls =
    Codegen_js.Of_optimized.emit_module ~blocks ~arities ~constructors
      ~built ~siblings ~typedecls ~imports ~exports decls
end

type artifact = {
  module_name : string;
  source : string;
  exports : string list;
  warnings : Reporting.Warning.t list;
}

type linkable = {
  module_name : string;
  arities : (Data.Name.t * int) list;
  constructors : (Data.Name.t * int) list;
  siblings : (Data.Name.t * (Data.Name.t * int) list) list;
  typedecls : Optimized.Typedecl.t list;
  exports : Data.Name.t list;
  declarations : Optimized.Declaration.t list;
  warnings : Reporting.Warning.t list;
}

type outcome = {
  modules : linkable list;
  written : string list;
  errors : Reporting.Error.t list;
  sources : (string * string) list;
  entry : Entry.t option;
}

module Reached = Set.Make (struct
  type t = string * string

  let compare (one_home, one_name) (other_home, other_name) =
    match String.compare one_home other_home with
    | 0 -> String.compare one_name other_name
    | ordering -> ordering
end)

module Bodies = Map.Make (struct
  type t = string * string

  let compare (one_home, one_name) (other_home, other_name) =
    match String.compare one_home other_home with
    | 0 -> String.compare one_name other_name
    | ordering -> ordering
end)

let address ~home (name : Data.Name.t) =
  match name with
  | Data.Name.Local own -> (home, own)
  | Data.Name.Global { module_name; exported_name } ->
      (module_name, exported_name)

let name_of_declaration (declaration : Optimized.Declaration.t) =
  Data.Located.unwrap declaration.name

let entry_root (entry : Entry.t) =
  Data.Name.global ~module_name:entry.module_name
    ~exported_name:entry.declaration

let everything outcome =
  Option.to_list (Option.map entry_root outcome.entry)
  @ List.concat_map
    (fun module_ ->
      List.map
        (fun name ->
          Data.Name.global ~module_name:module_.module_name
            ~exported_name:(Data.Name.base name))
        module_.exports)
    outcome.modules

module Module_names = Canonical.Exports.Names

let prelude_file module_ = Prelude.name module_ ^ Project.Elm_file.extension

let imported_by (module_ : Canonical.Module.t) =
  List.map (fun (import : Canonical.Import.t) -> import.module_name)
    module_.imports

let reachable ~demand prelude =
  let rec grown closure =
    let next =
      List.fold_left
        (fun found (module_ : Canonical.Module.t) ->
          if Module_names.mem module_.name found then
            Module_names.union found
              (Module_names.of_list (imported_by module_))
          else found)
        closure prelude
    in
    if Module_names.equal next closure then closure else grown next
  in
  let closure = grown demand in
  List.filter
    (fun (module_ : Canonical.Module.t) ->
      Module_names.mem module_.name closure)
    prelude

let frontend_module ~file content =
  match Parse.Main.parse ~file content with
  | Error error -> raise (Reporting.Error.Found error)
  | Ok impl_list -> Frontend.Module.of_impl impl_list

let parsed_module ~file ~fallback_name content =
  Canonical.Module.of_frontend ~fallback_name (frontend_module ~file content)

let named_after_path ~expected (declared : string Data.Located.t option) =
  match declared with
  | Some name when not (String.equal (Data.Located.unwrap name) expected) ->
      Reporting.Error.raise_syntax ~region:name.region
        (Reporting.Syntax_error.Module_name_mismatch { expected })
  | Some _ | None -> expected

module Make (B : BACKEND) = struct
  let extension = B.extension

  type progress = {
    dependencies : Canonical.Module.t list;
    interfaces : Interface.t list;
    output : linkable list;
    errors : Reporting.Error.t list;
    entry : Entry.t option;
  }

  let is_prelude name =
    List.exists (fun module_ -> String.equal (Prelude.name module_) name) Prelude.all

  let blocks_for module_name = not (is_prelude module_name)

  let through_of module_name =
    if blocks_for module_name then B.through else Optimized.Expr.whole

  let providing_modules ~module_name declarations =
    Optimized.Declaration.references_in_all ~through:(through_of module_name) declarations
    |> Data.Name.Set.elements
    |> List.filter_map (fun (name : Data.Name.t) ->
           match name with
           | Data.Name.Global { module_name; _ } -> Some module_name
           | Data.Name.Local _ -> None)
    |> List.sort_uniq String.compare

  let inline_modules = [ "Html"; "Html.Attributes"; "Html.Events"; "Html.Keyed" ]

  let imports_of (compiled : linkable list) =
    List.filter_map
      (fun (module_ : linkable) ->
        if List.mem module_.module_name inline_modules then
          Some
            {
              After_typed.Optimize.Import.module_name = module_.module_name;
              exports = module_.exports;
              declarations = module_.declarations;
            }
        else None)
      compiled

  let prepare ~imports (typed : Infer.Declarations.infer_result) =
    let declarations =
      match
        After_typed.Optimize.optimize ~imports typed.declarations
        |> Optimized.Declaration.in_dependency_order
      with
      | Ok declarations -> declarations
      | Error (Optimized.Declaration.Bad_recursion names) ->
          let written = List.map Data.Name.base names in
          let region =
            List.find_map
              (fun (declaration : Typed.Declaration.t) ->
                if List.mem (Data.Located.unwrap declaration.name) written then
                  Some declaration.name.region
                else None)
              typed.declarations
            |> Option.value ~default:Data.Region.nowhere
          in
          Reporting.Error.raise_name ~region
            (Reporting.Name_error.Recursive_value { names = written })
    in
    ( declarations,
      List.concat_map Canonical.Typedecl.arities (List.rev typed.typedecls),
      Data.Name.Map.bindings (Canonical.Typedecl.siblings typed.typedecls) )

  let shaped_types (typed : Infer.Declarations.infer_result) :
      Optimized.Typedecl.t list =
    List.map
      (fun (declared : Canonical.Typedecl.t) ->
        let params, ctors = Infer.Type_env.typedecl_payloads declared in
        {
          Optimized.Typedecl.name = declared.name;
          params;
          ctors =
            List.map
              (fun (id, payload) -> { Optimized.Typedecl.id; payload })
              ctors;
        })
      typed.typedecls

  let prelude_modules =
    lazy
      (List.map
         (fun module_ ->
           parsed_module
             ~file:(prelude_file module_)
             ~fallback_name:(Prelude.name module_)
             (Prelude.source module_))
         Prelude.all)

  let module_of (source : Project.Elm_file.t) =
    let frontend = frontend_module ~file:source.path source.content in
    let name = named_after_path ~expected:source.name frontend.name in
    let module_ = Canonical.Module.of_frontend ~fallback_name:name frontend in
    { module_ with imports = Prelude.default_imports @ module_.imports }

  let resolved_against ~platform_kernel dependencies
      (module_ : Canonical.Module.t) =
    Canonicalization.Resolve_names.in_module ~platform_kernel ~dependencies
      module_

  let exported_names (module_ : Canonical.Module.t)
      (typed : Infer.Declarations.infer_result) =
    let open Canonical.Exports in
    let exports = of_module module_ in
    let own_names =
      List.fold_left
        (fun acc (d : Typed.Declaration.t) ->
          Names.add (Data.Located.unwrap d.name) acc)
        Names.empty typed.declarations
    in
    let own_constructors =
      List.fold_left
        (fun acc (name, _) ->
          match name with
          | Data.Name.Local ctor -> Names.add ctor acc
          | Data.Name.Global _ -> acc)
        Names.empty
        (List.concat_map Canonical.Typedecl.arities typed.typedecls)
    in
    Names.inter exports.terms (Names.union own_names own_constructors)
    |> Names.elements |> List.map Data.Name.local

  let imported_arities (imports : Interface.t list) =
    List.concat_map
      (fun (interface : Interface.t) ->
        List.map
          (fun (value : Interface.value) -> (value.name, Interface.arity value))
          interface.values)
      imports

  let imported_interfaces (module_ : Canonical.Module.t) interfaces =
    List.filter
      (fun (interface : Interface.t) ->
        List.exists
          (fun (import : Canonical.Import.t) ->
            String.equal import.module_name interface.module_name)
          module_.imports)
      interfaces

  let cycle_region ~modules sources =
    let importer = List.nth_opt modules 0 in
    List.find_map
      (fun (source : Project.Elm_file.t) ->
        let module_ = module_of source in
        match importer with
        | Some name when String.equal module_.name name ->
            List.find_map
              (fun (import : Canonical.Import.t) ->
                if List.mem import.module_name modules then Some import.region
                else None)
              module_.imports
        | Some _ | None -> None)
      sources
    |> Option.value ~default:Data.Region.nowhere

  let bodies_of modules =
    List.fold_left
      (fun found module_ ->
        List.fold_left
          (fun found declaration ->
            Bodies.add
              (module_.module_name, name_of_declaration declaration)
              (Optimized.Declaration.free ~through:(through_of module_.module_name) declaration)
              found)
          found module_.declarations)
      Bodies.empty modules

  let reach ~roots modules =
    let bodies = bodies_of modules in
    let rec grown seen = function
      | [] -> seen
      | key :: rest when Reached.mem key seen -> grown seen rest
      | key :: rest -> (
          let seen = Reached.add key seen in
          match Bodies.find_opt key bodies with
          | None -> grown seen rest
          | Some free ->
              grown seen
                (List.map (address ~home:(fst key))
                   (Data.Name.Set.elements free)
                @ rest))
    in
    grown Reached.empty (List.map (address ~home:"") roots)

  let own_constructors module_ =
    List.filter_map
      (fun (name, arity) ->
        match name with
        | Data.Name.Local base -> Some (base, arity)
        | Data.Name.Global _ -> None)
      module_.constructors

  let link_module ~alive module_ =
    let survives name = Reached.mem (module_.module_name, name) alive in
    let own =
      List.map name_of_declaration module_.declarations
      @ List.map fst (own_constructors module_)
    in
    let declarations =
      List.filter
        (fun declaration -> survives (name_of_declaration declaration))
        module_.declarations
    in
    let built =
      List.filter
        (fun (name, _) ->
          match name with
          | Data.Name.Local base -> survives base
          | Data.Name.Global _ -> false)
        module_.constructors
    in
    let exports =
      List.filter
        (fun name ->
          let base = Data.Name.base name in
          (not (List.mem base own)) || survives base)
        module_.exports
    in
    if List.is_empty declarations && List.is_empty built && List.is_empty exports
    then None
    else
      let source, runtimes =
        B.emit_module ~blocks:(blocks_for module_.module_name)
          ~arities:module_.arities
          ~constructors:module_.constructors ~built ~siblings:module_.siblings
          ~typedecls:module_.typedecls
          ~imports:(providing_modules ~module_name:module_.module_name declarations)
          ~exports declarations
      in
      Some
        ( {
            module_name = module_.module_name;
            exports = List.map Data.Name.base exports;
            warnings = module_.warnings;
            source;
          },
          runtimes )

  let merge found used =
    List.fold_left
      (fun found (module_name, helpers) ->
        let known =
          Option.value (List.assoc_opt module_name found) ~default:[]
        in
        let grown =
          List.fold_left
            (fun kept helper ->
              if List.mem helper kept then kept else helper :: kept)
            known helpers
        in
        (module_name, grown) :: List.remove_assoc module_name found)
      found used

  let link ~roots outcome =
    let alive = reach ~roots outcome.modules in
    let pieces = List.filter_map (link_module ~alive) outcome.modules in
    let gather found (_, used) = merge found used in
    let usage = List.fold_left gather [] pieces in
    List.map
      (fun (module_name, source) ->
        { module_name; source; exports = []; warnings = [] })
      (B.runtime_modules usage)
    @ List.map fst pieces

  let compile_modules ~entry (checked : Project.Sources.t) : outcome =
    let sources = Project.Sources.files checked in
    let written =
      List.map
        (fun (source : Project.Elm_file.t) ->
          (source.path, source.content))
        sources
      @ List.map
          (fun module_ ->
            (prelude_file module_, Prelude.source module_))
          Prelude.all
    in
    let platform_kernel = B.platform_kernel in
    let compile_one progress module_ =
      match resolved_against ~platform_kernel progress.dependencies module_ with
      | Error found ->
          { progress with errors = List.rev_append found progress.errors }
      | Ok resolved -> (
          let imports = imported_interfaces resolved progress.interfaces in
          let deps =
            { progress with dependencies = resolved :: progress.dependencies }
          in
          match Infer.Declarations.infer_toplevel ~imports resolved with
          | exception Reporting.Error.Found error ->
              { deps with errors = error :: deps.errors }
          | typed ->
              let known =
                {
                  deps with
                  interfaces =
                    Infer.Declarations.interface_of resolved typed
                    :: deps.interfaces;
                }
              in
              if not (List.is_empty typed.errors) then
                {
                  known with
                  errors = List.rev_append typed.errors known.errors;
                }
              else
                let declarations, constructors, siblings =
                  prepare ~imports:(imports_of known.output) typed
                in
                let exports = exported_names resolved typed in
                let roots =
                  match entry with
                  | Some demand when String.equal demand resolved.name ->
                      Data.Name.local Entry.declaration :: exports
                  | Some _ | None -> exports
                in
                let declarations =
                  Optimized.Declaration.alive ~roots declarations
                in
                let linkable_module =
                  {
                    module_name = resolved.name;
                    arities = imported_arities imports;
                    constructors;
                    siblings;
                    typedecls = shaped_types typed;
                    exports;
                    declarations;
                    warnings =
                      List.concat_map
                        (After_typed.Exhaustive.warnings
                           (Canonical.Typedecl.siblings typed.typedecls))
                        typed.declarations;
                  }
                in
                let entry_now =
                  match entry with
                  | Some demand when String.equal demand resolved.name ->
                      Entry.of_declarations ~module_name:resolved.name
                        typed.declarations
                  | Some _ | None -> known.entry
                in
                {
                  known with
                  output = linkable_module :: known.output;
                  entry = entry_now;
                })
    in
    let compile_module progress module_ =
      match compile_one progress module_ with
      | outcome -> outcome
      | exception Reporting.Error.Found error ->
          { progress with errors = error :: progress.errors }
    in
    let ordered_modules () =
      let written_modules = List.map module_of sources in
      let demand =
        Module_names.of_list (List.concat_map imported_by written_modules)
      in
      Canonical.Module.in_dependency_order
        (reachable ~demand (Lazy.force prelude_modules) @ written_modules)
    in
    match ordered_modules () with
    | exception Reporting.Error.Found error ->
        {
          modules = [];
          written = [];
          errors = [ error ];
          sources = written;
          entry = None;
        }
    | Error (Canonical.Module.Import_cycle modules) ->
        {
          modules = [];
          written = [];
          sources = written;
          entry = None;
          errors =
            [
              Reporting.Error.name ~region:(cycle_region ~modules sources)
                (Reporting.Name_error.Import_cycle { modules });
            ];
        }
    | Ok ordered ->
        let final =
          List.fold_left compile_module
            {
              dependencies = [];
              interfaces = [];
              output = [];
              errors = [];
              entry = None;
            }
            ordered
        in
        let compiled_modules = List.rev final.output in
        let errors =
          match (entry, final.entry, final.errors) with
          | Some module_name, None, [] ->
              [
                Reporting.Error.of_failure
                  (Diagnostic.Failure.about
                     (No_entry
                        { module_name; declaration = Entry.declaration }));
              ]
          | _, _, errors -> List.rev errors
        in
        let modules = if List.is_empty errors then compiled_modules else [] in
        {
          modules;
          written = List.filter_map Project.Elm_file.written sources;
          errors;
          sources = written;
          entry = final.entry;
        }

  let compile_source (content : string) : outcome =
    compile_modules ~entry:None
      (Project.Sources.of_list
         [
           Project.Elm_file.of_path
             ~path:("Main" ^ Project.Elm_file.extension)
             content;
         ])
end

include Make (Js_backend)
