module type BACKEND = sig
  val extension : string
  val runtime_modules : Data.Name.t list -> (string * string) list

  val platform_kernel : Data.Name.t -> int option

  val emit_module :
    notice:string list ->
    arities:(Data.Name.t * int) list ->
    constructors:(Data.Name.t * int) list ->
    siblings:(Data.Name.t * (Data.Name.t * int) list) list ->
    typedecls:Optimized.Typedecl.t list ->
    imports:string list ->
    exports:Data.Name.t list ->
    Optimized.Declaration.t list ->
    string
end

module Js_backend : BACKEND = struct
  let extension = Codegen_js.Of_optimized.extension

  let runtime_modules used =
    ( Codegen_js.Of_optimized.runtime_module_name,
      Codegen_js.Of_optimized.runtime_module_source () )
    :: Codegen_js.Platform_kernel.runtimes used

  let platform_kernel = Codegen_js.Platform_kernel.arity

  let emit_module ~notice ~arities ~constructors ~siblings ~typedecls ~imports
      ~exports decls =
    Codegen_js.Of_optimized.emit_module ~notice ~arities ~constructors ~siblings
      ~typedecls ~imports ~exports decls
end

type compiled = {
  module_name : string;
  source : string;
  exports : string list;
  warnings : Reporting.Warning.t list;
}

type outcome = {
  output : compiled list;
  errors : Reporting.Error.t list;
  sources : (string * string) list;
  entry : Entry.t option;
}

module Module_names = Canonical.Exports.Names

let prelude_file module_ = Prelude.name module_ ^ Project.Elm_file.extension

let imported_by (module_ : Canonical.Module.t) =
  List.map (fun (import : Canonical.Import.t) -> import.module_name)
    module_.imports

let reachable ~wanted prelude =
  let rec grown needed =
    let reached =
      List.fold_left
        (fun found (module_ : Canonical.Module.t) ->
          if Module_names.mem module_.name found then
            Module_names.union found
              (Module_names.of_list (imported_by module_))
          else found)
        needed prelude
    in
    if Module_names.equal reached needed then needed else grown reached
  in
  let needed = grown wanted in
  List.filter
    (fun (module_ : Canonical.Module.t) ->
      Module_names.mem module_.name needed)
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
    output : compiled list;
    errors : Reporting.Error.t list;
    entry : Entry.t option;
  }

  let providing_modules declarations =
    After_typed.Scope.referenced_in_declarations declarations
    |> After_typed.Scope.Names.elements
    |> List.filter_map (fun (name : Data.Name.t) ->
           match name with
           | Data.Name.Global { module_name; _ } -> Some module_name
           | Data.Name.Local _ -> None)
    |> List.sort_uniq String.compare

  let prepared (typed : Infer.Declarations.infer_result) =
    let declarations =
      match
        After_typed.Optimize.optimize typed.declarations
        |> After_typed.Dependency_sort.sort_declarations
      with
      | Ok declarations -> declarations
      | Error (After_typed.Dependency_sort.Bad_recursion names) ->
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
    let constructors =
      List.map
        (fun (c : Infer.Type_env.ctor_info) -> (c.name, c.arity))
        typed.constructors
    in
    ( declarations,
      constructors,
      Data.Name.Map.bindings typed.siblings_env )

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
    let declared =
      List.fold_left
        (fun acc (d : Typed.Declaration.t) ->
          Names.add (Data.Located.unwrap d.name) acc)
        Names.empty typed.declarations
    in
    let own_constructors =
      List.fold_left
        (fun acc (c : Infer.Type_env.ctor_info) ->
          match c.name with
          | Data.Name.Local ctor -> Names.add ctor acc
          | Data.Name.Global _ -> acc)
        Names.empty typed.constructors
    in
    Names.inter exports.terms (Names.union declared own_constructors)
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
    let importing = List.nth_opt modules 0 in
    List.find_map
      (fun (source : Project.Elm_file.t) ->
        let module_ = module_of source in
        match importing with
        | Some name when String.equal module_.name name ->
            List.find_map
              (fun (import : Canonical.Import.t) ->
                if List.mem import.module_name modules then Some import.region
                else None)
              module_.imports
        | Some _ | None -> None)
      sources
    |> Option.value ~default:Data.Region.nowhere

  let compile_modules ~entry (sources : Project.Elm_file.t list) :
      outcome =
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
    let notice_for name =
      List.find_opt
        (fun module_ -> String.equal (Prelude.name module_) name)
        Prelude.all
      |> Option.map Prelude.notice
      |> Option.value ~default:[]
    in
    let used_kernels = ref [] in
    let platform_kernel name =
      match B.platform_kernel name with
      | Some arity ->
          if not (List.exists (Data.Name.equal name) !used_kernels) then
            used_kernels := name :: !used_kernels;
          Some arity
      | None -> None
    in
    let compiling progress module_ =
      match resolved_against ~platform_kernel progress.dependencies module_ with
      | Error found ->
          { progress with errors = List.rev_append found progress.errors }
      | Ok resolved -> (
          let imports = imported_interfaces resolved progress.interfaces in
          let depended =
            { progress with dependencies = resolved :: progress.dependencies }
          in
          match Infer.Declarations.infer_toplevel ~imports resolved with
          | exception Reporting.Error.Found error ->
              { depended with errors = error :: depended.errors }
          | typed ->
              let known =
                {
                  depended with
                  interfaces =
                    Infer.Declarations.interface_of resolved typed
                    :: depended.interfaces;
                }
              in
              if typed.errors <> [] then
                {
                  known with
                  errors = List.rev_append typed.errors known.errors;
                }
              else
                let declarations, constructors, siblings = prepared typed in
                let exports = exported_names resolved typed in
                let roots =
                  match entry with
                  | Some wanted when String.equal wanted resolved.name ->
                      Data.Name.local Entry.declaration :: exports
                  | Some _ | None -> exports
                in
                let declarations =
                  After_typed.Dead_code.alive ~roots declarations
                in
                let compiled =
                  {
                    module_name = resolved.name;
                    exports = List.map Data.Name.base exports;
                    source =
                      B.emit_module ~notice:(notice_for resolved.name)
                        ~arities:(imported_arities imports)
                        ~constructors ~siblings ~typedecls:(shaped_types typed)
                        ~imports:(providing_modules declarations)
                        ~exports declarations;
                    warnings =
                      List.concat_map
                        (After_typed.Exhaustiveness_check.warnings
                           typed.siblings_env)
                        typed.declarations;
                  }
                in
                let started =
                  match entry with
                  | Some wanted when String.equal wanted resolved.name ->
                      Entry.of_declarations ~module_name:resolved.name
                        typed.declarations
                  | Some _ | None -> known.entry
                in
                { known with output = compiled :: known.output; entry = started })
    in
    let compile_module progress module_ =
      match compiling progress module_ with
      | outcome -> outcome
      | exception Reporting.Error.Found error ->
          { progress with errors = error :: progress.errors }
    in
    match
      let written_modules = List.map module_of sources in
      let wanted =
        Module_names.of_list (List.concat_map imported_by written_modules)
      in
      Canonicalization.Module_graph.in_dependency_order
        (reachable ~wanted (Lazy.force prelude_modules) @ written_modules)
    with
    | exception Reporting.Error.Found error ->
        { output = []; errors = [ error ]; sources = written; entry = None }
    | Error (Canonicalization.Module_graph.Import_cycle modules) ->
        {
          output = [];
          sources = written;
          entry = None;
          errors =
            [
              Reporting.Error.name ~region:(cycle_region ~modules sources)
                (Reporting.Name_error.Import_cycle { modules });
            ];
        }
    | Ok ordered ->
        let finished =
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
        let compiled_modules = List.rev finished.output in
        let runtime =
          List.map
            (fun (module_name, source) ->
              { module_name; source; exports = []; warnings = [] })
            (B.runtime_modules !used_kernels)
        in
        let errors =
          match (entry, finished.entry, finished.errors) with
          | Some module_name, None, [] ->
              [
                Reporting.Error.project
                  (No_entry { module_name; declaration = Entry.declaration });
              ]
          | _, _, errors -> List.rev errors
        in
        {
          output = (if errors = [] then runtime @ compiled_modules else []);
          errors;
          sources = written;
          entry = finished.entry;
        }

  let compile_source (content : string) : outcome =
    compile_modules ~entry:None
      [
        Project.Elm_file.of_path
          ~path:("Main" ^ Project.Elm_file.extension)
          content;
      ]
end

include Make (Js_backend)
