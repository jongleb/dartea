let initial_ctx =
  List.fold_left
    (fun ctx (operator, scheme) ->
      Infer.Infer_proc.Name_map.add (Data.Name.local operator) scheme ctx)
    Infer.Infer_proc.Name_map.empty Primitives.values

module type BACKEND = sig
  val extension : string
  val runtime_module : unit -> (string * string) option

  val emit_module :
    arities:(Data.Name.t * int) list ->
    constructors:(Data.Name.t * int) list ->
    siblings:(Data.Name.t * (Data.Name.t * int) list) list ->
    imports:string list ->
    exports:Data.Name.t list ->
    Optimized.Declaration.t list ->
    string
end

module Js_backend : BACKEND = struct
  let extension = Codegen.Js_of_optimized.extension

  let runtime_module () =
    Some
      ( Codegen.Js_of_optimized.runtime_module_name,
        Codegen.Js_of_optimized.runtime_module_source () )

  let emit_module ~arities ~constructors ~siblings ~imports ~exports decls =
    Codegen.Js_of_optimized.emit_module ~arities ~constructors ~siblings
      ~imports ~exports decls
end

type compiled = {
  module_name : string;
  source : string;
  warnings : string list;
}

let parsed_module ~fallback_name content =
  match Parse.Main.parse content with
  | Error e -> raise e
  | Ok impl_list ->
      Canonical.Module.of_frontend ~fallback_name
        (Frontend.Module.of_impl impl_list)

module Make (B : BACKEND) = struct
  let extension = B.extension

  type progress = {
    dependencies : Canonical.Module.t list;
    interfaces : Interface.t list;
    output : compiled list;
  }

  let providing_modules declarations =
    After_typed.Scope.referenced_in_declarations declarations
    |> After_typed.Scope.Names.elements
    |> List.filter_map (fun (name : Data.Name.t) ->
           match name with
           | Data.Name.Global { module_name; _ } -> Some module_name
           | Data.Name.Local _ -> None)
    |> List.sort_uniq String.compare

  let prepared (typed : Infer.Infer_proc.infer_result) =
    let declarations =
      After_typed.Optimize.optimize typed.declarations
      |> After_typed.Dependency_sort.sort_declarations
    in
    let constructors =
      List.map
        (fun (c : Infer.Infer_proc.ctor_info) -> (c.name, c.arity))
        typed.constructors
    in
    ( declarations,
      constructors,
      Infer.Infer_proc.Name_map.bindings typed.siblings_env )

  let prelude_modules =
    lazy
      (List.map
         (fun module_ ->
           parsed_module
             ~fallback_name:(Prelude.name module_)
             (Prelude.source module_))
         Prelude.all)

  let module_of (source : File_loader.Files.Elm_file.t) =
    let module_ =
      parsed_module
        ~fallback_name:
          (Filename.remove_extension (Filename.basename source.path))
        source.content
    in
    { module_ with imports = Prelude.default_imports @ module_.imports }

  let resolved_against dependencies (module_ : Canonical.Module.t) =
    match Canonicalization.Resolve_names.in_module ~dependencies module_ with
    | Ok resolved -> resolved
    | Error errors ->
        List.map Canonicalization.Resolve_names.show_error errors
        |> String.concat "\n" |> failwith

  let exported_names (module_ : Canonical.Module.t)
      (typed : Infer.Infer_proc.infer_result) =
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
        (fun acc (c : Infer.Infer_proc.ctor_info) ->
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

  let compile_modules (sources : File_loader.Files.Elm_file.t list) :
      compiled list =
    let compile_module progress module_ =
      let resolved = resolved_against progress.dependencies module_ in
      let imports = imported_interfaces resolved progress.interfaces in
      Infer.Infer_proc.State.reset ();
      let typed =
        Infer.Infer_proc.infer_toplevel ~imports resolved initial_ctx
      in
      let declarations, constructors, siblings = prepared typed in
      let compiled =
        {
          module_name = resolved.name;
          source =
            B.emit_module ~arities:(imported_arities imports) ~constructors
              ~siblings
              ~imports:(providing_modules declarations)
              ~exports:(exported_names resolved typed)
              declarations;
          warnings =
            List.concat_map
              (After_typed.Exhaustiveness_check.warnings typed.siblings_env)
              typed.declarations;
        }
      in
      {
        dependencies = resolved :: progress.dependencies;
        interfaces =
          Infer.Infer_proc.interface_of resolved typed :: progress.interfaces;
        output = compiled :: progress.output;
      }
    in
    match
      Canonicalization.Module_graph.in_dependency_order
        (Lazy.force prelude_modules @ List.map module_of sources)
    with
    | Error error -> failwith (Canonicalization.Module_graph.show_error error)
    | Ok ordered ->
        let runtime =
          match B.runtime_module () with
          | None -> []
          | Some (module_name, source) ->
              [ { module_name; source; warnings = [] } ]
        in
        let finished =
          List.fold_left compile_module
            { dependencies = []; interfaces = []; output = [] }
            ordered
        in
        runtime @ List.rev finished.output

  let compile_source (content : string) : compiled list =
    compile_modules
      [ File_loader.Files.Elm_file.{ path = "Main.elm"; content } ]
end

include Make (Js_backend)
