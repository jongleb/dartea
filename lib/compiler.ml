let initial_ctx =
  let f acc (v, scheme) =
    Infer.Infer_proc.Name_map.add (Data.Name.local v) scheme acc
  in
  List.fold_left f Infer.Infer_proc.Name_map.empty Builtins.values

module type BACKEND = sig
  val extension : string
  val runtime_module : unit -> (string * string) option

  val emit_standalone :
    constructors:(Data.Name.t * int) list ->
    siblings:(Data.Name.t * (Data.Name.t * int) list) list ->
    Optimized.Declaration.t list ->
    string

  val emit_module :
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
        Codegen.Js_of_optimized.runtime_source () )

  let emit_standalone ~constructors ~siblings decls =
    Codegen.Js_of_optimized.emit_standalone ~constructors ~siblings decls

  let emit_module ~constructors ~siblings ~imports ~exports decls =
    Codegen.Js_of_optimized.emit_module ~constructors ~siblings ~imports
      ~exports decls
end

type compiled = {
  module_name : string;
  source : string;
  warnings : string list;
}

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

  let compile_string (content : string) : string =
    match Parse.Main.parse content with
    | Error e -> raise e
    | Ok impl_list ->
        let canonical =
          Canonical.Module.of_frontend ~fallback_name:"Main"
            (Frontend.Module.of_impl impl_list)
        in
        Infer.Infer_proc.State.reset ();
        let declarations, constructors, siblings =
          prepared
            (Infer.Infer_proc.infer_toplevel ~imports:[] canonical initial_ctx)
        in
        B.emit_standalone ~constructors ~siblings declarations

  let module_of (source : File_loader.Files.Elm_file.t) =
    match Parse.Main.parse source.content with
    | Error e -> raise e
    | Ok impl_list ->
        Canonical.Module.of_frontend
          ~fallback_name:
            (Filename.remove_extension (Filename.basename source.path))
          (Frontend.Module.of_impl impl_list)

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
            B.emit_module ~constructors ~siblings
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
        (List.map module_of sources)
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
end

include Make (Js_backend)
