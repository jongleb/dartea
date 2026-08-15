let initial_ctx =
  let f acc (v, scheme) =
    Infer.Infer_proc.Name_map.add (Data.Name.local v) scheme acc
  in
  List.fold_left f Infer.Infer_proc.Name_map.empty Builtins.values

module type BACKEND = sig
  val emit :
    constructors:(Data.Name.t * int) list ->
    siblings:(Data.Name.t * (Data.Name.t * int) list) list ->
    Optimized.Declaration.t list ->
    string
end

module Js_backend : BACKEND = struct
  let emit ~constructors ~siblings decls =
    Codegen.Js_of_optimized.emit ~constructors ~siblings decls
end

type compiled = {
  module_name : string;
  source : string;
  warnings : string list;
}

module Make (B : BACKEND) = struct
  type progress = {
    dependencies : Canonical.Module.t list;
    interfaces : Interface.t list;
    output : compiled list;
  }

  let emitted (typed : Infer.Infer_proc.infer_result) =
    let declarations =
      After_typed.Optimize.optimize typed.declarations
      |> After_typed.Dependency_sort.sort_declarations
    in
    let constructors =
      List.map
        (fun (c : Infer.Infer_proc.ctor_info) -> (c.name, c.arity))
        typed.constructors
    in
    let siblings = Infer.Infer_proc.Name_map.bindings typed.siblings_env in
    B.emit ~constructors ~siblings declarations

  let compile_string (content : string) : string =
    match Parse.Main.parse content with
    | Error e -> raise e
    | Ok impl_list ->
        let canonical =
          Canonical.Module.of_frontend ~fallback_name:"Main"
            (Frontend.Module.of_impl impl_list)
        in
        Infer.Infer_proc.State.reset ();
        emitted
          (Infer.Infer_proc.infer_toplevel ~imports:[] canonical initial_ctx)

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
      {
        dependencies = resolved :: progress.dependencies;
        interfaces =
          Infer.Infer_proc.interface_of resolved typed :: progress.interfaces;
        output =
          {
            module_name = resolved.name;
            source = emitted typed;
            warnings =
              List.concat_map
                (After_typed.Exhaustiveness_check.warnings typed.siblings_env)
                typed.declarations;
          }
          :: progress.output;
      }
    in
    match
      Canonicalization.Module_graph.in_dependency_order
        (List.map module_of sources)
    with
    | Error error -> failwith (Canonicalization.Module_graph.show_error error)
    | Ok ordered ->
        let finished =
          List.fold_left compile_module
            { dependencies = []; interfaces = []; output = [] }
            ordered
        in
        List.rev finished.output
end

include Make (Js_backend)
