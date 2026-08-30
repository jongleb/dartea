type file = { path : string; content : string }

let licence_file = "dartea.LICENSE.txt"
let licence path = { path; content = Licence_text.notice }

let carries_derived (modules : Compiler.artifact list) =
  List.exists
    (fun (module_ : Compiler.artifact) ->
      Option.fold ~none:false ~some:Prelude.is_derived (Prelude.of_name module_.module_name))
    modules

let licence_files ~at modules = if carries_derived modules then [ licence at ] else []

module type S = sig
  val name : string
  val roots : Compiler.outcome -> Data.Name.t list

  val files :
    entry:Entry.t option ->
    output:string ->
    Compiler.artifact list ->
    file list
end

let beside output = Filename.concat (Filename.dirname output) licence_file

module Esm_folder : S = struct
  let name = "esm_folder"

  let roots (outcome : Compiler.outcome) =
    List.concat_map
      (fun (module_ : Compiler.linkable) ->
        if List.mem module_.module_name outcome.written then
          List.map
            (fun exported ->
              Data.Name.global ~module_name:module_.module_name
                ~exported_name:(Data.Name.base exported))
            module_.exports
        else [])
      outcome.modules

  let files ~entry:_ ~output modules =
    let inside path = Filename.concat output path in
    licence_files ~at:(inside licence_file) modules
    @ List.map
         (fun (module_ : Compiler.artifact) ->
           {
             path =
               inside
                 (Codegen_js.Of_optimized.module_file module_.module_name);
             content = module_.source;
           })
         modules
end

module Browser_program = struct
  let program_type = "Platform.Program"

  let program =
    Data.Name.global ~module_name:"Platform" ~exported_name:"Program"

  let require_entry ~name = function
    | None ->
        Diagnostic.Failure.raise_project (Delivery_needs_entry { delivery = name })
    | Some (entry : Entry.t) -> entry

  let roots ~name (outcome : Compiler.outcome) =
    [ Compiler.entry_root (require_entry ~name outcome.entry) ]

  let exposes (entry : Entry.t) (module_ : Compiler.artifact) =
    String.equal module_.module_name entry.module_name
    && List.mem entry.declaration module_.exports

  let check_exposure ~name (entry : Entry.t) modules =
    if not (List.exists (exposes entry) modules) then
      Diagnostic.Failure.raise_project_at ~region:entry.region
        (Entry_not_exposed
           {
             delivery = name;
             module_name = entry.module_name;
             declaration = entry.declaration;
           })

  let showable ~name (entry : Entry.t) =
    match Typed.Type.head entry.typ with
    | Typed.Type.TCustom (found, _) when Data.Name.equal found program -> ()
    | found ->
        Diagnostic.Failure.raise_project_at ~region:entry.region
          (Bad_entry
             {
               delivery = name;
               module_name = entry.module_name;
               declaration = entry.declaration;
               expected = program_type;
               found = Reporting.Message.of_type found;
             })

  let bundle modules =
    List.map
      (fun (module_ : Compiler.artifact) ->
        {
          Codegen_js.Bundle.name = module_.module_name;
          source = module_.source;
        })
      modules

  let flags_type (entry : Entry.t) =
    match Typed.Type.head entry.typ with
    | Typed.Type.TCustom (_, taken :: _) -> taken
    | _ -> Typed.Type.TUnit

  let flags ~name (entry : Entry.t) =
    match Codegen_js.Flags.decoder (flags_type entry) with
    | Ok written -> written
    | Error found ->
        Diagnostic.Failure.raise_project_at ~region:entry.region
          (Bad_flags
             {
               delivery = name;
               module_name = entry.module_name;
               declaration = entry.declaration;
               found;
             })

  let script ~name entry modules =
    check_exposure ~name entry modules;
    showable ~name entry;
    Codegen_js.Bundle.of_modules ~entry_module:entry.module_name
      ~declaration:entry.declaration ~flags:(flags ~name entry)
      (bundle modules)

end

module Script : S = struct
  let name = "script"
  let roots outcome = Browser_program.roots ~name outcome

  let banner =
    "/*! Licences of the runtime and standard library compiled in: see "
    ^ licence_file ^ " */\n"

  let files ~entry ~output modules =
    let entry = Browser_program.require_entry ~name entry in
    let heading = if carries_derived modules then banner else "" in
    licence_files ~at:(beside output) modules
    @ [ { path = output; content = heading ^ Browser_program.script ~name entry modules } ]
end

module Sandwich : S = struct
  let name = "page"
  let roots outcome = Browser_program.roots ~name outcome

  let files ~entry ~output modules =
    let entry = Browser_program.require_entry ~name entry in
    [
      {
        path = output;
        content =
          Codegen_js.Bundle.sandwich ~title:entry.module_name
            ~entry_module:entry.module_name
            ~notice:(if carries_derived modules then Some Licence_text.notice else None)
            (Browser_program.script ~name entry modules);
      };
    ]
end

let default : (module S) = (module Esm_folder)

let for_output output =
  if Filename.check_suffix output ".js" then (module Script : S)
  else if Filename.check_suffix output ".html" then (module Sandwich : S)
  else default

let produce ~delivery ~output (outcome : Compiler.outcome) =
  let module Delivery = (val delivery : S) in
  Delivery.files ~entry:outcome.entry ~output
    (Compiler.link ~roots:(Delivery.roots outcome) outcome)
