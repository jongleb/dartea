type file = { path : string; content : string }

let licence_file = "dartea.LICENSE.txt"
let licence path = { path; content = Licence_text.licence }

module type S = sig
  val name : string
  val roots : Compiler.outcome -> Data.Name.t list

  val files :
    entry:Entry.t option ->
    output:string ->
    Compiler.compiled list ->
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
    licence (inside licence_file)
    :: List.map
         (fun (module_ : Compiler.compiled) ->
           {
             path =
               inside
                 (Codegen_js.Of_optimized.module_file module_.module_name);
             content = module_.source;
           })
         modules
end

module Browser_program = struct
  let handled = "Platform.Program"

  let program =
    Data.Name.global ~module_name:"Platform" ~exported_name:"Program"

  let wanted ~name = function
    | None ->
        Reporting.Error.raise_project (Delivery_needs_entry { delivery = name })
    | Some (entry : Entry.t) -> entry

  let roots ~name (outcome : Compiler.outcome) =
    [ Compiler.entry_root (wanted ~name outcome.entry) ]

  let exposes (entry : Entry.t) (module_ : Compiler.compiled) =
    String.equal module_.module_name entry.module_name
    && List.mem entry.declaration module_.exports

  let exposed ~name (entry : Entry.t) modules =
    if not (List.exists (exposes entry) modules) then
      Reporting.Error.raise_project_at ~region:entry.region
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
        Reporting.Error.raise_project_at ~region:entry.region
          (Bad_entry
             {
               delivery = name;
               module_name = entry.module_name;
               declaration = entry.declaration;
               expected = handled;
               found = Reporting.Message.of_type found;
             })

  let bundled modules =
    List.map
      (fun (module_ : Compiler.compiled) ->
        {
          Codegen_js.Bundle.name = module_.module_name;
          source = module_.source;
        })
      modules

  let carried (entry : Entry.t) =
    match Typed.Type.head entry.typ with
    | Typed.Type.TCustom (_, taken :: _) -> taken
    | _ -> Typed.Type.TUnit

  let flags ~name (entry : Entry.t) =
    match Codegen_js.Flags.decoder (carried entry) with
    | Ok written -> written
    | Error found ->
        Reporting.Error.raise_project_at ~region:entry.region
          (Bad_flags
             {
               delivery = name;
               module_name = entry.module_name;
               declaration = entry.declaration;
               found;
             })

  let script ~name entry modules =
    exposed ~name entry modules;
    showable ~name entry;
    Codegen_js.Bundle.of_modules ~entry_module:entry.module_name
      ~declaration:entry.declaration ~flags:(flags ~name entry)
      (bundled modules)

  let files ~name ~output ~wrapped entry modules =
    let found = wanted ~name entry in
    [
      licence (beside output);
      { path = output; content = wrapped found (script ~name found modules) };
    ]
end

module Script : S = struct
  let name = "script"
  let roots outcome = Browser_program.roots ~name outcome
  let bare _ written = written

  let files ~entry ~output modules =
    Browser_program.files ~name ~output ~wrapped:bare entry modules
end

module Sandwich : S = struct
  let name = "page"
  let roots outcome = Browser_program.roots ~name outcome

  let page (entry : Entry.t) written =
    Codegen_js.Bundle.sandwich ~title:entry.module_name
      ~entry_module:entry.module_name written

  let files ~entry ~output modules =
    Browser_program.files ~name ~output ~wrapped:page entry modules
end

let default : (module S) = (module Esm_folder)

let for_output output =
  if Filename.check_suffix output ".js" then (module Script : S)
  else if Filename.check_suffix output ".html" then (module Sandwich : S)
  else default

let produced ~delivery ~output (outcome : Compiler.outcome) =
  let module Delivery = (val delivery : S) in
  Delivery.files ~entry:outcome.entry ~output
    (Compiler.link ~roots:(Delivery.roots outcome) outcome)
