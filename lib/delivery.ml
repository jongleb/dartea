type file = { path : string; content : string }

let licence_file = "dartea.LICENSE.txt"
let licence path = { path; content = Licence_text.licence }

module type S = sig
  val name : string
  val roots : Compiler.outcome -> Data.Name.t list
  val files : entry:Entry.t option -> Compiler.compiled list -> file list
end

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

  let files ~entry:_ modules =
    licence licence_file
    :: List.map
         (fun (module_ : Compiler.compiled) ->
           {
             path = Codegen_js.Of_optimized.module_file module_.module_name;
             content = module_.source;
           })
         modules
end

module Classic_js_browser : S = struct
  let name = "classic_js_browser"
  let folder = "build"
  let handled = "Platform.Program"

  let program =
    Data.Name.global ~module_name:"Platform" ~exported_name:"Program"

  let wanted = function
    | None ->
        Reporting.Error.raise_project (Delivery_needs_entry { delivery = name })
    | Some (entry : Entry.t) -> entry

  let roots (outcome : Compiler.outcome) =
    [ Compiler.entry_root (wanted outcome.entry) ]

  let exposes (entry : Entry.t) (module_ : Compiler.compiled) =
    String.equal module_.module_name entry.module_name
    && List.mem entry.declaration module_.exports

  let exposed (entry : Entry.t) modules =
    if not (List.exists (exposes entry) modules) then
      Reporting.Error.raise_project_at ~region:entry.region
        (Entry_not_exposed
           {
             delivery = name;
             module_name = entry.module_name;
             declaration = entry.declaration;
           })

  let showable (entry : Entry.t) =
    match Typed.Type.head entry.typ with
    | Typed.Type.TCustom (name, _) when Data.Name.equal name program -> ()
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

  let built path content = { path = Filename.concat folder path; content }

  let files ~entry modules =
    let entry = wanted entry in
    exposed entry modules;
    showable entry;
    [
      built licence_file Licence_text.licence;
      built Codegen_js.Bundle.entry_file
        (Codegen_js.Bundle.of_modules ~entry_module:entry.module_name
           ~declaration:entry.declaration (bundled modules));
      built Codegen_js.Bundle.page_file
        (Codegen_js.Bundle.page ~title:entry.module_name);
    ]
end

let all : (module S) list = [ (module Esm_folder); (module Classic_js_browser) ]
let names = List.map (fun (module D : S) -> D.name) all
let default : (module S) = (module Esm_folder)

let find name =
  match List.find_opt (fun (module D : S) -> String.equal D.name name) all with
  | Some found -> found
  | None ->
      Reporting.Error.raise_project (Unknown_delivery { name; known = names })

let produced ~delivery (outcome : Compiler.outcome) =
  let module Delivery = (val delivery : S) in
  Delivery.files ~entry:outcome.entry
    (Compiler.link ~roots:(Delivery.roots outcome) outcome)
