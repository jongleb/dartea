let colours_wanted () =
  match Sys.getenv_opt "NO_COLOR" with
  | Some _ -> false
  | None -> Unix.isatty Unix.stderr

let printed reports =
  let colours = colours_wanted () in
  List.iter
    (fun report ->
      prerr_endline (Reporting.Report.to_string ~colours report))
    reports

let saved ~path ~seen (module_ : Dartea.Compiler.compiled) =
  printed (List.map (Reporting.Sources.warning seen) module_.warnings);
  let file_name = module_.module_name ^ "." ^ Dartea.Compiler.extension in
  Eio.Path.save
    ~create:(`Or_truncate 0o644)
    Eio.Path.(path / file_name)
    module_.source

let compiled ~path ?entry sources =
  let outcome = Dartea.Compiler.compile_modules ?entry sources in
  let seen = Reporting.Sources.of_list outcome.sources in
  printed (List.map (Reporting.Sources.report seen) outcome.errors);
  List.iter (saved ~path ~seen) outcome.output;
  if outcome.errors <> [] then exit 1

let refused error =
  printed [ Reporting.Sources.report Reporting.Sources.empty error ];
  exit 1

let entry_of sources path =
  match
    List.find_opt
      (fun (source : File_loader.Files.Elm_file.t) ->
        String.equal source.path path)
      sources
  with
  | Some source -> source.name
  | None -> refused (Reporting.Error.project (Unknown_entry { path }))

let asked () = if Array.length Sys.argv > 1 then Sys.argv.(1) else "."

let () =
  Eio_main.run @@ fun env ->
  let wanted = asked () in
  let folder, entry_path =
    if String.ends_with ~suffix:File_loader.Files.extension wanted then
      (Filename.current_dir_name, Some wanted)
    else (wanted, None)
  in
  let path = Eio.Path.(Eio.Stdenv.fs env / folder) in
  match File_loader.Files.load path with
  | Error error -> refused error
  | Ok sources ->
      let entry = Option.map (entry_of sources) entry_path in
      compiled ~path ?entry sources
