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

let compiled ~path sources =
  let outcome = Dartea.Compiler.compile_modules sources in
  let seen = Reporting.Sources.of_list outcome.sources in
  printed (List.map (Reporting.Sources.report seen) outcome.errors);
  List.iter (saved ~path ~seen) outcome.output;
  if outcome.errors <> [] then exit 1

let () =
  Eio_main.run @@ fun env ->
  let folder = if Array.length Sys.argv > 1 then Sys.argv.(1) else "." in
  let path = Eio.Path.(Eio.Stdenv.fs env / folder) in
  match File_loader.Files.load path with
  | Error error ->
      printed [ Reporting.Sources.report Reporting.Sources.empty error ];
      exit 1
  | Ok sources -> compiled ~path sources
