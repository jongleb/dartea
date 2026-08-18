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

let () =
  Eio_main.run @@ fun env ->
  let path =
    if Array.length Sys.argv > 1 then Eio.Path.(Eio.Stdenv.fs env / Sys.argv.(1))
    else Eio.Path.(Eio.Stdenv.fs env / ".")
  in
  let outcome =
    File_loader.Files.current_folder path |> Dartea.Compiler.compile_modules
  in
  let sources = Reporting.Sources.of_list outcome.sources in
  printed
    (List.map (Reporting.Sources.report sources) outcome.errors);
  List.iter
    (fun (module_ : Dartea.Compiler.compiled) ->
      printed
        (List.map (Reporting.Sources.warning sources) module_.warnings);
      let file_name = module_.module_name ^ "." ^ Dartea.Compiler.extension in
      Eio.Path.save
        ~create:(`Or_truncate 0o644)
        Eio.Path.(path / file_name)
        module_.source)
    outcome.output;
  if outcome.errors <> [] then exit 1
