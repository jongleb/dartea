let () =
  Eio_main.run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  let path = Eio.Path.(cwd / "playgrounds" / "elm_code") in
  let outcome =
    File_loader.Files.current_folder path |> Dartea.Compiler.compile_modules
  in
  let sources = Reporting.Sources.of_list outcome.sources in
  List.iter
    (fun error ->
      prerr_endline
        (Reporting.Report.to_string ~colours:false
           (Reporting.Sources.report sources error)))
    outcome.errors;
  List.iter
    (fun (module_ : Dartea.Compiler.compiled) ->
      List.iter
        (fun found ->
          prerr_endline
            (Reporting.Report.to_string ~colours:false
               (Reporting.Sources.warning sources found)))
        module_.warnings;
      Printf.printf "\n=== %s ===\n%s\n" module_.module_name module_.source)
    outcome.output
