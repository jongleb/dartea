let () =
  let path =
    Fpath.v (Filename.concat "playgrounds" "elm_code")
  in
  let outcome =
    match Project.Sources.load ~provided:Prelude.packages path with
    | Ok sources -> Dartea.Compiler.compile_modules ~entry:None sources
    | Error failure ->
        prerr_endline
          (Reporting.Report.to_string ~colours:false
             (Reporting.Sources.report Reporting.Sources.empty
                (Reporting.Error.of_failure failure)));
        exit 1
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
    (Dartea.Compiler.link
       ~roots:(Dartea.Compiler.everything outcome)
       outcome)
