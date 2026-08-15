let () =
  Eio_main.run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  let path = Eio.Path.(cwd / "playgrounds" / "elm_code") in
  File_loader.Files.current_folder path
  |> Dartea.Compiler.compile_modules
  |> List.iter (fun (module_ : Dartea.Compiler.compiled) ->
         List.iter prerr_endline module_.warnings;
         Printf.printf "\n=== %s ===\n%s\n" module_.module_name module_.source)
