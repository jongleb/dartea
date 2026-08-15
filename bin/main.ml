let () =
  Eio_main.run @@ fun env ->
  let path =
    if Array.length Sys.argv > 1 then
      Eio.Path.(Eio.Stdenv.fs env / Sys.argv.(1))
    else Eio.Path.(Eio.Stdenv.fs env / ".")
  in
  File_loader.Files.current_folder path
  |> Dartea.Compiler.compile_modules
  |> List.iter (fun (module_ : Dartea.Compiler.compiled) ->
         List.iter prerr_endline module_.warnings;
         Printf.printf "\n=== %s ===\n%s\n" module_.module_name module_.source)
