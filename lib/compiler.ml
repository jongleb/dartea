let compile path =
  path |> File_loader.Files.current_folder
  |> List.map (fun content ->
         content |> Parse.Main.parse
         |> Result.map Ast.Kind.Frontend.Module.of_impl)
