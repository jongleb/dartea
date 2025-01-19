let () =
  let result = File_loader.Files.current_folder "playgrounds/elm_code" in
  let parsed =
    List.map
      (fun i -> Parse.Main.parse i.File_loader.Files.Elm_file.content)
      result
  in
  let canonicalized =
    List.map
      (fun x -> Result.map (List.map Canonical.Impl.of_frontend) x)
      parsed
  in
  let typed =
    List.map
      (fun x ->
        Result.map
          (List.map (fun x ->
               Typed.Infer.infer
                 (match x with Canonical.Impl.Top_declaration x -> x.expr)))
          x)
      canonicalized
  in
  List.iter (fun x -> match x with Ok _ -> () | Error x -> raise x) typed;
  prerr_endline "Success!"
