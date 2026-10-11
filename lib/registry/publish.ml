open Packages

type mode = Publish | Rehearse

type outcome =
  | Published of Manifest.listing
  | Rehearsed of Manifest.listing
  | Npm_failed of int

let run root ~modules mode =
  let manifest =
    match Manifest.of_folder root with
    | Some manifest -> manifest
    | None ->
        Diagnostic.Failure.raise_project
          (Missing_manifest { file = Manifest.file_name })
  in
  let file = manifest.file in
  let listing =
    match manifest.kind with
    | Package listing -> listing
    | Application _ -> Diagnostic.Failure.raise_project (Not_a_package { file })
  in
  Option.iter
    (fun module_name ->
      Diagnostic.Failure.raise_project (Missing_exposed { file; module_name }))
    (List.find_opt
       (fun name -> not (List.mem name modules))
       (Non_empty_list.to_list listing.exposed_modules));
  let staging = Fpath.v ".dartea/publish" in
  let sources = Fpath.v Manifest.default_source_directory in
  let package_json =
    `Assoc
      [
        ("name", `String listing.name);
        ("version", `String (Version.show listing.version));
        ("description", `String listing.summary);
        ("license", `String listing.license);
        ("keywords", `List [ `String "dartea-package" ]);
        ( "dependencies",
          `Assoc
            (List.map
               (fun (name, range) -> (name, `String (Version.Range.show range)))
               manifest.dependencies) );
      ]
  in
  let copy path =
    Files.save root (Fpath.append staging path) (Files.load root path)
  in
  Files.clear root staging;
  Files.save root
    (Fpath.add_seg staging "package.json")
    (Yojson.Safe.pretty_to_string package_json ^ "\n");
  copy Manifest.path;
  List.iter
    (fun path -> copy (Fpath.append sources path))
    (Files.under ~suffix:".elm" (Fpath.append root sources));
  List.iter copy
    (List.filter (Files.is_file root) [ Fpath.v "README.md"; Fpath.v "LICENSE" ]);
  let folder =
    Fpath.normalize
      (Fpath.append (Fpath.v (Sys.getcwd ())) (Fpath.append root staging))
  in
  let rehearsal =
    match mode with Publish -> [] | Rehearse -> [ "--dry-run" ]
  in
  let arguments =
    [ "npm"; "publish"; Fpath.to_string folder; "--access"; "public" ]
    @ rehearsal
  in
  let code =
    match
      Unix.create_process "npm" (Array.of_list arguments) Unix.stdin
        Unix.stdout Unix.stderr
    with
    | pid -> begin
        match Unix.waitpid [] pid with
        | _, Unix.WEXITED code -> code
        | _, (Unix.WSIGNALED _ | Unix.WSTOPPED _) -> 1
      end
    | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
        Diagnostic.Failure.raise_project (No_npm { file })
  in
  match (code, mode) with
  | 0, Publish -> Published listing
  | 0, Rehearse -> Rehearsed listing
  | code, (Publish | Rehearse) -> Npm_failed code
