type t = { path : string; name : string; content : string }

let extension = ".elm"

let dotted path =
  match List.rev path with
  | [] -> ""
  | last :: earlier ->
      String.concat "." (List.rev (Filename.remove_extension last :: earlier))

let under ~directory ~path content =
  {
    path = Files.Relative.shown (Files.Relative.inside directory path);
    name = dotted path;
    content;
  }

let of_path ~path content =
  under ~directory:Files.Relative.root
    ~path:(Files.Relative.of_string path)
    content
