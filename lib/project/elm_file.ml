type origin = Written | Package

type t = { path : string; name : string; content : string; origin : origin }

let extension = ".elm"

let dotted path =
  match List.rev path with
  | [] -> ""
  | last :: earlier ->
      String.concat "." (List.rev (Filename.remove_extension last :: earlier))

let under ~origin ~directory ~path content =
  {
    path = Files.Relative.shown (Files.Relative.inside directory path);
    name = dotted path;
    content;
    origin;
  }

let written source =
  match source.origin with Written -> Some source.name | Package -> None

let of_path ~path content =
  under ~origin:Written ~directory:Files.Relative.root
    ~path:(Files.Relative.of_string path)
    content
