type origin = Written | Package

type t = { path : string; name : string; content : string; origin : origin }

let extension = ".elm"

let module_name path =
  String.concat "." (Fpath.segs (Fpath.rem_ext (Fpath.normalize path)))

let under ~origin ~directory ~path content =
  {
    path = Fpath.to_string (Fpath.normalize (Fpath.append directory path));
    name = module_name path;
    content;
    origin;
  }

let written source =
  match source.origin with Written -> Some source.name | Package -> None

let of_path ~path content =
  under ~origin:Written
    ~directory:(Fpath.v Filename.current_dir_name)
    ~path:(Fpath.v path) content
