type t = { file : string; packages : Pick.t list }

let file_name = "dartea.lock"
let path = Fpath.v file_name
let field = "packages"
let packages = Json.pairs "an object of packages and versions" Json.version

let decoded ~file content =
  let rows = Json.rows_of ~file content in
  {
    file;
    packages = List.map Pick.of_pair (Json.required ~file rows field packages);
  }

let of_folder root = Json.loaded root path decoded

let shown packages =
  let row (pick : Pick.t) =
    (pick.package, `String (Version.show pick.version))
  in
  let sorted = List.sort Pick.by_name packages in
  let rows = `Assoc [ (field, `Assoc (List.map row sorted)) ] in
  Yojson.Safe.pretty_to_string rows ^ "\n"

let saved root packages = Files.saved root path (shown packages)
