type t = { file : string; packages : Pick.t list }

let file_name = "dartea.lock"
let path = Fpath.v file_name
let field = "packages"
let packages = Json.pairs "an object of packages and versions" Json.version

let decode ~file content =
  let rows = Json.rows_of ~file content in
  {
    file;
    packages = List.map Pick.of_pair (Json.require ~file rows field packages);
  }

let of_folder root = Json.load root path decode

let shown packages =
  let row (pick : Pick.t) =
    (pick.package, `String (Version.show pick.version))
  in
  let in_order = List.sort Pick.by_name packages in
  let rows = `Assoc [ (field, `Assoc (List.map row in_order)) ] in
  Yojson.Safe.pretty_to_string rows ^ "\n"

let save root packages = Files.save root path (shown packages)
