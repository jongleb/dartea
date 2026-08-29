type t = { package : string; version : Version.t }

let of_pair (package, version) = { package; version }
let by_name one other = String.compare one.package other.package
let shown pick = pick.package ^ " " ^ Version.show pick.version
let path pick = pick.package ^ "/" ^ Version.show pick.version

let found package picks =
  List.find_opt (fun pick -> String.equal pick.package package) picks
