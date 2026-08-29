type t = { package : string; version : Version.t }

let of_pair (package, version) = { package; version }
let by_name one other = String.compare one.package other.package
let shown pick = pick.package ^ " " ^ Version.show pick.version
let path pick = Fpath.v (pick.package ^ "/" ^ Version.show pick.version)

let found package picks =
  List.find_opt (fun pick -> String.equal pick.package package) picks

let vendor_folder = Fpath.v ".dartea/packages"
let cache_folder = Fpath.v ".dartea/cache"
let folder pick = Fpath.append vendor_folder (path pick)
let sources pick = Fpath.add_seg (folder pick) "src"
let tarball pick = Fpath.add_ext ".tgz" (Fpath.append cache_folder (path pick))
