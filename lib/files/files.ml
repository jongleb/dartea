let at root path = Fpath.to_string (Fpath.normalize (Fpath.append root path))
let shown root = Fpath.to_string root
let or_fail outcome = Rresult.R.failwith_error_msg outcome

let is_directory root path =
  Result.value ~default:false (Bos.OS.Dir.exists (Fpath.append root path))

let is_file root path =
  Result.value ~default:false (Bos.OS.File.exists (Fpath.append root path))

let load root path = or_fail (Bos.OS.File.read (Fpath.append root path))

let save root path content =
  let target = Fpath.append root path in
  ignore (or_fail (Bos.OS.Dir.create ~path:true (Fpath.parent target)));
  or_fail (Bos.OS.File.write target content)

let under ~suffix root =
  let inside = Fpath.to_dir_path root in
  let note path found =
    if Fpath.has_ext suffix path then
      Option.to_list (Fpath.rem_prefix inside path) @ found
    else found
  in
  or_fail (Bos.OS.Dir.fold_contents ~elements:`Files note [] inside)
  |> List.sort Fpath.compare
