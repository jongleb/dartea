let at root path = Fpath.to_string (Fpath.normalize (Fpath.append root path))
let shown root = Fpath.to_string root
let checked outcome = Rresult.R.failwith_error_msg outcome

let is_directory root path =
  Result.value ~default:false (Bos.OS.Dir.exists (Fpath.append root path))

let is_file root path =
  Result.value ~default:false (Bos.OS.File.exists (Fpath.append root path))

let load root path = checked (Bos.OS.File.read (Fpath.append root path))

let saved root path content =
  let placed = Fpath.append root path in
  ignore (checked (Bos.OS.Dir.create ~path:true (Fpath.parent placed)));
  checked (Bos.OS.File.write placed content)

let under ~suffix root =
  let inside = Fpath.to_dir_path root in
  let noted path found =
    if Fpath.has_ext suffix path then
      Option.to_list (Fpath.rem_prefix inside path) @ found
    else found
  in
  checked (Bos.OS.Dir.fold_contents ~elements:`Files noted [] inside)
  |> List.sort Fpath.compare
