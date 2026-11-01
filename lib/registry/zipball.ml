type t = Placed | Kernel

let kept root pick ~hash content =
  let readable what =
    Result.fold ~ok:Fun.id ~error:(fun _ ->
        Store.refuse pick ("I could not unpack " ^ what))
  in
  let inside name =
    match String.split_on_char '/' name with
    | _github_folder :: (_ :: _ as path) -> Store.safe pick ~name path
    | [ _ ] | [] ->
        Store.refuse pick ("the zip holds " ^ name ^ " outside its own folder")
  in
  let unpacked member files =
    match Zipc.Member.kind member with
    | Zipc.Member.Dir -> files
    | Zipc.Member.File file ->
        let name = Zipc.Member.path member in
        (inside name, readable name (Zipc.File.to_binary_string file)) :: files
  in
  let for_elm_runtime = function
    | "src" :: _ :: _, source ->
        String.starts_with ~prefix:"effect module " source
        || List.exists
             (String.starts_with ~prefix:"import Elm.Kernel.")
             (String.split_on_char '\n' source)
    | _ -> false
  in
  let outline (path, _) = List.equal String.equal path [ "elm.json" ] in
  if
    not
      (String.equal
         (String.lowercase_ascii hash)
         Digestif.SHA1.(to_hex (digest_string content)))
  then Store.refuse pick "the zip does not match the hash in the Elm registry";
  let files =
    Zipc.fold unpacked (readable "the zip" (Zipc.of_binary_string content)) []
  in
  if not (List.exists outline files) then
    Store.refuse pick "this is not an Elm package, it has no elm.json";
  if List.exists for_elm_runtime files then Kernel
  else (
    List.iter (Store.place root pick) files;
    Placed)
