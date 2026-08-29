open Packages

exception Broken of { package : string; version : string; problem : string }

let sha512 = "sha512-"
let root_folder = "package"
let upward = ".."

let refuse (pick : Pick.t) problem =
  raise
    (Broken
       { package = pick.package; version = Version.show pick.version; problem })

let digest content =
  Base64.encode_string Digestif.SHA512.(to_raw_string (digest_string content))

let verify pick ~integrity content =
  if not (String.starts_with ~prefix:sha512 integrity) then
    refuse pick "the registry gave me no sha512 checksum for this tarball"
  else
    let width = String.length sha512 in
    let checksum = String.sub integrity width (String.length integrity - width) in
    if not (String.equal checksum (digest content)) then
      refuse pick "the tarball does not match the checksum in the registry"

let held ?global:_ header entries =
  let size = header.Tar.Header.file_size in
  match header.Tar.Header.link_indicator with
  | Tar.Header.Link.Normal ->
      Tar.bind (Tar.really_read size) (fun content ->
          Tar.return (Ok ((header.Tar.Header.file_name, content) :: entries)))
  | _ -> Tar.bind (Tar.seek size) (fun () -> Tar.return (Ok entries))

let unpack pick native =
  let handle = Unix.openfile native [ Unix.O_RDONLY ] 0 in
  let outcome = Tar_unix.run (Tar_gz.in_gzipped (Tar.fold held [])) handle in
  Unix.close handle;
  match outcome with
  | Ok entries -> List.rev entries
  | Error _ -> refuse pick "I could not unpack the tarball"

let strip pick name =
  match Fpath.segs (Fpath.v name) with
  | head :: rest when String.equal head root_folder -> rest
  | _ -> refuse pick ("the tarball holds " ^ name ^ " outside its own folder")

let safe pick ~name segments =
  if List.exists (String.equal upward) segments then
    refuse pick ("the tarball tries to write outside itself: " ^ name)
  else segments

let place root pick (name, content) =
  match safe pick ~name (strip pick name) with
  | [] -> ()
  | first :: rest ->
      let path = List.fold_left Fpath.add_seg (Fpath.v first) rest in
      Files.save root (Fpath.append (Pick.folder pick) path) content

let cache root pick content =
  let path = Pick.tarball pick in
  Files.save root path content;
  Files.at root path

let kept root pick ~integrity content =
  verify pick ~integrity content;
  let native = cache root pick content in
  List.iter (place root pick) (unpack pick native)
