open Packages

exception Broken of { package : string; version : string; problem : string }

let sha512 = "sha512-"
let root_folder = "package"
let upward = ".."

let refuse (pick : Pick.t) problem =
  raise
    (Broken
       { package = pick.package; version = Version.show pick.version; problem })

let digested content =
  Base64.encode_string Digestif.SHA512.(to_raw_string (digest_string content))

let after prefix written =
  String.sub written (String.length prefix)
    (String.length written - String.length prefix)

let matching pick ~integrity content =
  if String.equal (after sha512 integrity) (digested content) then content
  else refuse pick "the tarball does not match the checksum in the registry"

let verified pick ~integrity content =
  if String.starts_with ~prefix:sha512 integrity then
    matching pick ~integrity content
  else refuse pick "the registry gave me no sha512 checksum for this tarball"

let held ?global:_ header entries =
  let size = header.Tar.Header.file_size in
  match header.Tar.Header.link_indicator with
  | Tar.Header.Link.Normal ->
      Tar.bind (Tar.really_read size) (fun content ->
          Tar.return (Ok ((header.Tar.Header.file_name, content) :: entries)))
  | _ -> Tar.bind (Tar.seek size) (fun () -> Tar.return (Ok entries))

let unpacked pick native =
  let handle = Unix.openfile native [ Unix.O_RDONLY ] 0 in
  let outcome = Tar_unix.run (Tar_gz.in_gzipped (Tar.fold held [])) handle in
  Unix.close handle;
  match outcome with
  | Ok entries -> List.rev entries
  | Error _ -> refuse pick "I could not unpack the tarball"

let stripped pick name =
  match Files.Relative.of_string name with
  | head :: rest when String.equal head root_folder -> rest
  | _ -> refuse pick ("the tarball holds " ^ name ^ " outside its own folder")

let safe pick path =
  if List.exists (String.equal upward) path then
    refuse pick
      ("the tarball tries to write outside itself: "
     ^ Files.Relative.shown path)
  else path

let placed root pick (name, content) =
  match safe pick (stripped pick name) with
  | [] -> ()
  | path ->
      Files.Dir.saved root (Files.Relative.inside (Vendor.at pick) path) content

let cached root pick content =
  let path = Vendor.tarball pick in
  Files.Dir.saved root path content;
  Files.Dir.shown (Files.Dir.at root path)

let kept root pick ~integrity content =
  let native = cached root pick (verified pick ~integrity content) in
  List.iter (placed root pick) (unpacked pick native)
