open OUnit2
open Packages

let version written = Option.get (Version.of_string written)

let content written =
  let sent = ref false in
  fun () ->
    if !sent then Tar.return (Ok None)
    else (
      sent := true;
      Tar.return (Ok (Some written)))

let entries files =
  let left = ref files in
  fun () ->
    match !left with
    | [] -> Tar.return (Ok None)
    | (name, written) :: rest ->
        left := rest;
        let header =
          Tar.Header.make name (Int64.of_int (String.length written))
        in
        Tar.return (Ok (Some (None, header, content written)))

let packed files =
  let path = Filename.temp_file "dartea" ".tgz" in
  let handle = Unix.openfile path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o644 in
  let archive =
    Tar_gz.out_gzipped ~level:4 ~mtime:0l Gz.Unix (Tar.out (entries files))
  in
  let outcome = Tar_unix.run archive handle in
  Unix.close handle;
  match outcome with
  | Ok () ->
      let channel = open_in_bin path in
      let written = really_input_string channel (in_channel_length channel) in
      close_in channel;
      Sys.remove path;
      written
  | Error _ -> assert_failure "the fixture tarball was not written"

let checksum written =
  "sha512-"
  ^ Base64.encode_string Digestif.SHA512.(to_raw_string (digest_string written))

let pick = Pick.of_pair ("@dartea/ui", version "1.0.0")

let unpacked ~integrity files =
  let folder = Sample.folder () in
  Registry.Store.kept (Files.Dir.of_string folder) pick ~integrity
    (packed files);
  folder

let inside folder path =
  let channel = open_in_bin (Filename.concat folder path) in
  let written = really_input_string channel (in_channel_length channel) in
  close_in channel;
  written

let refused ~integrity files =
  match unpacked ~integrity files with
  | _ -> assert_failure "the tarball was accepted"
  | exception Registry.Store.Broken { problem; _ } -> problem

let test_a_tarball_lands_without_its_own_folder _ =
  let files =
    [ ("package/dartea.json", "{}"); ("package/src/Ui.elm", "module Ui\n") ]
  in
  let folder = unpacked ~integrity:(checksum (packed files)) files in
  assert_equal ~printer:Fun.id "{}"
    (inside folder ".dartea/packages/@dartea/ui/1.0.0/dartea.json");
  assert_equal ~printer:Fun.id "module Ui\n"
    (inside folder ".dartea/packages/@dartea/ui/1.0.0/src/Ui.elm")

let test_the_tarball_is_kept _ =
  let files = [ ("package/dartea.json", "{}") ] in
  let folder = unpacked ~integrity:(checksum (packed files)) files in
  assert_bool "the tarball was not cached"
    (Sys.file_exists
       (Filename.concat folder ".dartea/cache/@dartea/ui/1.0.0.tgz"))

let test_a_wrong_checksum_is_refused _ =
  let files = [ ("package/dartea.json", "{}") ] in
  let problem = refused ~integrity:(checksum "something else") files in
  assert_bool problem
    (Node_runner.contains ~needle:"does not match the checksum" problem)

let test_a_checksum_we_cannot_read_is_refused _ =
  let files = [ ("package/dartea.json", "{}") ] in
  let problem = refused ~integrity:"sha1-abcdef" files in
  assert_bool problem (Node_runner.contains ~needle:"no sha512" problem)

let test_a_tarball_cannot_write_outside_itself _ =
  let files = [ ("package/../../../away.elm", "gone\n") ] in
  let problem = refused ~integrity:(checksum (packed files)) files in
  assert_bool problem
    (Node_runner.contains ~needle:"outside itself" problem)

let test_a_tarball_must_hold_one_folder _ =
  let files = [ ("elsewhere/dartea.json", "{}") ] in
  let problem = refused ~integrity:(checksum (packed files)) files in
  assert_bool problem
    (Node_runner.contains ~needle:"outside its own folder" problem)

let packument written =
  Printf.sprintf {|{ "versions": {%s} }|} (String.concat ", " written)

let release at needs =
  Printf.sprintf
    {|"%s": { "dependencies": %s
      , "dist": { "tarball": "http://x/%s.tgz", "integrity": "sha512-x" } }|}
    at needs at

let found written =
  Registry.Npm.releases (packument written)

let offered written =
  List.map
    (fun (release : Registry.Npm.release) -> Version.show release.version)
    (found written).releases

let test_a_release_we_can_read_is_offered _ =
  let written =
    [ release "1.0.0" {|{}|}; release "2.0.1" {|{ "left": "^1.1.0" }|} ]
  in
  assert_equal ~printer:Sample.names [ "1.0.0"; "2.0.1" ] (offered written)

let test_a_range_we_cannot_read_takes_the_release_out _ =
  assert_equal ~printer:Sample.names [ "1.0.0" ]
    (offered
       [ release "1.0.0" {|{}|}; release "2.0.1" {|{ "left": "~1.1.4" }|} ])

let test_a_range_we_cannot_read_is_remembered _ =
  match
    (found [ release "2.0.1" {|{ "left": "~1.1.4" }|} ]).refusals
  with
  | [ refusal ] ->
      assert_equal ~printer:Fun.id "2.0.1" (Version.show refusal.at);
      assert_equal ~printer:Fun.id "left" refusal.dependency;
      assert_equal ~printer:Fun.id "~1.1.4" refusal.range
  | _ -> assert_failure "the range was not remembered"

let suite =
  [
    "a_tarball_lands_without_its_own_folder"
    >:: test_a_tarball_lands_without_its_own_folder;
    "the_tarball_is_kept" >:: test_the_tarball_is_kept;
    "a_wrong_checksum_is_refused" >:: test_a_wrong_checksum_is_refused;
    "a_checksum_we_cannot_read_is_refused"
    >:: test_a_checksum_we_cannot_read_is_refused;
    "a_tarball_cannot_write_outside_itself"
    >:: test_a_tarball_cannot_write_outside_itself;
    "a_tarball_must_hold_one_folder" >:: test_a_tarball_must_hold_one_folder;
    "a_release_we_can_read_is_offered"
    >:: test_a_release_we_can_read_is_offered;
    "a_range_we_cannot_read_takes_the_release_out"
    >:: test_a_range_we_cannot_read_takes_the_release_out;
    "a_range_we_cannot_read_is_remembered"
    >:: test_a_range_we_cannot_read_is_remembered;
  ]
