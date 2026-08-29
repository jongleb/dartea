open OUnit2
open Packages

let version written =
  match Version.of_string written with
  | Some version -> version
  | None -> assert_failure (written ^ " is not a version")

let range written =
  match Version.Range.of_string written with
  | Some range -> range
  | None -> assert_failure (written ^ " is not a range")

let test_versions_round_trip _ =
  List.iter
    (fun written ->
      assert_equal ~printer:Fun.id written (Version.show (version written)))
    [ "1.0.0"; "0.19.1"; "10.2.30" ]

let test_versions_are_three_numbers _ =
  List.iter
    (fun written ->
      assert_equal ~printer:(fun _ -> written) None (Version.of_string written))
    [ "1.0"; "1.0.0.0"; "1.0.x"; ""; "1.0.-1"; "1.0.+1"; "v1.0.0"; "1.0.0x" ]

let test_versions_order _ =
  let ordered = [ "1.0.0"; "1.0.2"; "1.3.0"; "2.0.0"; "10.0.0" ] in
  let sorted = List.sort Version.compare (List.rev_map version ordered) in
  assert_equal ~printer:Sample.names ordered (List.map Version.show sorted)

let allows written picked =
  Version.Interval.holds (Version.Interval.of_range (range written)) (version picked)

let test_an_exact_range_takes_one_version _ =
  assert_bool "1.2.0 was refused" (allows "1.2.0" "1.2.0");
  assert_bool "1.2.1 was taken" (not (allows "1.2.0" "1.2.1"));
  assert_bool "1.1.9 was taken" (not (allows "1.2.0" "1.1.9"))

let test_a_caret_range_takes_the_major _ =
  assert_bool "1.2.0 was refused" (allows "^1.2.0" "1.2.0");
  assert_bool "1.9.9 was refused" (allows "^1.2.0" "1.9.9");
  assert_bool "1.1.9 was taken" (not (allows "^1.2.0" "1.1.9"));
  assert_bool "2.0.0 was taken" (not (allows "^1.2.0" "2.0.0"))

let test_ranges_round_trip _ =
  List.iter
    (fun written ->
      assert_equal ~printer:Fun.id written (Version.Range.show (range written)))
    [ "1.2.0"; "^1.2.0" ]

let met one other =
  Option.map
    (fun (bounds : Version.Interval.t) ->
      (Version.show bounds.least, Version.show bounds.below))
    (Version.Interval.meet (Version.Interval.of_range (range one))
       (Version.Interval.of_range (range other)))

let printer = function
  | None -> "nothing"
  | Some (least, below) -> least ^ ".." ^ below

let test_overlapping_ranges_meet _ =
  assert_equal ~printer
    (Some ("1.3.0", "2.0.0"))
    (met "^1.2.0" "^1.3.0")

let test_clashing_ranges_do_not_meet _ =
  assert_equal ~printer None (met "^1.2.0" "^2.0.0");
  assert_equal ~printer None (met "1.2.0" "1.3.0")

let releases =
  [ ("a", [ "1.0.0"; "1.1.0"; "2.0.0" ]); ("b", [ "1.0.0"; "1.5.0" ]) ]

let view needs =
  {
    Solver.versions =
      (fun package ->
        List.map version
          (Option.value (List.assoc_opt package releases) ~default:[]));
    dependencies =
      (fun package picked ->
        List.map
          (fun (name, written) -> (name, range written))
          (Option.value
             (List.assoc_opt (package ^ "@" ^ Version.show picked) needs)
             ~default:[]));
  }

let shown_problem = function
  | Solver.Unknown_package { package; _ } -> "unknown package " ^ package
  | Solver.No_version { package; _ } -> "no version of " ^ package

let wanted written = List.map (fun (name, text) -> (name, range text)) written

let solved needs written =
  match Solver.solved (view needs) (wanted written) with
  | Ok picked ->
      let named (pick : Pick.t) =
        pick.package ^ "@" ^ Version.show pick.version
      in
      List.map named picked
  | Error (Solver.Unknown_package { package; _ }) ->
      assert_failure ("no such package: " ^ package)
  | Error (Solver.No_version { package; _ }) ->
      assert_failure ("no version of " ^ package)

let refused needs written =
  match Solver.solved (view needs) (wanted written) with
  | Ok _ -> assert_failure "the dependencies resolved"
  | Error problem -> problem

let test_the_newest_allowed_version_wins _ =
  assert_equal ~printer:Sample.names [ "a@1.1.0" ]
    (solved [] [ ("a", "^1.0.0") ])

let test_transitive_dependencies_are_pulled_in _ =
  let needs = [ ("a@1.1.0", [ ("b", "^1.0.0") ]) ] in
  assert_equal ~printer:Sample.names
    [ "a@1.1.0"; "b@1.5.0" ]
    (solved needs [ ("a", "^1.0.0") ])

let test_one_version_per_package _ =
  let needs = [ ("a@1.1.0", [ ("b", "1.0.0") ]) ] in
  assert_equal ~printer:Sample.names
    [ "a@1.1.0"; "b@1.0.0" ]
    (solved needs [ ("a", "^1.0.0"); ("b", "^1.0.0") ])

let test_an_older_version_is_taken_when_the_newest_clashes _ =
  let needs =
    [ ("a@1.1.0", [ ("b", "^2.0.0") ]); ("a@1.0.0", [ ("b", "^1.0.0") ]) ]
  in
  assert_equal ~printer:Sample.names
    [ "a@1.0.0"; "b@1.5.0" ]
    (solved needs [ ("a", "^1.0.0") ])

let test_a_clash_names_everyone_who_asked _ =
  let needs = [ ("a@2.0.0", [ ("b", "^2.0.0") ]) ] in
  match refused needs [ ("a", "2.0.0"); ("b", "^1.0.0") ] with
  | Solver.No_version { package; asked } ->
      assert_equal ~printer:Fun.id "b" package;
      assert_equal ~printer:Sample.names
        [ "your dependencies"; "a" ]
        (List.map fst asked);
      assert_equal ~printer:Sample.names [ "^1.0.0"; "^2.0.0" ]
        (List.map snd asked)
  | problem -> assert_failure (shown_problem problem)

let test_an_unknown_package_is_named _ =
  match refused [] [ ("nope", "^1.0.0") ] with
  | Solver.Unknown_package { package; asked_by } ->
      assert_equal ~printer:Fun.id "nope" package;
      assert_equal ~printer:Fun.id "your dependencies" asked_by
  | problem -> assert_failure (shown_problem problem)

let read_manifest written =
  let folder = Sample.folder () in
  Sample.written ~folder ~path:Manifest.file_name written;
  Manifest.of_folder (Fpath.v folder)

let manifest_of written =
  match read_manifest written with
  | Some manifest -> manifest
  | None -> assert_failure "the manifest was not found"

let test_a_manifest_reads_ranges _ =
  let manifest =
    manifest_of
      {|{ "name": "app"
        , "dependencies": { "@dartea/router": "^1.2.0", "@dartea/ui": "2.0.0" }
        }|}
  in
  assert_equal ~printer:Sample.names [ "src" ] manifest.source_directories;
  assert_equal ~printer:Sample.names
    [ "@dartea/router"; "@dartea/ui" ]
    (List.map fst manifest.dependencies);
  assert_equal ~printer:Sample.names [ "^1.2.0"; "2.0.0" ]
    (List.map (fun (_, held) -> Version.Range.show held) manifest.dependencies)

let test_a_manifest_defaults_to_src _ =
  let manifest = manifest_of {|{ "name": "app" }|} in
  assert_equal ~printer:Sample.names [ "src" ] manifest.source_directories;
  assert_equal ~printer:Sample.names [] (List.map fst manifest.dependencies)

let test_a_manifest_takes_source_directories _ =
  let manifest =
    manifest_of {|{ "name": "app", "source-directories": ["app", "vendor"] }|}
  in
  assert_equal ~printer:Sample.names [ "app"; "vendor" ]
    manifest.source_directories

let test_a_bad_range_is_refused _ =
  match read_manifest {|{ "dependencies": { "@dartea/ui": "~1.2" } }|} with
  | _ -> assert_failure "the range was accepted"
  | exception Diagnostic.Failure.Found { problem = Bad_field { field; _ }; _ }
    ->
      assert_equal ~printer:Fun.id "dependencies" field

let test_a_lock_round_trips _ =
  let packages =
    List.map Pick.of_pair
      [ ("@dartea/ui", version "2.0.0"); ("@dartea/router", version "1.2.0") ]
  in
  let folder = Sample.folder () in
  Sample.written ~folder ~path:Lock.file_name (Lock.shown packages);
  match Lock.of_folder (Fpath.v folder) with
  | None -> assert_failure "the lock file was not found"
  | Some lock ->
      assert_equal ~printer:Sample.names
        [ "@dartea/router"; "@dartea/ui" ]
        (List.map (fun (pick : Pick.t) -> pick.package) lock.packages);
      assert_equal ~printer:Sample.names [ "1.2.0"; "2.0.0" ]
        (List.map (fun (pick : Pick.t) -> Version.show pick.version)
           lock.packages)

let suite =
  [
    "versions_round_trip" >:: test_versions_round_trip;
    "versions_are_three_numbers" >:: test_versions_are_three_numbers;
    "versions_order" >:: test_versions_order;
    "an_exact_range_takes_one_version"
    >:: test_an_exact_range_takes_one_version;
    "a_caret_range_takes_the_major" >:: test_a_caret_range_takes_the_major;
    "ranges_round_trip" >:: test_ranges_round_trip;
    "overlapping_ranges_meet" >:: test_overlapping_ranges_meet;
    "clashing_ranges_do_not_meet" >:: test_clashing_ranges_do_not_meet;
    "the_newest_allowed_version_wins" >:: test_the_newest_allowed_version_wins;
    "transitive_dependencies_are_pulled_in"
    >:: test_transitive_dependencies_are_pulled_in;
    "one_version_per_package" >:: test_one_version_per_package;
    "an_older_version_is_taken_when_the_newest_clashes"
    >:: test_an_older_version_is_taken_when_the_newest_clashes;
    "a_clash_names_everyone_who_asked"
    >:: test_a_clash_names_everyone_who_asked;
    "an_unknown_package_is_named" >:: test_an_unknown_package_is_named;
    "a_manifest_reads_ranges" >:: test_a_manifest_reads_ranges;
    "a_manifest_defaults_to_src" >:: test_a_manifest_defaults_to_src;
    "a_manifest_takes_source_directories"
    >:: test_a_manifest_takes_source_directories;
    "a_bad_range_is_refused" >:: test_a_bad_range_is_refused;
    "a_lock_round_trips" >:: test_a_lock_round_trips;
  ]
