open OUnit2

let emitted ~entry content =
  let outcome =
    Dartea.Compiler.compile_modules ~entry
      (Project.Sources.of_list
         [ Project.Elm_file.of_path ~path:"Main.elm" content ])
  in
  match outcome.errors with
  | [] ->
      Node_runner.source_of ~module_name:"Main" (Node_runner.output_of outcome)
  | error :: _ ->
      assert_failure
        (Sample.rendered (Reporting.Sources.of_list outcome.sources) error)

let holds ~needle source =
  assert_bool (Printf.sprintf "%s is missing from\n%s" needle source)
    (Node_runner.contains ~needle source)

let lacks ~needle source =
  assert_bool (Printf.sprintf "%s is still in\n%s" needle source)
    (not (Node_runner.contains ~needle source))

let counting = {|module Main exposing (kept)


hidden : Int -> Int
hidden n =
    if n <= 0 then
        0

    else
        hidden (n - 1)


kept : Int -> Int
kept n =
    n * 2
|}

let test_unreachable_declaration_goes _ =
  lacks ~needle:"hidden" (emitted ~entry:None counting)

let test_exposed_declaration_stays _ =
  let exposing_both =
    Str.global_replace (Str.regexp_string "exposing (kept)")
      "exposing (kept, hidden)" counting
  in
  holds ~needle:"hidden" (emitted ~entry:None exposing_both)

let test_reachable_declaration_stays _ =
  let used =
    Str.global_replace (Str.regexp_string "kept n =\n    n * 2")
      "kept n =\n    hidden n * 2" counting
  in
  holds ~needle:"hidden" (emitted ~entry:None used)

let starting = {|module Main exposing (other)


other : Int
other =
    1


main : String
main =
    "hi"
|}

let test_entry_stays _ =
  holds ~needle:"main" (emitted ~entry:(Some "Main") starting)

let test_entry_goes_when_not_asked_for _ =
  lacks ~needle:"main" (emitted ~entry:None starting)

let suite =
  [
    "unreachable_declaration_goes" >:: test_unreachable_declaration_goes;
    "exposed_declaration_stays" >:: test_exposed_declaration_stays;
    "reachable_declaration_stays" >:: test_reachable_declaration_stays;
    "entry_stays" >:: test_entry_stays;
    "entry_goes_when_not_asked_for" >:: test_entry_goes_when_not_asked_for;
  ]
