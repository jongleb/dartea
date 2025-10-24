open OUnit2

module Main = Parse.Main

let assert_parse_ok name input =
  name >:: fun _ ->
  match Main.parse input with
  | Ok _ -> assert_bool "parsed" true
  | Error e -> assert_failure (Printexc.to_string e)

let suite =
  [
    assert_parse_ok
      "let_singleline"
      {|
a: Int
a = let x = 2 in x
|};
    assert_parse_ok
      "let_multibind"
      {|
b: Int
b = let x = 2
y = 3 in 42
|};
    assert_parse_ok
      "case_of_lower_vars"
      {|
c: Int
c = case v of
  just -> 1
  _    -> 0
|};
    assert_parse_ok
      "if_then_else_singleline"
      {|
d: Int
d = if cond then 1 else 2
|};
    assert_parse_ok
      "list_and_record_simple"
      {|
e: Int
e = let xs = [1, 2, 3]
r = { a = 1, b = 2 } in 42
|};
  ]


