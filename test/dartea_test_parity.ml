open OUnit2

let upstream = "upstream"

let recorded module_name =
  let path = Filename.concat upstream (module_name ^ ".txt") in
  String.split_on_char '\n' (Sample.read path)
  |> List.filter (fun line -> not (String.equal (String.trim line) ""))
  |> List.sort_uniq String.compare

let shown module_ =
  let module_name = Prelude.name module_ in
  let theirs = recorded module_name in
  let ours = Parity.names (Prelude.source module_) in
  Printf.sprintf "%s %d/%d" module_name (List.length ours) (List.length theirs)

let expected =
  [
    "Basics 40/60";
    "Browser 6/6";
    "Browser.Dom 2/10";
    "Browser.Events 12/12";
    "Browser.Navigation 8/8";
    "Char 11/14";
    "Dict 23/23";
    "Html 101/101";
    "Html.Attributes 86/86";
    "Html.Events 20/20";
    "Html.Keyed 3/3";
    "Html.Lazy 8/8";
    "Http 30/37";
    "Json.Decode 32/35";
    "Json.Encode 9/12";
    "List 37/37";
    "Maybe 8/8";
    "Platform 1/7";
    "Platform.Cmd 4/4";
    "Platform.Sub 4/4";
    "Result 11/11";
    "String 44/45";
    "Task 14/14";
    "Time 4/21";
    "Tuple 6/6";
    "Url 6/6";
    "Url.Builder 9/9";
    "Url.Parser 13/13";
    "Url.Parser.Internal 1/1";
    "Url.Parser.Query 13/13";
    "VirtualDom 23/23";
  ]

let test_every_module_against_upstream _ =
  assert_equal ~printer:(String.concat "\n") expected
    (List.sort String.compare (List.map shown Prelude.all))

let suite = [ "every_module_against_upstream" >:: test_every_module_against_upstream ]
