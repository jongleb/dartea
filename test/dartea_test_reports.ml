open OUnit2
module Error = Reporting.Error

type sample =
  | Compiled of (string * string) list
  | Rendered of { file : string; content : string; region : Data.Region.t }

let at ~line ~column ~length ~offset =
  {
    Data.Region.file = "Main.elm";
    start = { offset; line; column };
    stop = { offset = offset + length; line; column = column + length };
  }

let main content = Compiled [ ("Main.elm", content) ]

let sample_of_type (problem : Reporting.Type_error.t) =
  match problem with
  | Bad_expression _ ->
      main {|f : Int -> Int
f n = n

bad = f "text"
|}
  | Bad_pattern _ ->
      main {|answer =
    case "text" of
        1 ->
            1
|}
  | Infinite_type _ -> main {|bad x = x x
|}
  | Bad_arity { thing = A_variant; _ } ->
      main {|first m =
    case m of
        Just a b ->
            a
|}
  | Bad_arity { thing = A_type; _ } ->
      main {|type alias Pair a = ( a, a )

both : Pair Int Int
both = ( 1, 2 )
|}
  | Case_without_branches ->
      Rendered
        {
          file = "Main.elm";
          content = "empty m =\n    case m of\n";
          region = at ~line:2 ~column:5 ~length:9 ~offset:14;
        }

let sample_of_name (problem : Reporting.Name_error.t) =
  match problem with
  | Unknown_module _ ->
      main {|module Main exposing (..)

import Tuplee

xs = 1
|}
  | Not_exposed _ ->
      Compiled
        [
          ( "Main.elm",
            "module Main exposing (..)\n\nimport Paint exposing (secret)\n\nx = 1\n" );
          ("Paint.elm", "module Paint exposing (shown)\n\nshown = 1\n\nsecret = 2\n");
        ]
  | Ctors_not_exposed _ ->
      Compiled
        [
          ( "Main.elm",
            "module Main exposing (..)\n\nimport Paint exposing (Colour(..))\n\nx = 1\n" );
          ("Paint.elm", "module Paint exposing (Colour)\n\ntype Colour = Red | Blue\n");
        ]
  | Ambiguous _ ->
      Compiled
        [
          ( "Main.elm",
            "module Main exposing (..)\n\nimport One exposing (thing)\nimport Two exposing (thing)\n\nx = thing\n"
          );
          ("One.elm", "module One exposing (thing)\n\nthing = 1\n");
          ("Two.elm", "module Two exposing (thing)\n\nthing = 2\n");
        ]
  | Unknown_kernel _ ->
      main {|reverse : String -> String
reverse = Elm.Kernel.String.reverse
|}
  | Kernel_needs_annotation _ -> main {|size = Elm.Kernel.String.length
|}
  | Kernel_arity_mismatch _ ->
      main {|size : String -> String -> Int
size = Elm.Kernel.String.length
|}
  | Duplicate_declaration _ -> main {|x = 1

x = 2
|}
  | Duplicate_binder _ ->
      main {|first t =
    case t of
        ( a, a ) ->
            a
|}
  | Unbound_value _ -> main {|x = missing
|}
  | Unknown_constructor _ -> main {|x = Missing
|}
  | Unknown_type _ -> main {|x : Missing
x = 1
|}
  | Import_cycle _ ->
      Compiled
        [
          ("Main.elm", "module Main exposing (..)\n\nimport Other\n\nx = Other.y\n");
          ("Other.elm", "module Other exposing (y)\n\nimport Main\n\ny = Main.x\n");
        ]
  | Recursive_value _ -> main {|a : Int
a = b + 1

b : Int
b = a + 1
|}

let sample_of_syntax (problem : Reporting.Syntax_error.t) =
  match problem with
  | Unexpected_input _ -> main {|x = )
|}
  | Unknown_character _ -> main {|x = ?
|}
  | Unterminated _ -> main {|x = "unfinished
|}
  | Empty_character -> main {|x = ''
|}
  | Crowded_character -> main {|x = 'ab'
|}
  | Unknown_escape _ -> main {|x = "a\q"
|}
  | Too_many_tuple_parts _ -> main {|x = ( 1, 2, 3, 4 )
|}
  | Module_name_mismatch _ -> main {|module Deep.Thing exposing (..)

x = 1
|}

let entry_sample =
  "module Main exposing (main)\n\n\nmain : Int\nmain =\n    1\n"

let sample_of_project (problem : Reporting.Project_error.t) =
  match problem with
  | Entry_not_exposed _ | Bad_entry _ ->
      Rendered
        {
          file = "Main.elm";
          content = entry_sample;
          region = at ~line:5 ~column:1 ~length:4 ~offset:38;
        }
  | Unknown_folder _ | No_sources _ | Bad_json _ | Missing_field _ | Bad_field _
  | Missing_source_directory _ | Duplicate_module _ | Unknown_entry _
  | No_entry _ | Unknown_delivery _ | Delivery_needs_entry _ ->
      let file = Reporting.Project_error.file_of problem in
      Rendered { file; content = ""; region = { Data.Region.nowhere with file } }

let sample (problem : Error.problem) =
  match problem with
  | Error.Type problem -> sample_of_type problem
  | Error.Name problem -> sample_of_name problem
  | Error.Syntax problem -> sample_of_syntax problem
  | Error.Project problem -> sample_of_project problem

let a_type = Typed.Type.TInt

let a_record =
  Typed.Type.TRecord (Typed.Type.TRowExtend ("x", Typed.Type.TInt, Typed.Type.TRowEmpty))
let a_name = Data.Name.local "Thing"

let type_kinds : (string * Reporting.Type_error.t) list =
  [
    ( "bad-expression",
      Bad_expression
        {
          category = Number;
          found = a_type;
          expected = No_expectation a_type;
        } );
    ( "bad-pattern",
      Bad_pattern
        {
          category = P_int;
          found = a_type;
          expected = Pattern_no_expectation a_type;
        } );
    ("infinite-type", Infinite_type { category = Lambda; found = a_type });
    ( "variant-arity",
      Bad_arity { thing = A_variant; name = a_name; expects = 1; given = 2 } );
    ("case-without-branches", Case_without_branches);
    ( "type-arity",
      Bad_arity { thing = A_type; name = a_name; expects = 1; given = 2 } );
  ]

let name_kinds : (string * Reporting.Name_error.t) list =
  [
    ("unknown-module", Unknown_module { qualifier = "Tuplee"; near = [] });
    ( "not-exposed",
      Not_exposed { module_name = "Paint"; name = "secret"; near = [] } );
    ("ctors-not-exposed", Ctors_not_exposed { module_name = "Paint"; type_name = "Colour" });
    ("ambiguous", Ambiguous { name = "thing"; modules = [ "One"; "Two" ] });
    ( "unknown-kernel",
      Unknown_kernel { module_name = "Elm.Kernel.String"; exported_name = "reverse" } );
    ("kernel-needs-annotation", Kernel_needs_annotation { name = "size" });
    ("kernel-arity-mismatch", Kernel_arity_mismatch { declared = 2; kernel = 1 });
    ("duplicate-declaration", Duplicate_declaration { name = "x" });
    ("duplicate-binder", Duplicate_binder { name = "a" });
    ( "unbound-value",
      Unbound_value { name = a_name; prefix = No_prefix; near = [] } );
    ( "unknown-constructor",
      Unknown_constructor { name = a_name; prefix = No_prefix; near = [] } );
    ( "unknown-type",
      Unknown_type { name = a_name; prefix = No_prefix; near = [] } );
    ("import-cycle", Import_cycle { modules = [ "Main"; "Other" ] });
    ("recursive-value", Recursive_value { names = [ "a"; "b" ] });
  ]

let syntax_kinds : (string * Reporting.Syntax_error.t) list =
  [
    ("unexpected-input", Unexpected_input { found = ")" });
    ("unknown-character", Unknown_character { found = "?" });
    ("unterminated", Unterminated { what = Text });
    ("empty-character", Empty_character);
    ("crowded-character", Crowded_character);
    ("unknown-escape", Unknown_escape { found = "q" });
    ("too-many-tuple-parts", Too_many_tuple_parts { given = 4 });
    ("module-name-mismatch", Module_name_mismatch { expected = "Main" });
  ]

let project_kinds : (string * Reporting.Project_error.t) list =
  [
    ("unknown-folder", Unknown_folder { folder = "src" });
    ("no-sources", No_sources { folder = "src" });
    ( "bad-json",
      Bad_json
        {
          file = "elm.json";
          problem = "Line 1, bytes 25-27: Expected string or identifier but found '} '";
        } );
    ("missing-field", Missing_field { file = "elm.json"; field = "type" });
    ( "bad-field",
      Bad_field
        {
          file = "elm.json";
          field = "source-directories";
          expected = "an array of strings";
        } );
    ( "missing-source-directory",
      Missing_source_directory { file = "elm.json"; folder = "vendor" } );
    ("unknown-entry", Unknown_entry { path = "src/Nope.elm" });
    ("no-entry", No_entry { module_name = "Main"; declaration = "main" });
    ( "entry-not-exposed",
      Entry_not_exposed
        {
          delivery = "classic_js_browser";
          module_name = "Main";
          declaration = "main";
        } );
    ( "bad-entry",
      Bad_entry
        {
          delivery = "classic_js_browser";
          module_name = "Main";
          declaration = "main";
          expected = "String";
          found = "Int";
        } );
    ( "unknown-delivery",
      Unknown_delivery
        { name = "nope"; known = [ "esm_folder"; "classic_js_browser" ] } );
    ("delivery-needs-entry", Delivery_needs_entry { delivery = "classic_js_browser" });
    ( "duplicate-module",
      Duplicate_module
        {
          name = "Deep.Thing";
          one = "app/Deep/Thing.elm";
          other = "vendor/Deep/Thing.elm";
        } );
  ]

let kinds =
  List.map (fun (name, problem) -> (name, Error.Type problem)) type_kinds
  @ List.map (fun (name, problem) -> (name, Error.Name problem)) name_kinds
  @ List.map (fun (name, problem) -> (name, Error.Syntax problem)) syntax_kinds
  @ List.map (fun (name, problem) -> (name, Error.Project problem)) project_kinds

let constructor_of (problem : Error.problem) =
  let written = Error.show_problem problem in
  let inner = String.length "_error." in
  let rec after index =
    if index + inner > String.length written then written
    else if String.equal (String.sub written index inner) "_error." then
      let start = index + inner in
      let rec ends stop =
        if
          stop < String.length written
          && (written.[stop] = '_'
             || (written.[stop] >= 'A' && written.[stop] <= 'Z')
             || (written.[stop] >= 'a' && written.[stop] <= 'z'))
        then ends (stop + 1)
        else stop
      in
      String.sub written start (ends start - start)
    else after (index + 1)
  in
  after 0

let same_kind (one : Error.problem) (other : Error.problem) =
  String.equal (constructor_of one) (constructor_of other)

let reported files =
  match
    Dartea.Compiler.compile_modules ~entry:None
      (List.map
         (fun (path, content) -> Project.Elm_file.of_path ~path content)
         files)
  with
  | outcome -> (
      match outcome.errors with [] -> None | error :: _ -> Some error)
  | exception Error.Found error -> Some error

let rendered (problem : Error.problem) =
  match sample problem with
  | Rendered { file; content; region } ->
      let sources = Reporting.Sources.of_list [ (file, content) ] in
      Reporting.Report.to_string ~colours:false
        (Reporting.Sources.report sources { region; problem })
  | Compiled files -> (
      match reported files with
      | None -> assert_failure "the sample compiled without an error"
      | Some error ->
          assert_bool
            (Printf.sprintf "the sample raised %s instead of %s"
               (Error.show_problem error.problem)
               (Error.show_problem problem))
            (same_kind error.problem problem);
          let sources = Reporting.Sources.of_list files in
          Reporting.Report.to_string ~colours:false
            (Reporting.Sources.report sources error))

let contexts =
  [
    ("context-if-condition", [ ("Main.elm", "x = if 1 then 2 else 3\n") ]);
    ("context-if-branch", [ ("Main.elm", "x = if True then 1 else \"a\"\n") ]);
    ( "context-case-branch",
      [
        ( "Main.elm",
          "answer m =\n    case m of\n        1 ->\n            1\n\n        _ ->\n            \"a\"\n"
        );
      ] );
    ( "context-case-pattern",
      [
        ( "Main.elm",
          "answer : String -> Int\nanswer m =\n    case m of\n        1 ->\n            1\n"
        );
      ] );
    ("context-list-element", [ ("Main.elm", "xs = [ 1, \"a\" ]\n") ]);
    ("context-annotation", [ ("Main.elm", "x : Int\nx = \"a\"\n") ]);
    ( "context-field-access",
      [ ("Main.elm", "reach : Int -> Int\nreach r = r.name\n") ] );
    ("context-cons-tail", [ ("Main.elm", "xs = 1 :: \"a\"\n") ]);
    ( "context-record-field",
      [
        ( "Main.elm",
          "rename : { name : String } -> { name : String }\nrename r = { r | name = 1 }\n"
        );
      ] );
    ("context-callee", [ ("Main.elm", "x = 1 2\n") ]);
    ( "context-qualified-value-missing",
      [
        ( "Main.elm",
          "module Main exposing (..)\n\nimport Paint\n\nx = Paint.secret\n" );
        ("Paint.elm", "module Paint exposing (shown)\n\nshown = 1\n\nsecret = 2\n");
      ] );
    ( "context-unimported-module",
      [
        ("Main.elm", "xs = Paint.shown\n");
        ("Paint.elm", "module Paint exposing (shown)\n\nshown = 1\n");
      ] );
    ( "context-constructor-payload",
      [
        ( "Main.elm",
          "answer : Maybe Int -> Int\nanswer m =\n    case m of\n        Just \"a\" ->\n            1\n\n        Nothing ->\n            0\n"
        );
      ] );
  ]

let golden name = Filename.concat "errors" (name ^ ".txt")


let promoting_into = Sys.getenv_opt "DARTEA_PROMOTE_ERRORS"

let test_report (name, problem) =
  name >:: fun _ ->
  let path = golden name in
  let produced = rendered problem in
  Option.iter
    (fun directory ->
      Out_channel.with_open_bin
        (Filename.concat directory (name ^ ".txt"))
        (fun out -> Out_channel.output_string out produced))
    promoting_into;
  assert_bool (Printf.sprintf "%s is missing" path) (Sys.file_exists path);
  assert_equal ~printer:Fun.id (Sample.read path) produced

let sample_of_warning (problem : Reporting.Warning.problem) =
  match problem with
  | Missing_patterns _ ->
      [
        ( "Main.elm",
          "answer m =\n    case m of\n        Just x ->\n            x\n" );
      ]
  | Redundant_pattern _ ->
      [
        ( "Main.elm",
          "answer m =\n    case m of\n        Just x ->\n            x\n\n        Just y ->\n            y\n\n        Nothing ->\n            0\n"
        );
      ]

let warning_kinds : (string * Reporting.Warning.problem) list =
  [
    ("missing-patterns", Missing_patterns { unhandled = [] });
    ("redundant-pattern", Redundant_pattern { index = 2 });
  ]

let warned files =
  match
    Dartea.Compiler.compile_modules ~entry:None
      (List.map
         (fun (path, content) -> Project.Elm_file.of_path ~path content)
         files)
  with
  | outcome ->
      List.concat_map
        (fun (compiled : Dartea.Compiler.compiled) -> compiled.warnings)
        outcome.output
  | exception Error.Found error ->
      assert_failure
        (Printf.sprintf "the sample was rejected: %s"
           (Error.show_problem error.problem))

let same_warning (one : Reporting.Warning.problem)
    (other : Reporting.Warning.problem) =
  match (one, other) with
  | Missing_patterns _, Missing_patterns _
  | Redundant_pattern _, Redundant_pattern _ ->
      true
  | (Missing_patterns _ | Redundant_pattern _), _ -> false

let test_warning (name, problem) =
  name >:: fun _ ->
  let files = sample_of_warning problem in
  match List.find_opt (fun (found : Reporting.Warning.t) -> same_warning found.problem problem) (warned files) with
  | None -> assert_failure "the sample produced no such warning"
  | Some found ->
      let produced =
        Reporting.Report.to_string ~colours:false
          (Reporting.Sources.warning (Reporting.Sources.of_list files) found)
      in
      let path = golden name in
      Option.iter
        (fun directory ->
          Out_channel.with_open_bin
            (Filename.concat directory (name ^ ".txt"))
            (fun out -> Out_channel.output_string out produced))
        promoting_into;
      assert_bool (Printf.sprintf "%s is missing" path) (Sys.file_exists path);
      assert_equal ~printer:Fun.id (Sample.read path) produced

let test_context (name, files) =
  name >:: fun _ ->
  match reported files with
  | None -> assert_failure "the sample compiled without an error"
  | Some error ->
      let produced =
        Reporting.Report.to_string ~colours:false
          (Reporting.Sources.report (Reporting.Sources.of_list files) error)
      in
      let path = golden name in
      Option.iter
        (fun directory ->
          Out_channel.with_open_bin
            (Filename.concat directory (name ^ ".txt"))
            (fun out -> Out_channel.output_string out produced))
        promoting_into;
      assert_bool (Printf.sprintf "%s is missing" path) (Sys.file_exists path);
      assert_equal ~printer:Fun.id (Sample.read path) produced

let test_colours_are_a_layer _ =
  let files = [ ("Main.elm", "x = missing\n") ] in
  match reported files with
  | None -> assert_failure "the sample compiled without an error"
  | Some error ->
      let report =
        Reporting.Sources.report (Reporting.Sources.of_list files) error
      in
      let coloured = Reporting.Report.to_string ~colours:true report in
      let plain = Reporting.Report.to_string ~colours:false report in
      assert_bool "colours add ansi escapes"
        (String.length coloured > String.length plain);
      assert_bool "plain output carries no escapes"
        (not (String.exists (fun letter -> Char.code letter = 27) plain));
      assert_equal ~printer:Fun.id plain
        (String.concat ""
           (String.split_on_char '\027' coloured
           |> List.map (fun part ->
                  match String.index_opt part 'm' with
                  | Some stop ->
                      String.sub part (stop + 1) (String.length part - stop - 1)
                  | None -> part)))

let suite =
  ("colours are a separate layer" >:: test_colours_are_a_layer)
  :: List.map test_report kinds
  @ List.map test_context contexts
  @ List.map test_warning warning_kinds
