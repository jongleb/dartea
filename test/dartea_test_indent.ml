open OUnit2

module Main = Parse.Main

let assert_balanced input =
  let depth, lowest =
    List.fold_left
      (fun (depth, lowest) token ->
        match token with
        | Parse.Parser.INDENT -> (depth + 1, lowest)
        | Parse.Parser.DEDENT -> (depth - 1, min lowest (depth - 1))
        | _ -> (depth, lowest))
      (0, 0) (Utils.layout_stream input)
  in
  assert_equal ~printer:string_of_int ~msg:"unmatched DEDENT" 0 lowest;
  assert_equal ~printer:string_of_int ~msg:"unclosed INDENT" 0 depth

let splice lines index line =
  List.concat
    (List.mapi (fun i l -> if i = index then [ line; l ] else [ l ]) lines)

let assert_blank_lines_ignored input =
  let lines = String.split_on_char '\n' input in
  let expected = Utils.layout_stream input in
  List.iteri
    (fun index _ ->
      if index < List.length lines - 1 then
        List.iter
          (fun width ->
            let blank = String.make width ' ' in
            let spliced = String.concat "\n" (splice lines index blank) in
            assert_bool
              (Printf.sprintf "blank line of width %d at line %d changed layout"
                 width index)
              (Utils.layout_stream spliced = expected))
          [ 0; 1; 3 ])
    lines

(** Parses successfully: the layout pass produced a balanced INDENT/DEDENT
    stream the grammar accepts. *)
let ok name input =
  name >:: fun _ ->
  (match Main.parse ~file:"Main.elm" input with
  | Ok _ -> assert_bool "parsed" true
  | Error error -> assert_failure (Reporting.Error.show error));
  assert_balanced input;
  assert_blank_lines_ignored input

let parses_but_owns_its_blank_lines name input =
  name >:: fun _ ->
  (match Main.parse ~file:"Main.elm" input with
  | Ok _ -> assert_bool "parsed" true
  | Error error -> assert_failure (Reporting.Error.show error));
  assert_balanced input

(** Intentionally ill-laid-out input that must be rejected. Documents the
    error behaviour we want to keep. *)
let rejects name input =
  name >:: fun _ ->
  match Main.parse ~file:"Main.elm" input with
  | Error _ -> assert_bool "rejected" true
  | Ok _ -> assert_failure "expected a layout/parse error, but it parsed"

(** A documented layout limitation: the input is valid Dartea but the current
    indenter cannot lay it out yet, so parsing fails. Pinned as a test so the
    day it starts working (or regresses further) we notice. *)
let known_limitation name input =
  name >:: fun _ ->
  match Main.parse ~file:"Main.elm" input with
  | Error _ -> assert_bool "still unsupported" true
  | Ok _ ->
      assert_failure "now parses — promote this to `ok` in dartea_test_indent"

(* -------------------------------------------------------------------------- *)
(* let / in                                                                   *)
(* -------------------------------------------------------------------------- *)

let let_tests =
  [
    ok "let_tuple_bind_after_name_bind" {|
f: Int
f =
  let
    x =
      1

    ( a, b ) =
      ( 2, 3 )
  in
  x + a + b
|};
    ok "let_record_bind_after_name_bind" {|
f: Int
f =
  let
    x =
      1

    { c } =
      { c = 2 }
  in
  x + c
|};
    ok "let_tuple_bind_after_case_bind" {|
f: Int
f =
  let
    g n =
      case n of
        _ ->
          1

    ( a, b ) =
      ( 2, 3 )
  in
  g 0 + a + b
|};
    ok "let_singleline" {|
a: Int
a = let x = 2 in x
|};
    ok
      "nested_let_in_let"
      {|
o: Int
o = let x = let y = 1 in y in x
|};
    ok
      "let_in_case_arm"
      {|
g: Int
g = case v of
  A -> let y = 1 in y
  B -> 2
|};
    ok
      "let_body_multiline"
      {|
f: Int
f =
  let x = 1
  in x
|};
    ok
      "let_block_bindings"
      {|
f: Int
f =
  let
    x = 1
    y = 2
  in x
|};
    ok
      "let_block_function_binding_with_indented_body"
      {|
f: Int
f =
    let
        keep g = g

        unbox b =
            b
    in
    keep (unbox 1)
|};
    ok
      "let_block_every_binding_has_an_indented_body"
      {|
f: Int
f =
    let
        keep g =
            g

        unbox b =
            b
    in
    keep (unbox 1)
|};
    ok
      "let_block_function_binding_with_several_parameters"
      {|
f: Int
f =
    let
        a = 1

        add x y z =
            x + y + z
    in
    add a a a
|};
    ok
      "let_block_three_function_bindings"
      {|
f: Int
f =
    let
        one x =
            x

        two x =
            x

        three x =
            x
    in
    one (two (three 1))
|};
    ok
      "let_block_binding_body_is_a_case"
      {|
f: Int
f =
    let
        keep g = g

        unbox b =
            case b of
                A ->
                    1

                _ ->
                    0
    in
    keep (unbox v)
|};
    ok
      "let_bindings_left_of_let_keyword"
      {|
f: Int
f =
 let x = 1 in let
  y = 2
  z = 3
 in y
|};
  ]

(* -------------------------------------------------------------------------- *)
(* case / of                                                                  *)
(* -------------------------------------------------------------------------- *)

let case_tests =
  [
    ok
      "case_of_lower_vars"
      {|
c: Int
c = case v of
  just -> 1
  _    -> 0
|};
    ok
      "case_of_ctor_args"
      {|
f: Int
f = case x of
  Just a -> a
  Nothing -> 0
|};
    ok "single_case_arm" {|
f: Int
f = case x of
  A -> 1
|};
    ok
      "case_scrutinee_is_application"
      {|
f: Int
f = case g a b of
  A -> 1
  _ -> 0
|};
    ok
      "case_arm_multiline_body"
      {|
k: Int
k = case v of
  A ->
    1
  B ->
    2
|};
    ok
      "nested_case_in_case_arm"
      {|
f: Int
f = case x of
  A -> case y of
    C -> 1
    D -> 2
  B -> 3
|};
    ok
      "case_arm_with_let_multiline"
      {|
f: Int
f = case x of
  A ->
    let y = 1 in y
  B ->
    2
|};
    (* A blank line between arms must not close the block. *)
    ok
      "blank_line_between_arms"
      {|
f: Int
f = case x of
  A -> 1

  B -> 2
|};
    (* A whitespace-only line inside the block must be ignored too. *)
    ok
      "whitespace_only_line_in_block"
      "\nf: Int\nf = case x of\n  A -> 1\n   \n  B -> 2\n";
  ]

(* -------------------------------------------------------------------------- *)
(* case patterns                                                              *)
(* -------------------------------------------------------------------------- *)

let pattern_tests =
  [
    ok
      "cons_pattern"
      {|
f: Int
f = case xs of
  h :: t -> h
  _ -> 0
|};
    ok
      "list_pattern"
      {|
f: Int
f = case xs of
  [a, b] -> a
  _ -> 0
|};
    ok
      "record_pattern"
      {|
f: Int
f = case r of
  {a, b} -> a
  _ -> 0
|};
  ]

(* -------------------------------------------------------------------------- *)
(* if / then / else                                                           *)
(* -------------------------------------------------------------------------- *)

let if_tests =
  [
    ok "if_then_else_singleline" {|
d: Int
d = if cond then 1 else 2
|};
    ok
      "nested_if"
      {|
i: Int
i = if a then if b then 1 else 2 else 3
|};
    ok
      "if_then_else_multiline"
      {|
f: Int
f = if cond
then 1
else 2
|};
    ok
      "if_multiline_indented_blocks"
      {|
f: Int
f =
  if cond
  then 1
  else 2
|};
    ok
      "if_in_case_arm"
      {|
h: Int
h = case v of
  A -> if c then 1 else 2
  B -> 3
|};
  ]

(* -------------------------------------------------------------------------- *)
(* expressions: application, lambdas, pipes, records, access                  *)
(* -------------------------------------------------------------------------- *)

let expr_tests =
  [
    ok "app_multiarg" {|
f: Int
f = g a b c d
|};
    ok "lambda" {|
l: Int
l = \a b -> a
|};
    ok "curried_lambda" {|
f: Int
f = \a -> \b -> a
|};
    ok
      "lambda_in_case_arm"
      {|
f: Int
f = case x of
  A -> \y -> y
  B -> 2
|};
    ok "pipe_chain" {|
m: Int
m = x |> f |> g
|};
    ok "record_literal" {|
n: Int
n = { a = 1, b = 2 }
|};
    ok
      "record_nested"
      {|
f: Int
f = { a = { b = 1 }, c = 2 }
|};
    ok "unit_expr" {|
f: Int
f = ()
|};
    ok "access_chain" {|
f: Int
f = r.a.b.c
|};
    ok "nested_parens_deep" {|
f: Int
f = ((((1))))
|};
    ok "string_with_spaces" {|
f: Int
f = greet "hello world"
|};
  ]

(* -------------------------------------------------------------------------- *)
(* line-continuation shapes (leading/trailing operators, indented args)       *)
(* -------------------------------------------------------------------------- *)

let continuation_tests =
  [
    (* An application whose arguments hang on the following lines. *)
    ok "application_across_lines" {|
f: Int
f = g
  a
  b
|};
    (* Elm-style pipe-first continuation. *)
    ok "pipe_first_continuation" {|
f: Int
f = x
  |> g
  |> h
|};
    ok "binop_trailing_operator" {|
f: Int
f = 1 +
  2
|};
    ok "binop_leading_operator" {|
f: Int
f = 1
  + 2
|};
    (* Elm-style comma-first record and list. *)
    ok
      "comma_first_record"
      {|
f: Int
f =
  { a = 1
  , b = 2
  }
|};
    ok
      "comma_first_list"
      {|
f: Int
f =
  [ 1
  , 2
  , 3
  ]
|};
    ok
      "deeply_nested_record_across_lines"
      {|
f: Int
f =
  { a =
    { b = 1 }
  , c = 2
  }
|};
    (* An oddly-but-consistently indented body is fine. *)
    ok "very_deep_body_indent" {|
f: Int
f =
            1
|};
  ]

(* -------------------------------------------------------------------------- *)
(* layout inside a sub-expression                                             *)
(* -------------------------------------------------------------------------- *)

let subexpr_tests =
  [
    ok
      "let_in_parenthesised_argument"
      {|
f: Int
f = g (let x = 1 in x)
|};
    ok
      "let_inside_arithmetic"
      {|
f: Int
f = 1 + (let x = 2 in x)
|};
  ]

(* -------------------------------------------------------------------------- *)
(* type declarations & signatures spanning multiple lines                     *)
(* -------------------------------------------------------------------------- *)

let type_tests =
  [
    ok
      "type_decl_multiline"
      {|
type Color
  = Red
  | Green
  | Blue
|};
    ok
      "type_alias_record_multiline"
      {|
type alias User =
  { name: String
  , age: Int
  }
|};
    ok
      "type_signature_across_lines"
      {|
f: Int
  -> Int
  -> Int
f a b = 1
|};
  ]

(* -------------------------------------------------------------------------- *)
(* several top-level declarations                                             *)
(* -------------------------------------------------------------------------- *)

let toplevel_tests =
  [
    ok
      "two_decls_with_blank_line"
      {|
f: Int
f = let x = 1 in x

g: Int
g = 2
|};
    ok
      "three_decls"
      {|
a: Int
a = 1

b: Int
b = 2

c: Int
c = 3
|};
    (* Back-to-back declarations with no blank line separating them. *)
    ok "two_decls_no_blank_line" {|
a: Int
a = 1
b: Int
b = 2
|};
    ok "declaration_without_signature" {|
f = 1
|};
    (* A case block that dedents straight into the next declaration. *)
    ok
      "case_block_then_next_decl"
      {|
f: Int
f = case x of
  A -> 1
  B -> 2

g: Int
g = 2
|};
  ]

(* -------------------------------------------------------------------------- *)
(* whitespace corner cases                                                    *)
(* -------------------------------------------------------------------------- *)

let whitespace_tests =
  [
    ok
      "several_blank_lines_between_decls"
      "\na: Int\na = 1\n\n\n\nb: Int\nb = 2\n";
    ok "trailing_whitespace_on_lines" "\nf: Int   \nf = 1   \n";
    (* Tabs are accepted as indentation. *)
    ok "tabs_as_indentation" "\nf: Int\nf = case x of\n\tA -> 1\n\tB -> 2\n";
    (* No trailing newline: EOF must still close the open block. *)
    ok "no_trailing_newline" "\nf: Int\nf = 1";
    ok "empty_input" "";
    ok "whitespace_only_input" "   \n\n  \n";
  ]

(* -------------------------------------------------------------------------- *)
(* let bindings with a type annotation                                        *)
(* -------------------------------------------------------------------------- *)

let let_annotation_tests =
  [
    ok "string_literal_starts_a_case_arm" {|
f: Int
f =
    case v of
        "a" ->
            1

        _ ->
            0
|};
    ok "char_literal_starts_a_case_arm" {|
f: Int
f =
    case v of
        'a' ->
            1

        _ ->
            0
|};
    parses_but_owns_its_blank_lines "block_string_across_lines_keeps_the_layout" "\nf: Int\nf =\n    let\n        s = \"\"\"first\nsecond\"\"\"\n\n        y = 1\n    in\n    y\n";
    ok "let_binding_with_an_annotation" {|
f: Int
f =
    let
        y : Int
        y = 1
    in
    y
|};
    ok "let_binding_annotation_and_indented_body" {|
f: Int
f =
    let
        y : Int
        y =
            1
    in
    y
|};
    ok "two_annotated_let_bindings" {|
f: Int
f =
    let
        y : Int
        y = 1

        z : Int
        z = 2
    in
    y + z
|};
    ok "annotated_and_plain_let_bindings_mixed" {|
f: Int
f =
    let
        y = 1

        z : Int
        z = 2

        w = 3
    in
    y + z + w
|};
    ok "annotated_let_binding_with_parameters" {|
f: Int
f =
    let
        add : Int -> Int -> Int
        add a b =
            a + b
    in
    add 1 2
|};
    ok "let_annotation_across_lines" {|
f: Int
f =
    let
        add :
            Int
            -> Int
        add a =
            a
    in
    add 1
|};
    ok "annotated_let_binding_with_a_function_type" {|
f: Int
f =
    let
        apply : (Int -> Int) -> Int
        apply g =
            g 1
    in
    apply identity
|};
    ok "let_binding_annotation_does_not_leak_to_top_level" {|
f: Int
f =
    let
        y : Int
        y = 1
    in
    y

g: Int
g = 2
|};
    ok "destructuring_let_binding" {|
f: Int
f =
    let
        ( a, b ) = p
    in
    a + b
|};
    ok "destructuring_let_binding_with_an_indented_body" {|
f: Int
f =
    let
        ( a, b ) =
            p
    in
    a + b
|};
    ok "destructuring_record_let_binding" {|
f: Int
f =
    let
        { count } = model
    in
    count
|};
    ok "destructuring_and_named_let_bindings_mixed" {|
f: Int
f =
    let
        ( a, b ) = p

        c = 1
    in
    a + b + c
|};
  ]

(* -------------------------------------------------------------------------- *)
(* operators                                                                  *)
(* -------------------------------------------------------------------------- *)

let operator_tests =
  [
    ok "operators_on_one_line" {|
f: Int
f = 1 + 2 * 3 // 4 ^ 5 - 6
|};
    ok "pipelines_across_lines" {|
f: Int
f =
    5
        |> increment
        |> double
|};
    ok "apply_left_across_lines" {|
f: Int
f =
    double
        <| increment
        <| 5
|};
    ok "composition_across_lines" {|
f: Int -> Int
f =
    increment
        >> double
        >> increment
|};
    ok "operators_as_values" {|
f: Int
f = fold (+) (*) (//) (^) (++) (<<) (>>) (|>) (<|) (-) (==) (&&)
|};
    ok "pattern_parameters" {|
f: Int
f _ () ( a, b ) { count } = a
|};
    ok "pattern_parameters_in_a_lambda" {|
f: Int
f = \( a, b ) _ -> a
|};
    ok "pattern_parameters_in_a_let_binding" {|
f: Int
f =
    let
        g ( a, b ) _ =
            a
    in
    g ( 1, 2 ) 3
|};
    ok "record_update_on_one_line" {|
f: Int
f = { model | count = 1 }
|};
    ok "record_update_across_lines" {|
f: Int
f =
    { model
        | count = 1
        , name = "a"
    }
|};
    ok "record_update_of_a_qualified_record" {|
f: Int
f = { M.model | count = 1 }
|};
    ok "record_literal_still_parses" {|
f: Int
f = { count = 1, name = "a" }
|};
    ok "empty_record_still_parses" {|
f: Int
f = {}
|};
    ok "tuple_pair_and_triple" {|
f: Int
f = g ( 1, 2 ) ( 1, 2, 3 )
|};
    rejects "tuple_of_four_parts" {|
f: Int
f = ( 1, 2, 3, 4 )
|};
    ok "cons_in_an_expression" {|
f: Int
f = 1 :: 2 :: rest
|};
    ok "cons_across_lines" {|
f: Int
f =
    1
        :: 2
        :: rest
|};
    ok "operator_line_can_lead" {|
f: Int
f =
    1
        + 2
        + 3
|};
  ]

(* -------------------------------------------------------------------------- *)
(* comments                                                                   *)
(* -------------------------------------------------------------------------- *)

let comment_tests =
  [
    ok "line_comment_between_declarations" {|
-- a leading remark
f: Int
f = 1

-- another remark
g: Int
g = 2
|};
    ok "line_comment_at_the_block_column" {|
f: Int
f =
    let
        x = 1
        -- steady
        y = 2
    in
    x + y
|};
    ok "line_comment_deeper_than_the_block" {|
f: Int
f =
    let
        x = 1
                -- far to the right
        y = 2
    in
    x + y
|};
    ok "line_comment_shallower_than_the_block" {|
f: Int
f =
    let
        x = 1
  -- far to the left
        y = 2
    in
    x + y
|};
    ok "line_comment_before_a_case_arm" {|
f: Int
f =
    case v of
        -- the first shape
        A ->
            1

        -- the second shape
        B ->
            2
|};
    ok "line_comment_ends_a_case_arm_line" {|
f: Int
f =
    case v of
        A -> 1 -- one
        B -> 2 -- two
|};
    ok "line_comment_inside_a_record_literal" {|
f: Int
f =
    { a = 1
    -- the second field
    , b = 2
    }
|};
    ok "line_comment_after_the_last_token" "\nf: Int\nf = 1 -- done";
    ok "line_comment_is_the_whole_input" "-- nothing here\n";
    ok "block_comment_inline" {|
f: Int
f = {- silent -} 1
|};
    ok "block_comment_across_lines_between_bindings" {|
f: Int
f =
    let
        x = 1
        {- a longer
           remark
        -}
        y = 2
    in
    x + y
|};
    ok "block_comment_across_lines_deeper_than_the_block" {|
f: Int
f =
    let
        x = 1
              {- pushed
                     right
              -}
        y = 2
    in
    x + y
|};
    ok "nested_block_comment" {|
f: Int
f = {- outer {- inner -} still outer -} 1
|};
    ok "nested_block_comment_across_lines" {|
f: Int
f =
    let
        x = 1
        {- outer
           {- inner
           -}
        -}
        y = 2
    in
    x + y
|};
    ok "doc_block_comment" {|
{-| The module remark.
-}
f: Int
f = 1
|};
    ok "comment_markers_inside_a_string" {|
f: String
f = "-- not a comment {- either -}"
|};
    ok "block_comment_closes_a_declaration_body" {|
f: Int
f =
    1
{- between declarations
-}
g: Int
g = 2
|};
    rejects "unterminated_block_comment" {|
f: Int
f = {- never closed
|};
  ]

(* -------------------------------------------------------------------------- *)
(* inputs that must be rejected                                               *)
(* -------------------------------------------------------------------------- *)

let rejected_tests =
  [
    (* The second arm is indented deeper than the first — not a valid arm. *)
    rejects
      "misaligned_case_arms"
      {|
f: Int
f = case x of
  A -> 1
    B -> 2
|};
  ]

(* -------------------------------------------------------------------------- *)
(* known limitations                                                          *)
(* -------------------------------------------------------------------------- *)

let known_limitations =
  [
    (* Multi-line `let` with sibling bindings on their own lines. *)
    known_limitation
      "let_multibind"
      {|
b: Int
b = let x = 2
y = 3 in 42
|};
    known_limitation
      "let_multibind_three"
      {|
b: Int
b = let x = 2
y = 3
z = 4 in 42
|};
    known_limitation
      "let_with_list_and_record_binds"
      {|
e: Int
e = let xs = [1, 2, 3]
r = { a = 1, b = 2 } in 42
|};
    known_limitation
      "multiline_case_in_let_bind"
      {|
f: Int
f = let x = case v of
  A -> 1
  B -> 2
in x
|};
    (* A parenthesised, multi-line case: the closing `)` is inline, so no
       newline triggers the DEDENT that would close the arm block. *)
    known_limitation
      "parenthesised_multiline_case"
      {|
f: Int
f = (case x of
  A -> 1
  B -> 2)
|};
    (* CRLF line endings: the lexer only recognises '\n'. *)
    known_limitation "crlf_line_endings" "f: Int\r\nf = 1\r\n";
  ]

let suite =
  List.concat
    [
      let_tests;
      case_tests;
      pattern_tests;
      if_tests;
      expr_tests;
      continuation_tests;
      subexpr_tests;
      type_tests;
      toplevel_tests;
      whitespace_tests;
      let_annotation_tests;
      operator_tests;
      comment_tests;
      rejected_tests;
      known_limitations;
    ]
