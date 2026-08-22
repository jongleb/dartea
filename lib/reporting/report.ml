type t = {
  title : string;
  region : Data.Region.t;
  suggestions : string list;
  message : Doc.t;
}

let title_of_syntax (problem : Syntax_error.t) =
  match problem with
  | Unexpected_input _ -> "PARSE ERROR"
  | Unknown_character _ -> "UNKNOWN CHARACTER"
  | Unterminated _ -> "ENDLESS LITERAL"
  | Empty_character -> "EMPTY CHAR"
  | Crowded_character -> "CHAR TOO BIG"
  | Unknown_escape _ -> "UNKNOWN ESCAPE"
  | Too_many_tuple_parts _ -> "BAD TUPLE"
  | Module_name_mismatch _ -> "NAME MISMATCH"

let title_of_name (problem : Name_error.t) =
  match problem with
  | Unknown_module _ -> "MODULE NOT FOUND"
  | Not_exposed _ -> "BAD IMPORT"
  | Ctors_not_exposed _ -> "BAD IMPORT"
  | Ambiguous _ -> "AMBIGUOUS NAME"
  | Unknown_kernel _ -> "UNKNOWN KERNEL"
  | Kernel_needs_annotation _ -> "MISSING ANNOTATION"
  | Kernel_arity_mismatch _ -> "BAD KERNEL ARITY"
  | Duplicate_declaration _ -> "NAME CLASH"
  | Duplicate_binder _ -> "DUPLICATE PATTERN VARIABLE"
  | Unbound_value _ -> "NAMING ERROR"
  | Unknown_constructor _ -> "NAMING ERROR"
  | Unknown_type _ -> "NAMING ERROR"
  | Import_cycle _ -> "IMPORT CYCLE"
  | Recursive_value _ -> "CYCLIC DEFINITION"

let title_of_project (problem : Project_error.t) =
  match problem with No_sources _ -> "NO SOURCE FILES"

let title_of_type (problem : Type_error.t) =
  match problem with
  | Bad_expression { expected = From_context { context = Call_arity _; _ }; _ } ->
      "TOO MANY ARGS"
  | Bad_expression _ -> "TYPE MISMATCH"
  | Bad_pattern _ -> "TYPE MISMATCH"
  | Infinite_type _ -> "INFINITE TYPE"
  | Bad_arity { expects; given; _ } ->
      if given < expects then "TOO FEW ARGS" else "TOO MANY ARGS"
  | Case_without_branches -> "EMPTY CASE"

let title (problem : Error.problem) =
  match problem with
  | Error.Syntax problem -> title_of_syntax problem
  | Error.Name problem -> title_of_name problem
  | Error.Type problem -> title_of_type problem
  | Error.Project problem -> title_of_project problem

let quoted written = "`" ^ written ^ "`"
let indented inner = Doc.indent 4 inner

let args count =
  Printf.sprintf "%d %s" count (if count = 1 then "argument" else "arguments")

let ordinal index =
  let ending =
    match (index mod 100, index mod 10) with
    | (11 | 12 | 13), _ -> "th"
    | _, 1 -> "st"
    | _, 2 -> "nd"
    | _, 3 -> "rd"
    | _, _ -> "th"
  in
  string_of_int index ^ ending

let listed written =
  match List.rev written with
  | [] -> ""
  | [ only ] -> quoted only
  | last :: rest ->
      String.concat ", " (List.rev_map quoted rest) ^ " and " ^ quoted last

let simple_hint written = Doc.words ("Hint: " ^ written)
let note written = Doc.words ("Note: " ^ written)

let showing ~snippet headline body =
  Doc.stack [ Doc.words headline; snippet; body ]

let explaining ~snippet headline explanation =
  showing ~snippet headline (Doc.words explanation)

let mixing_hint =
  simple_hint
    "Everything in a list must be the same type of value. This way, we never run into unexpected values partway through a List.map, List.foldl, etc. Read <https://elm-lang.org/0.19.1/custom-types> to learn how to \"mix\" types."

let imports_link =
  simple_hint
    "Read <https://elm-lang.org/0.19.1/imports> to see how `import` declarations work in Elm."

let no_implicit_casts =
  note
    "Read <https://elm-lang.org/0.19.1/implicit-casts> to learn why Elm does not implicitly convert Ints to Floats."

let hint_of (problem : Hint.t) =
  match problem with
  | Int_float ->
      [
        note
          "Read <https://elm-lang.org/0.19.1/implicit-casts> to learn why Elm does not implicitly convert Ints to Floats. Use toFloat and round to do explicit conversions.";
      ]
  | String_from_int ->
      [
        simple_hint
          "Want to convert an Int into a String? Use the String.fromInt function!";
      ]
  | String_from_float ->
      [
        simple_hint
          "Want to convert a Float into a String? Use the String.fromFloat function!";
      ]
  | String_to_int ->
      [
        simple_hint
          "Want to convert a String into an Int? Use the String.toInt function!";
      ]
  | String_to_float ->
      [
        simple_hint
          "Want to convert a String into a Float? Use the String.toFloat function!";
      ]
  | Anything_to_bool ->
      [
        simple_hint
          "Elm does not have \"truthiness\" such that ints and strings and lists are automatically converted to booleans. Do that conversion explicitly!";
      ]
  | Anything_from_maybe ->
      [
        simple_hint
          "Use Maybe.withDefault to handle possible errors. Longer term, it is usually better to write out the full `case` though!";
      ]
  | Arity_mismatch { found; expected } ->
      [
        simple_hint
          (if found < expected then
             Printf.sprintf
               "It looks like it takes too few arguments. I was expecting %d more."
               (expected - found)
           else
             Printf.sprintf
               "It looks like it takes too many arguments. I see %d extra."
               (found - expected));
      ]
  | Bad_flex_super { direction; required; found } -> begin
      match (required : Data.Constraint.t) with
      | Comparable -> begin
          match Typed.Type.head found with
          | TRecord _ | TRowExtend _ | TRowEmpty ->
              [
                simple_hint
                  "I do not know how to compare records. I can only compare ints, floats, chars, strings, lists of comparable values, and tuples of comparable values. Check out <https://elm-lang.org/0.19.1/comparing-records> for ideas on how to proceed.";
              ]
          | TCustom (name, _) ->
              [
                simple_hint
                  (Printf.sprintf
                     "I do not know how to compare %s values. I can only compare ints, floats, chars, strings, lists of comparable values, and tuples of comparable values."
                     (quoted (Data.Name.base name)));
                Doc.words
                  "Check out <https://elm-lang.org/0.19.1/comparing-custom-types> for ideas on how to proceed.";
              ]
          | TVar _ | TInt | TFloat | TChar | TBool | TStr | TUnit | TFun _
          | TTup _ ->
              [
                simple_hint
                  "I only know how to compare ints, floats, chars, strings, lists of comparable values, and tuples of comparable values.";
              ]
        end
      | Appendable -> [ simple_hint "I only know how to append strings and lists." ]
      | Comp_appendable ->
          [ simple_hint "Only strings and lists are both comparable and appendable." ]
      | Number -> begin
          match (Typed.Type.head found, direction) with
          | TStr, Have ->
              [ simple_hint "Try using String.fromInt to convert it to a string?" ]
          | TStr, Need ->
              [ simple_hint "Try using String.toInt to convert it to an integer?" ]
          | ( ( TVar _ | TInt | TFloat | TChar | TBool | TUnit | TFun _
              | TTup _ | TCustom _ | TRecord _ | TRowExtend _ | TRowEmpty ),
              (Have | Need) ) ->
              [ simple_hint "Only Int and Float values work as numbers." ]
        end
    end
  | Fields_missing fields -> begin
      match fields with
      | [] -> []
      | [ only ] ->
          [
            simple_hint
              (Printf.sprintf "Looks like the %s field is missing." (quoted only));
          ]
      | _ ->
          [
            simple_hint
              (Printf.sprintf "Looks like fields %s are missing." (listed fields));
          ]
    end
  | Field_typo { typo; possibilities } -> begin
      match Suggest.nearest ~target:typo possibilities with
      | [] -> []
      | nearest :: _ ->
          [
            simple_hint
              (Printf.sprintf
                 "Seems like a record field typo. Maybe %s should be %s?"
                 (quoted typo) (quoted nearest));
          ]
    end

let hints problems =
  match problems with [] -> [] | problem :: _ -> hint_of problem

let named (callee : Category.maybe_name) =
  match callee with
  | No_name -> "this function"
  | Func_name name | Ctor_name name -> quoted (Data.Name.base name)
  | Op_name name -> "(" ^ Data.Name.base name ^ ")"

let this_value (callee : Category.maybe_name) =
  match callee with
  | No_name -> "This value"
  | Func_name name | Ctor_name name ->
      Printf.sprintf "The %s value" (quoted (Data.Name.base name))
  | Op_name name -> Printf.sprintf "The (%s) operator" (Data.Name.base name)

let this_function (callee : Category.maybe_name) =
  match callee with
  | No_name -> "This function"
  | Func_name name ->
      Printf.sprintf "The %s function" (quoted (Data.Name.base name))
  | Ctor_name name ->
      Printf.sprintf "The %s constructor" (quoted (Data.Name.base name))
  | Op_name name -> Printf.sprintf "The (%s) operator" (Data.Name.base name)

let with_category this_is (category : Category.t) =
  match category with
  | Local name | Foreign name ->
      Printf.sprintf "This %s value is a:" (quoted (Data.Name.base name))
  | Access field -> Printf.sprintf "The value at .%s is a:" field
  | Accessor field ->
      Printf.sprintf "This .%s field access function has type:" field
  | If -> "This `if` expression produces:"
  | Case -> "This `case` expression produces:"
  | List -> this_is ^ " a list of type:"
  | Number -> this_is ^ " a number of type:"
  | Float -> this_is ^ " a float of type:"
  | String -> this_is ^ " a string of type:"
  | Char -> this_is ^ " a character of type:"
  | Lambda -> this_is ^ " an anonymous function of type:"
  | Record -> this_is ^ " a record of type:"
  | Tuple -> this_is ^ " a tuple of type:"
  | Unit -> this_is ^ " a unit value:"
  | Call_result callee -> begin
      match callee with
      | No_name | Op_name _ -> this_is ^ ":"
      | Func_name name | Ctor_name name ->
          Printf.sprintf "This %s call produces:" (quoted (Data.Name.base name))
    end

let with_pattern_category trying_to_match (category : Category.pattern) =
  trying_to_match
  ^
  match category with
  | P_record -> " record values of type:"
  | P_unit -> " unit values:"
  | P_tuple -> " tuples of type:"
  | P_list -> " lists of type:"
  | P_ctor name ->
      Printf.sprintf " %s values of type:" (quoted (Data.Name.base name))
  | P_int -> " integers:"
  | P_str -> " strings:"
  | P_chr -> " characters:"

let comparison naming ~found ~expected ~i_am_seeing ~instead_of ~details =
  let found_doc, expected_doc, problems =
    Message.comparison naming ~found ~expected
  in
  Doc.stack
    ([
       Doc.words i_am_seeing;
       indented found_doc;
       Doc.words instead_of;
       indented expected_doc;
     ]
    @ details @ hints problems)

let lone naming ~found ~expected ~i_am_seeing ~details =
  let found_doc, _, problems = Message.comparison naming ~found ~expected in
  Doc.stack
    ([ Doc.words i_am_seeing; indented found_doc ] @ details @ hints problems)

let mismatching naming ~snippet ~found ~seeing ~problem ~this_is ~instead_of
    ~details expected =
  showing ~snippet problem
    (comparison naming ~found ~expected ~i_am_seeing:(seeing this_is)
       ~instead_of ~details)

let is_string ty =
  match Typed.Type.head ty with TStr -> true | _ -> false

let is_int ty = match Typed.Type.head ty with TInt -> true | _ -> false
let is_float ty = match Typed.Type.head ty with TFloat -> true | _ -> false

let number_named ty =
  match Typed.Type.head ty with
  | TInt -> Some ("Int", "String.fromInt")
  | TFloat -> Some ("Float", "String.fromFloat")
  | TVar variable -> begin
      match Typed.Variable.constraint_of variable with
      | Some Number -> Some ("number", "String.fromInt")
      | Some (Comparable | Appendable | Comp_appendable) | None -> None
    end
  | TChar | TBool | TStr | TUnit | TFun _ | TTup _ | TCustom _ | TRecord _
  | TRowExtend _ | TRowEmpty ->
      None

let of_operator naming ~snippet ~category ~found ~expected ~side ~operator =
  let written = Data.Name.base operator in
  let problem_and_body = showing ~snippet in
  let lone_with ~i_am_seeing ~details =
    lone naming ~found ~expected ~i_am_seeing ~details
  in
  let this_side =
    with_category
      (Printf.sprintf "The %s side of (%s) is" side written)
      category
  in
  let bad_math operation =
    problem_and_body
      (operation ^ " does not work with this value:")
      (lone_with ~i_am_seeing:this_side
         ~details:
           [
             Doc.words
               (Printf.sprintf
                  "But (%s) only works with Int and Float values." written);
           ])
  in
  let bad_comparison () =
    problem_and_body "I cannot do a comparison with this value:"
      (lone_with ~i_am_seeing:this_side
         ~details:
           [
             Doc.words
               (Printf.sprintf
                  "But (%s) only works on Int, Float, Char, and String values. \
                   It can work on lists and tuples of comparable values as \
                   well, but it is usually better to find a different path."
                  written);
           ])
  in
  let bad_bool () =
    problem_and_body "I am struggling with this boolean operation:"
      (lone_with
         ~i_am_seeing:
           (Printf.sprintf
              "Both sides of (%s) must be Bool values, but the %s side is:"
              written side)
         ~details:[])
  in
  let bad_division ~kind ~needs ~mistaken ~advice ~examples =
    problem_and_body
      (Printf.sprintf "The (%s) operator is specifically for %s division:"
         written kind)
      (if mistaken then
         Doc.stack
           [
             Doc.words advice;
             Doc.above
               (List.map (fun line -> indented (Doc.text line)) examples);
             no_implicit_casts;
           ]
       else
         lone_with
           ~i_am_seeing:
             (Printf.sprintf
                "The %s side of (%s) must be %s, but instead I am seeing:" side
                written needs)
           ~details:[])
  in
  let bad_float_division () =
    bad_division ~kind:"floating-point" ~needs:"a Float"
      ~mistaken:(is_int found)
      ~advice:
        (Printf.sprintf
           "The %s side of (/) must be a Float, but I am seeing an Int. I recommend:"
           side)
      ~examples:
        [
          "toFloat for explicit conversions     (toFloat 5 / 2) == 2.5";
          "(//)    for integer division         (5 // 2)        == 2";
        ]
  in
  let bad_integer_division () =
    bad_division ~kind:"integer" ~needs:"an Int" ~mistaken:(is_float found)
      ~advice:
        (Printf.sprintf
           "The %s side of (//) must be an Int, but I am seeing a Float. I recommend doing the conversion explicitly with one of these functions:"
           side)
      ~examples:
        [
          "round 3.5     == 4";
          "floor 3.5     == 3";
          "ceiling 3.5   == 4";
          "truncate 3.5  == 3";
        ]
  in
  let bad_append () =
    match number_named found with
    | Some (thing, conversion) ->
        problem_and_body
          (Printf.sprintf
             "The (++) operator can append List and String values, but not %s \
              values like this:"
             thing)
          (Doc.words
             (Printf.sprintf
                "Try using %s to turn it into a string? Or put it in [] to \
                 make it a list? Or switch to the (::) operator?"
                conversion))
    | None ->
        problem_and_body "The (++) operator cannot append this type of value:"
          (lone_with
             ~i_am_seeing:(with_category "I am seeing" category)
             ~details:
               [
                 Doc.words
                   "But the (++) operator is only for appending List and \
                    String values. Maybe put this value in [] to make it a \
                    list?";
               ])
  in
  let both_sides ~note_about =
    problem_and_body
      (Printf.sprintf "I need both sides of (%s) to be the same type:" written)
      (comparison naming ~found:expected ~expected:found
         ~i_am_seeing:(Printf.sprintf "The left side of (%s) is:" written)
         ~instead_of:"But the right side is:" ~details:[ Doc.words note_about ])
  in
  match (written, side) with
  | "+", _ when is_string found ->
      problem_and_body
        "I cannot do addition with String values like this one:"
        (Doc.stack
           [
             Doc.words "The (+) operator only works with Int and Float values.";
             simple_hint "Switch to the (++) operator to append strings!";
           ])
  | "+", _ -> bad_math "Addition"
  | "-", _ -> bad_math "Subtraction"
  | "*", _ -> bad_math "Multiplication"
  | "^", _ -> bad_math "Exponentiation"
  | "/", _ -> bad_float_division ()
  | "//", _ -> bad_integer_division ()
  | ("&&" | "||"), _ -> bad_bool ()
  | ("<" | ">" | "<=" | ">="), "left" -> bad_comparison ()
  | ("<" | ">" | "<=" | ">="), _ ->
      both_sides
        ~note_about:
          (Printf.sprintf
             "I cannot compare different types though! Which side of (%s) is \
              the problem?"
             written)
  | ("==" | "/="), _ ->
      if is_float found || is_float expected then
        both_sides
          ~note_about:
            "Note: Equality on floats is not 100% reliable due to the design \
             of IEEE 754. I recommend a check like (abs (x - y) < 0.0001) \
             instead."
      else
        both_sides
          ~note_about:
            "Different types can never be equal though! Which side is messed up?"
  | "++", _ -> bad_append ()
  | _, _ ->
      problem_and_body
        (Printf.sprintf "The %s argument of (%s) is causing problems:" side
           written)
        (comparison naming ~found ~expected
           ~i_am_seeing:
             (with_category (Printf.sprintf "The %s argument is" side) category)
           ~instead_of:
             (Printf.sprintf "But (%s) needs the %s argument to be:" written
                side)
           ~details:[])

let of_expression source region category found (expected : Expectation.t) =
  let naming = Message.naming () in
  let snippet = Snippet.of_region source region in
  let seeing this_is = with_category this_is category in
  let mismatch = mismatching naming ~snippet ~found ~seeing in
  let bad_type ~problem ~this_is ~details expected =
    showing ~snippet problem
      (lone naming ~found ~expected ~i_am_seeing:(seeing this_is) ~details)
  in
  match expected with
  | No_expectation expected ->
      mismatch expected
        ~problem:"This expression is being used in an unexpected way:"
        ~this_is:"It is" ~instead_of:"But you are trying to use it as:"
        ~details:[]
  | From_annotation { name; sub; expected } ->
      let thing =
        match sub with
        | Typed_body -> Printf.sprintf "body of the %s definition:" (quoted name)
      in
      let this_is = match sub with Typed_body -> "The body is" in
      mismatch expected
        ~problem:("Something is off with the " ^ thing)
        ~this_is
        ~instead_of:
          (Printf.sprintf "But the type annotation on %s says it should be:"
             (quoted name))
        ~details:[]
  | From_context { context; expected } -> begin
      match context with
      | List_entry index ->
          let ith = ordinal index in
          mismatch expected
            ~problem:
              ("The " ^ ith
             ^ " element of this list does not match all the previous elements:")
            ~this_is:("The " ^ ith ^ " element is")
            ~instead_of:"But all the previous elements in the list are:"
            ~details:[ mixing_hint ]
      | Negate ->
          bad_type expected
            ~problem:"I do not know how to negate this type of value:"
            ~this_is:"It is"
            ~details:
              [ Doc.words "But I only now how to negate Int and Float values." ]
      | Op_left operator ->
          of_operator naming ~snippet ~category ~found ~expected ~side:"left"
            ~operator
      | Op_right operator ->
          of_operator naming ~snippet ~category ~found ~expected ~side:"right"
            ~operator
      | If_condition ->
          bad_type expected
            ~problem:
              "This `if` condition does not evaluate to a boolean value, True or False."
            ~this_is:"It is"
            ~details:
              [ Doc.words "But I need this `if` condition to be a Bool value." ]
      | If_branch index ->
          let ith = ordinal index in
          mismatch expected
            ~problem:
              ("The " ^ ith
             ^ " branch of this `if` does not match all the previous branches:")
            ~this_is:("The " ^ ith ^ " branch is")
            ~instead_of:"But all the previous branches result in:"
            ~details:
              [
                simple_hint
                  "All branches in an `if` must produce the same type of values. This way, no matter which branch we take, the result is always a consistent shape. Read <https://elm-lang.org/0.19.1/custom-types> to learn how to \"mix\" types.";
              ]
      | Case_branch index ->
          let ith = ordinal index in
          mismatch expected
            ~problem:
              ("The " ^ ith
             ^ " branch of this `case` does not match all the previous branches:")
            ~this_is:("The " ^ ith ^ " branch is")
            ~instead_of:"But all the previous branches result in:"
            ~details:
              [
                simple_hint
                  "All branches in a `case` must produce the same type of values. This way, no matter which branch we take, the result is always a consistent shape. Read <https://elm-lang.org/0.19.1/custom-types> to learn how to \"mix\" types.";
              ]
      | Call_arity { callee; given } ->
          let takes = Typed.Type.arrows found in
          Doc.stack
            [
              Doc.words
                (if takes = 0 then
                   Printf.sprintf "%s is not a function, but it was given %s."
                     (this_value callee) (args given)
                 else
                   Printf.sprintf "%s expects %s, but it got %d instead."
                     (this_function callee) (args takes) given);
              snippet;
              Doc.words "Are there any missing commas? Or missing parentheses?";
            ]
      | Call_arg { callee; index } ->
          let ith = ordinal index in
          mismatch expected
            ~problem:
              (Printf.sprintf "The %s argument to %s is not what I expect:" ith
                 (named callee))
            ~this_is:"This argument is"
            ~instead_of:
              (Printf.sprintf "But %s needs the %s argument to be:"
                 (named callee) ith)
            ~details:
              (if index = 1 then []
               else
                 [
                   simple_hint
                     "I always figure out the argument types from left to right. If an argument is acceptable, I assume it is \"correct\" and move on. So the problem may actually be in one of the previous arguments!";
                 ])
      | Record_access { field } ->
          bad_type expected
            ~problem:"This is not a record, so it has no fields to access!"
            ~this_is:"It is"
            ~details:
              [
                Doc.words
                  (Printf.sprintf "But I need a record with a %s field!" field);
              ]
      | Record_update_value field ->
          mismatch expected
            ~problem:
              (Printf.sprintf "I cannot update the %s field like this:"
                 (quoted field))
            ~this_is:"You are trying to update it to be"
            ~instead_of:"But it should be:" ~details:[]
    end

let of_pattern source region category found (expected : Expectation.pattern) =
  let naming = Message.naming () in
  let snippet = Snippet.of_region source region in
  let seeing this_is = with_pattern_category this_is category in
  let mismatch = mismatching naming ~snippet ~found ~seeing in
  match expected with
  | Pattern_no_expectation expected ->
      mismatch expected
        ~problem:"This pattern is being used in an unexpected way:"
        ~this_is:"It is" ~instead_of:"But it needs to match:" ~details:[]
  | Pattern_from_context { context; expected } -> begin
      match context with
      | P_case_match index ->
          if index = 1 then
            mismatch expected
              ~problem:"The 1st pattern in this `case` causing a mismatch:"
              ~this_is:"The first pattern is trying to match"
              ~instead_of:"But the expression between `case` and `of` is:"
              ~details:
                [
                  Doc.words
                    "These can never match! Is the pattern the problem? Or is it the expression?";
                ]
          else
            let ith = ordinal index in
            mismatch expected
              ~problem:
                (Printf.sprintf
                   "The %s pattern in this `case` does not match the previous ones."
                   ith)
              ~this_is:("The " ^ ith ^ " pattern is trying to match")
              ~instead_of:"But all the previous patterns match:"
              ~details:
                [
                  note
                    "A `case` expression can only handle one type of value, so you may want to use <https://elm-lang.org/0.19.1/custom-types> to handle \"mixing\" types.";
                ]
      | P_ctor_arg { name; index } ->
          let ith = ordinal index in
          mismatch expected
            ~problem:
              (Printf.sprintf "The %s argument to %s is weird." ith
                 (quoted (Data.Name.base name)))
            ~this_is:"It is trying to match"
            ~instead_of:
              (Printf.sprintf "But %s needs its %s argument to be:"
                 (quoted (Data.Name.base name))
                 ith)
            ~details:[]
      | P_list_entry index ->
          let ith = ordinal index in
          mismatch expected
            ~problem:
              (Printf.sprintf
                 "The %s pattern in this list does not match all the previous ones:"
                 ith)
            ~this_is:("The " ^ ith ^ " pattern is trying to match")
            ~instead_of:"But all the previous patterns in the list are:"
            ~details:[ mixing_hint ]
      | P_tail ->
          mismatch expected ~problem:"The pattern after (::) is causing issues."
            ~this_is:"The pattern after (::) is trying to match"
            ~instead_of:"But it needs to match lists like this:" ~details:[]
    end

let of_type_problem source region (problem : Type_error.t) =
  let snippet = Snippet.of_region source region in
  let explaining = explaining ~snippet in
  match problem with
  | Bad_expression { category; found; expected } ->
      of_expression source region category found expected
  | Bad_pattern { category; found; expected } ->
      of_pattern source region category found expected
  | Infinite_type { category; found } ->
      let naming = Message.naming () in
      Doc.stack
        [
          Doc.words
            begin
              match category with
              | Category.Local name | Category.Foreign name ->
                  Printf.sprintf
                    "I am inferring a weird self-referential type for %s:"
                    (quoted (Data.Name.base name))
              | List | Number | Float | String | Char | If | Case
              | Call_result _ | Lambda | Accessor _ | Access _ | Record | Tuple
              | Unit ->
                  "I am inferring a weird self-referential type here:"
            end;
          snippet;
          Doc.words
            "Here is my best effort at writing down the type. You will see ? for parts of the type that repeat something already printed out infinitely.";
          indented (Message.alone naming found);
          simple_hint
            "The problem is often in the most recently added pattern or function. Try commenting out that code to see if it helps.";
        ]
  | Bad_arity { thing; name; expects; given } ->
      let what = match thing with A_type -> "type" | A_variant -> "variant" in
      explaining
        (Printf.sprintf "The %s %s needs %s, but I see %d instead:"
           (quoted (Data.Name.base name))
           what (args expects) given)
        (if given < expects then
           "What is missing? Are some parentheses misplaced?"
         else if given - expects = 1 then
           "Which is the extra one? Maybe some parentheses are missing?"
         else "Which are the extra ones? Maybe some parentheses are missing?")
  | Case_without_branches ->
      explaining "This `case` does not have any branches:"
        "A `case` needs at least one branch, so I know what to do with the value it is given."

let highlighted written =
  Doc.above
    (List.map (fun text -> indented (Doc.yellow (Doc.text text))) written)

let closest heading written =
  Doc.above [ Doc.words heading; Doc.blank; highlighted written ]

let cycle written =
  indented (Doc.text (String.concat " -> " (written @ [ List.hd written ])))

let cannot_find ~snippet ~what ~name ~bare ~prefix ~near =
  let without, with_names =
    match (prefix : Name_error.prefix) with
    | No_prefix ->
        ( "Is there an `import` or `exposing` missing up top?",
          "These names seem close though:" )
    | Unknown_prefix qualifier ->
        ( Printf.sprintf "I cannot find a %s module. Is there an `import` for it?"
            (quoted qualifier),
          Printf.sprintf
            "I cannot find a %s import. These names seem close though:"
            (quoted qualifier) )
    | Known_prefix qualifier ->
        let missing =
          Printf.sprintf "The %s module does not expose a %s %s."
            (quoted qualifier) (quoted bare) what
        in
        (missing, missing ^ " These names seem close though:")
  in
  Doc.stack
    ([
       Doc.words (Printf.sprintf "I cannot find a %s %s:" (quoted name) what);
       snippet;
     ]
    @ begin
        match near with
        | [] -> [ Doc.words without ]
        | _ -> [ closest with_names near ]
      end
    @ [ imports_link ])

let of_name_problem source region (problem : Name_error.t) =
  let snippet = Snippet.of_region source region in
  let explaining = explaining ~snippet in
  let showing = showing ~snippet in
  match problem with
  | Unknown_module { qualifier; near } ->
      Doc.stack
        [
          Doc.words
            (Printf.sprintf "You are trying to import a %s module:"
               (quoted qualifier));
          snippet;
          begin
            match near with
            | [] -> Doc.words "I cannot find it anywhere in this project!"
            | _ ->
                closest
                  "I looked through the modules of this project, but I cannot find it! Maybe it is a typo for one of these names?"
                  near
          end;
          imports_link;
        ]
  | Not_exposed { module_name; name; near } ->
      showing
        (Printf.sprintf "The %s module does not expose %s:"
           (quoted module_name) (quoted name))
        begin
          match near with
          | [] ->
              Doc.words
                "I cannot find any super similar exposed names. Maybe it is private?"
          | [ only ] ->
              Doc.words
                (Printf.sprintf "Maybe you want %s instead?" (quoted only))
          | _ -> closest "These names seem close though:" near
        end
  | Ctors_not_exposed { module_name; type_name } ->
      explaining
        (Printf.sprintf "The %s module does not expose the constructors of %s:"
           (quoted module_name) (quoted type_name))
        (Printf.sprintf
           "It would have to say `exposing (%s(..))` for them to be available here."
           type_name)
  | Ambiguous { name; modules } ->
      Doc.stack
        [
          Doc.words
            (Printf.sprintf "This usage of %s is ambiguous:" (quoted name));
          snippet;
          Doc.words
            (Printf.sprintf "It could refer to a value from %s."
               (listed modules));
          Doc.words
            "Whatever it is, use a qualified name to say which one you want.";
        ]
  | Unknown_kernel { module_name; exported_name } ->
      explaining
        (Printf.sprintf "There is no %s kernel value:"
           (quoted (module_name ^ "." ^ exported_name)))
        "Kernel values are the primitives the compiler knows about, and this is not one of them."
  | Kernel_needs_annotation { name } ->
      explaining
        (Printf.sprintf
           "The %s definition has a kernel body, so it needs a type annotation:"
           (quoted name))
        "A kernel value takes its type from the annotation alone, so I cannot work it out without one."
  | Kernel_arity_mismatch { declared; kernel } ->
      explaining
        (Printf.sprintf
           "This annotation takes %s, but the kernel value it names takes %s:"
           (args declared) (args kernel))
        "The two have to agree."
  | Duplicate_declaration { name } ->
      explaining
        (Printf.sprintf "This file has multiple %s declarations." (quoted name))
        "How can I know which one you want? Rename one of them!"
  | Duplicate_binder { name } ->
      showing
        (Printf.sprintf "This pattern uses %s more than once:" (quoted name))
        (simple_hint
           "Rename one of them to make it clear which one you want. Elm does not allow a name to be defined twice.")
  | Unbound_value { name; prefix; near } ->
      cannot_find ~snippet ~what:"variable" ~name:(Data.Name.to_string name)
        ~bare:(Data.Name.base name) ~prefix ~near
  | Unknown_constructor { name; prefix; near } ->
      cannot_find ~snippet ~what:"variant" ~name:(Data.Name.to_string name)
        ~bare:(Data.Name.base name) ~prefix ~near
  | Unknown_type { name; prefix; near } ->
      cannot_find ~snippet ~what:"type" ~name:(Data.Name.to_string name)
        ~bare:(Data.Name.base name) ~prefix ~near
  | Import_cycle { modules } ->
      Doc.stack
        [
          Doc.words "Your module imports form a cycle:";
          snippet;
          cycle modules;
          Doc.words
            "Learn more about why this is not allowed at <https://elm-lang.org/0.19.1/import-cycles>.";
        ]
  | Recursive_value { names } ->
      Doc.stack
        [
          Doc.words "This definition is causing an infinite loop:";
          snippet;
          cycle names;
          Doc.words
            "The definition depends on itself, so I cannot compute its value. Functions may call each other, but values may not.";
        ]

let of_syntax_problem source region (problem : Syntax_error.t) =
  let snippet = Snippet.of_region source region in
  let explaining = explaining ~snippet in
  match problem with
  | Unexpected_input { found } ->
      explaining "I got stuck here:"
        (Printf.sprintf "I was not expecting %s at this point." (quoted found))
  | Unknown_character { found } ->
      explaining
        (Printf.sprintf "I do not know what %s means here:" (quoted found))
        "This character cannot start anything I know of."
  | Unterminated { what } ->
      explaining
        (Printf.sprintf
           "I got to the end of the file without seeing the end of this %s:"
           (Syntax_error.what_is_unterminated what))
        "Add the closing mark, or delete it and start again."
  | Empty_character ->
      explaining "I thought I was parsing a character, but I got stuck here:"
        "Characters look like 'c' and hold exactly one character. For text, use double quotes instead."
  | Crowded_character ->
      explaining "This character literal holds more than one character:"
        "Characters hold exactly one character. Use double quotes for text of any length."
  | Unknown_escape { found } ->
      explaining
        (Printf.sprintf "I do not know the escape %s:" (quoted ("\\" ^ found)))
        "The escapes I know are \\n, \\t, \\r, \\b, \\\", \\', \\\\ and \\u{...}."
  | Too_many_tuple_parts { given } ->
      explaining
        (Printf.sprintf
           "I only accept tuples of two or three parts, but this one has %d:"
           given)
        "Switch to a record. Records can hold as many values as you need, and each one has a name."
  | Module_name_mismatch { expected } ->
      explaining "It looks like this module name is out of sync:"
        (Printf.sprintf
           "I need it to match the file path, so I was expecting to see %s here. Usually a mismatch like this means the file was moved or renamed. Change the name to match the path, or move the file to match the name."
           (quoted expected))

let suggestions_of (problem : Error.problem) =
  match problem with
  | Error.Name (Unbound_value { near; _ })
  | Error.Name (Unknown_constructor { near; _ })
  | Error.Name (Unknown_type { near; _ })
  | Error.Name (Unknown_module { near; _ })
  | Error.Name (Not_exposed { near; _ }) ->
      near
  | Error.Name
      ( Ctors_not_exposed _ | Ambiguous _ | Unknown_kernel _
      | Kernel_needs_annotation _ | Kernel_arity_mismatch _
      | Duplicate_declaration _ | Duplicate_binder _ | Import_cycle _
      | Recursive_value _ )
  | Error.Type _ | Error.Syntax _ | Error.Project _ ->
      []

let rec drawn ~inside (pattern : Typed.Pattern.t) =
  match pattern.pattern with
  | P_T_anything | P_T_var _ -> "_"
  | P_T_unit -> "()"
  | P_T_int value -> string_of_int value
  | P_T_str text -> "\"" ^ text ^ "\""
  | P_T_chr letter -> "'" ^ letter ^ "'"
  | P_T_record fields -> "{ " ^ String.concat ", " fields ^ " }"
  | P_T_alias (aliased, _) -> drawn ~inside aliased
  | P_T_tuple items ->
      "( " ^ String.concat ", " (List.map (drawn ~inside:false) items) ^ " )"
  | P_T_list items ->
      "[" ^ String.concat "," (List.map (drawn ~inside:false) items) ^ "]"
  | P_T_cons (head, tail) ->
      let written =
        drawn ~inside:true head ^ " :: " ^ drawn ~inside:false tail
      in
      if inside then "(" ^ written ^ ")" else written
  | P_T_ctor (name, []) -> Data.Name.base name
  | P_T_ctor (name, arguments) ->
      let written =
        Data.Name.base name ^ " "
        ^ String.concat " " (List.map (drawn ~inside:true) arguments)
      in
      if inside then "(" ^ written ^ ")" else written

let of_warning source (warning : Warning.t) =
  let snippet = Snippet.of_region source warning.region in
  let reported title message =
    { title; region = warning.region; suggestions = []; message }
  in
  match warning.problem with
  | Missing_patterns { unhandled } ->
      reported "MISSING PATTERNS"
        (Doc.stack
           [
             Doc.words
               "This `case` does not have branches for all possibilities:";
             snippet;
             closest "Missing possibilities include:"
               (List.map (drawn ~inside:false) unhandled);
             Doc.words
               "I would have to crash if I saw one of those. Add branches for them!";
           ])
  | Redundant_pattern { index } ->
      reported "REDUNDANT PATTERN"
        (explaining ~snippet
           (Printf.sprintf "The %s pattern is redundant:" (ordinal index))
           "Any value with this shape will be handled by a previous pattern, so it should be removed.")

let extension = ".elm"

let of_project_problem (problem : Project_error.t) =
  match problem with
  | No_sources { folder } ->
      Doc.stack
        [
          Doc.words
            (Printf.sprintf
               "I looked for %s files in %s and every folder inside it, but I found none."
               (quoted extension) (quoted folder));
          Doc.words
            "Point me at the folder that holds your source files, and I will start from there.";
        ]

let of_error source (error : Error.t) =
  let message =
    match error.problem with
    | Error.Syntax problem -> of_syntax_problem source error.region problem
    | Error.Name problem -> of_name_problem source error.region problem
    | Error.Type problem -> of_type_problem source error.region problem
    | Error.Project problem -> of_project_problem problem
  in
  {
    title = title error.problem;
    region = error.region;
    suggestions = suggestions_of error.problem;
    message;
  }

let width = 80

let bar ~title ~path =
  let taken = 4 + String.length title + 1 + String.length path in
  Doc.cyan
    (Doc.text
       ("-- " ^ title ^ " " ^ String.make (max 1 (width - taken)) '-' ^ " " ^ path))

let to_string ~colours report =
  Doc.to_string ~colours
    (Doc.above
       [
         bar ~title:report.title ~path:report.region.file;
         Doc.blank;
         report.message;
         Doc.blank;
       ])
