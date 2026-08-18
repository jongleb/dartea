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
  match written with
  | [] -> ""
  | [ only ] -> quoted only
  | _ ->
      let rest =
        List.filteri (fun index _ -> index < List.length written - 1) written
      in
      String.concat ", " (List.map quoted rest)
      ^ " and "
      ^ quoted (List.nth written (List.length written - 1))

let simple_hint written = Doc.words ("Hint: " ^ written)
let note written = Doc.words ("Note: " ^ written)

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
          | _, _ -> [ simple_hint "Only Int and Float values work as numbers." ]
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
  | P_bool -> " booleans:"

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

let arrow_count ty =
  let rec count found ty =
    match Typed.Type.head ty with
    | TFun (_, result) -> count (found + 1) result
    | TVar _ | TInt | TFloat | TChar | TBool | TStr | TUnit | TTup _
    | TCustom _ | TRecord _ | TRowExtend _ | TRowEmpty ->
        found
  in
  count 0 ty

let of_expression source region category found (expected : Expectation.t) =
  let naming = Message.naming () in
  let snippet = Snippet.of_region source region in
  let mismatch ~problem ~this_is ~instead_of ~details expected =
    Doc.stack
      [
        Doc.words problem;
        snippet;
        comparison naming ~found ~expected
          ~i_am_seeing:(with_category this_is category)
          ~instead_of ~details;
      ]
  in
  let bad_type ~problem ~this_is ~details expected =
    Doc.stack
      [
        Doc.words problem;
        snippet;
        lone naming ~found ~expected
          ~i_am_seeing:(with_category this_is category)
          ~details;
      ]
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
        | Typed_if_branch index ->
            ordinal index ^ " branch of this `if` expression:"
        | Typed_case_branch index ->
            ordinal index ^ " branch of this `case` expression:"
        | Typed_body -> Printf.sprintf "body of the %s definition:" (quoted name)
      in
      let this_is =
        match sub with
        | Typed_if_branch index | Typed_case_branch index ->
            "The " ^ ordinal index ^ " branch is"
        | Typed_body -> "The body is"
      in
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
            ~details:
              [
                simple_hint
                  "Everything in a list must be the same type of value. This way, we never run into unexpected values partway through a List.map, List.foldl, etc. Read <https://elm-lang.org/0.19.1/custom-types> to learn how to \"mix\" types.";
              ]
      | Negate ->
          bad_type expected
            ~problem:"I do not know how to negate this type of value:"
            ~this_is:"It is"
            ~details:
              [ Doc.words "But I only now how to negate Int and Float values." ]
      | Op_left operator ->
          mismatch expected
            ~problem:
              (Printf.sprintf "The left argument of (%s) is causing problems:"
                 (Data.Name.base operator))
            ~this_is:"The left argument is"
            ~instead_of:
              (Printf.sprintf "But (%s) needs the left argument to be:"
                 (Data.Name.base operator))
            ~details:[]
      | Op_right operator ->
          mismatch expected
            ~problem:
              (Printf.sprintf "The right argument of (%s) is causing problems:"
                 (Data.Name.base operator))
            ~this_is:"The right argument is"
            ~instead_of:
              (Printf.sprintf "But (%s) needs the right argument to be:"
                 (Data.Name.base operator))
            ~details:[]
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
          let takes = arrow_count found in
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
      | Destructure ->
          mismatch expected ~problem:"This definition is causing issues:"
            ~this_is:"You are defining"
            ~instead_of:"But then trying to destructure it as:" ~details:[]
    end

let of_pattern source region category found (expected : Expectation.pattern) =
  let naming = Message.naming () in
  let snippet = Snippet.of_region source region in
  let mismatch ~problem ~this_is ~instead_of ~details expected =
    Doc.stack
      [
        Doc.words problem;
        snippet;
        comparison naming ~found ~expected
          ~i_am_seeing:(with_pattern_category this_is category)
          ~instead_of ~details;
      ]
  in
  match expected with
  | Pattern_no_expectation expected ->
      mismatch expected
        ~problem:"This pattern is being used in an unexpected way:"
        ~this_is:"It is" ~instead_of:"But it needs to match:" ~details:[]
  | Pattern_from_context { context; expected } -> begin
      match context with
      | P_typed_arg { name; index } ->
          let ith = ordinal index in
          mismatch expected
            ~problem:
              (Printf.sprintf "The %s argument to %s is weird." ith
                 (quoted name))
            ~this_is:"The argument is a pattern that matches"
            ~instead_of:
              (Printf.sprintf
                 "But the type annotation on %s says the %s argument should be:"
                 (quoted name) ith)
            ~details:[]
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
            ~details:
              [
                simple_hint
                  "Everything in a list must be the same type of value. This way, we never run into unexpected values partway through a List.map, List.foldl, etc. Read <https://elm-lang.org/0.19.1/custom-types> to learn how to \"mix\" types.";
              ]
      | P_tail ->
          mismatch expected ~problem:"The pattern after (::) is causing issues."
            ~this_is:"The pattern after (::) is trying to match"
            ~instead_of:"But it needs to match lists like this:" ~details:[]
    end

let of_type_problem source region (problem : Type_error.t) =
  let snippet = Snippet.of_region source region in
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
            (match category with
            | Category.Local name | Category.Foreign name ->
                Printf.sprintf
                  "I am inferring a weird self-referential type for %s:"
                  (quoted (Data.Name.base name))
            | List | Number | Float | String | Char | If | Case
            | Call_result _ | Lambda | Accessor _ | Access _ | Record | Tuple
            | Unit ->
                "I am inferring a weird self-referential type here:");
          snippet;
          Doc.words
            "Here is my best effort at writing down the type. You will see ? for parts of the type that repeat something already printed out infinitely.";
          indented (Message.alone naming found);
          simple_hint
            "The problem is often in the most recently added pattern or function. Try commenting out that code to see if it helps.";
        ]
  | Bad_arity { thing; name; expects; given } ->
      let what = match thing with A_type -> "type" | A_variant -> "variant" in
      Doc.stack
        [
          Doc.words
            (Printf.sprintf "The %s %s needs %s, but I see %d instead:"
               (quoted (Data.Name.base name))
               what (args expects) given);
          snippet;
          Doc.words
            (if given < expects then "What is missing? Are some parentheses misplaced?"
             else if given - expects = 1 then
               "Which is the extra one? Maybe some parentheses are missing?"
             else "Which are the extra ones? Maybe some parentheses are missing?");
        ]
  | Case_without_branches ->
      Doc.stack
        [
          Doc.words "This `case` does not have any branches:";
          snippet;
          Doc.words
            "A `case` needs at least one branch, so I know what to do with the value it is given.";
        ]

let close_names near =
  Doc.above (List.map (fun name -> indented (Doc.yellow (Doc.text name))) near)

let cannot_find ~snippet ~what ~name ~bare ~prefix ~near =
  let imports_link =
    simple_hint
      "Read <https://elm-lang.org/0.19.1/imports> to see how `import` declarations work in Elm."
  in
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
    @ (match near with
      | [] -> [ Doc.words without ]
      | _ -> [ Doc.above [ Doc.words with_names; Doc.blank; close_names near ] ])
    @ [ imports_link ])

let of_name_problem source region (problem : Name_error.t) =
  let snippet = Snippet.of_region source region in
  let imports_link =
    simple_hint
      "Read <https://elm-lang.org/0.19.1/imports> to see how `import` declarations work in Elm."
  in
  match problem with
  | Unknown_module { qualifier; near } ->
      Doc.stack
        ([
           Doc.words
             (Printf.sprintf "You are trying to import a %s module:"
                (quoted qualifier));
           snippet;
         ]
        @ (match near with
          | [] -> [ Doc.words "I cannot find it anywhere in this project!" ]
          | _ ->
              [
                Doc.above
                  [
                    Doc.words
                      "I looked through the modules of this project, but I                        cannot find it! Maybe it is a typo for one of these                        names?";
                    Doc.blank;
                    close_names near;
                  ];
              ])
        @ [ imports_link ])
  | Not_exposed { module_name; name; near } ->
      Doc.stack
        ([
           Doc.words
             (Printf.sprintf "The %s module does not expose %s:"
                (quoted module_name) (quoted name));
           snippet;
         ]
        @
        match near with
        | [] ->
            [
              Doc.words
                "I cannot find any super similar exposed names. Maybe it is private?";
            ]
        | [ only ] ->
            [ Doc.words (Printf.sprintf "Maybe you want %s instead?" (quoted only)) ]
        | _ ->
            [
              Doc.above
                [ Doc.words "These names seem close though:"; Doc.blank; close_names near ];
            ])
  | Ctors_not_exposed { module_name; type_name } ->
      Doc.stack
        [
          Doc.words
            (Printf.sprintf
               "The %s module does not expose the constructors of %s:"
               (quoted module_name) (quoted type_name));
          snippet;
          Doc.words
            (Printf.sprintf
               "It would have to say `exposing (%s(..))` for them to be available here."
               type_name);
        ]
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
      Doc.stack
        [
          Doc.words
            (Printf.sprintf "There is no %s kernel value:"
               (quoted (module_name ^ "." ^ exported_name)));
          snippet;
          Doc.words
            "Kernel values are the primitives the compiler knows about, and this is not one of them.";
        ]
  | Kernel_needs_annotation { name } ->
      Doc.stack
        [
          Doc.words
            (Printf.sprintf
               "The %s definition has a kernel body, so it needs a type annotation:"
               (quoted name));
          snippet;
          Doc.words
            "A kernel value takes its type from the annotation alone, so I cannot work it out without one.";
        ]
  | Kernel_arity_mismatch { declared; kernel } ->
      Doc.stack
        [
          Doc.words
            (Printf.sprintf
               "This annotation takes %s, but the kernel value it names takes %s:"
               (args declared) (args kernel));
          snippet;
          Doc.words "The two have to agree.";
        ]
  | Duplicate_declaration { name } ->
      Doc.stack
        [
          Doc.words
            (Printf.sprintf "This file has multiple %s declarations."
               (quoted name));
          snippet;
          Doc.words "How can I know which one you want? Rename one of them!";
        ]
  | Duplicate_binder { name } ->
      Doc.stack
        [
          Doc.words
            (Printf.sprintf "This pattern uses %s more than once:" (quoted name));
          snippet;
          simple_hint
            "Rename one of them to make it clear which one you want. Elm does not allow a name to be defined twice.";
        ]
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
          indented
            (Doc.text (String.concat " -> " (modules @ [ List.nth modules 0 ])));
          Doc.words
            "Learn more about why this is not allowed at <https://elm-lang.org/0.19.1/import-cycles>.";
        ]
  | Recursive_value { names } ->
      Doc.stack
        [
          Doc.words "This definition is causing an infinite loop:";
          snippet;
          indented
            (Doc.text (String.concat " -> " (names @ [ List.nth names 0 ])));
          Doc.words
            "The definition depends on itself, so I cannot compute its value. Functions may call each other, but values may not.";
        ]

let of_syntax_problem source region (problem : Syntax_error.t) =
  let snippet = Snippet.of_region source region in
  match problem with
  | Unexpected_input { found } ->
      Doc.stack
        [
          Doc.words "I got stuck here:";
          snippet;
          Doc.words
            (Printf.sprintf "I was not expecting %s at this point."
               (quoted found));
        ]
  | Unknown_character { found } ->
      Doc.stack
        [
          Doc.words
            (Printf.sprintf "I do not know what %s means here:" (quoted found));
          snippet;
          Doc.words "This character cannot start anything I know of.";
        ]
  | Unterminated { what } ->
      Doc.stack
        [
          Doc.words
            (Printf.sprintf
               "I got to the end of the file without seeing the end of this %s:"
               (Syntax_error.what_is_unterminated what));
          snippet;
          Doc.words "Add the closing mark, or delete it and start again.";
        ]
  | Empty_character ->
      Doc.stack
        [
          Doc.words
            "I thought I was parsing a character, but I got stuck here:";
          snippet;
          Doc.words
            "Characters look like 'c' and hold exactly one character. For text, use double quotes instead.";
        ]
  | Crowded_character ->
      Doc.stack
        [
          Doc.words "This character literal holds more than one character:";
          snippet;
          Doc.words
            "Characters hold exactly one character. Use double quotes for text of any length.";
        ]
  | Unknown_escape { found } ->
      Doc.stack
        [
          Doc.words
            (Printf.sprintf "I do not know the escape %s:"
               (quoted ("\\" ^ found)));
          snippet;
          Doc.words
            "The escapes I know are \\n, \\t, \\r, \\b, \\\", \\', \\\\ and \\u{...}.";
        ]
  | Too_many_tuple_parts { given } ->
      Doc.stack
        [
          Doc.words
            (Printf.sprintf
               "I only accept tuples of two or three parts, but this one has %d:"
               given);
          snippet;
          Doc.words
            "Switch to a record. Records can hold as many values as you need, and each one has a name.";
        ]

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
  | Error.Type _ | Error.Syntax _ ->
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
  match warning.problem with
  | Missing_patterns { unhandled } ->
      {
        title = "MISSING PATTERNS";
        region = warning.region;
        suggestions = [];
        message =
          Doc.stack
            [
              Doc.words
                "This `case` does not have branches for all possibilities:";
              snippet;
              Doc.above
                [
                  Doc.words "Missing possibilities include:";
                  Doc.blank;
                  Doc.above
                    (List.map
                       (fun pattern ->
                         indented (Doc.yellow (Doc.text (drawn ~inside:false pattern))))
                       unhandled);
                ];
              Doc.words
                "I would have to crash if I saw one of those. Add branches for them!";
            ];
      }
  | Redundant_pattern { index } ->
      {
        title = "REDUNDANT PATTERN";
        region = warning.region;
        suggestions = [];
        message =
          Doc.stack
            [
              Doc.words
                (Printf.sprintf "The %s pattern is redundant:" (ordinal index));
              snippet;
              Doc.words
                "Any value with this shape will be handled by a previous pattern, so it should be removed.";
            ];
      }

let of_error source (error : Error.t) =
  let message =
    match error.problem with
    | Error.Syntax problem -> of_syntax_problem source error.region problem
    | Error.Name problem -> of_name_problem source error.region problem
    | Error.Type problem -> of_type_problem source error.region problem
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
