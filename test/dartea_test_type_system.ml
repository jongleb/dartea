open OUnit2

let canonical input =
  match Parse.Main.parse ~file:"Main.elm" input with
  | Error error -> raise (Reporting.Error.Found error)
  | Ok impl_list ->
      Canonical.Module.of_frontend ~fallback_name:"Main"
        (Ast.Kind.Frontend.Module.of_impl impl_list)

let resolved module_ =
  match Canonicalization.Resolve_names.in_module ~dependencies:[] module_ with
  | Ok resolved -> resolved
  | Error errors ->
      assert_failure
        (String.concat "\n"
           (List.map Reporting.Error.show errors))

let inferred source =
  Infer.Declarations.infer_toplevel ~imports:[] (resolved (canonical source))

let scheme_of source name =
  let result = inferred source in
  match
    Infer.Value_env.find (Data.Name.local name)
      result.Infer.Declarations.values
  with
  | Some scheme -> scheme
  | None -> assert_failure (Printf.sprintf "no type inferred for %s" name)

let printed source name =
  match scheme_of source name with
  | Typed.Type.Scheme (_, ty) -> Reporting.Message.of_type ty

let rec shape (ty : Typed.Type.t) =
  let parenthesised inner =
    match inner with
    | Typed.Type.TFun _ | Typed.Type.TTup _ -> "(" ^ shape inner ^ ")"
    | _ -> shape inner
  in
  match ty with
  | TVar variable -> begin
      match Typed.Variable.constraint_of variable with
      | Some carried -> Data.Constraint.name carried
      | None -> "*"
    end
  | TFun (parameter, result) -> parenthesised parameter ^ " -> " ^ shape result
  | TTup items -> "( " ^ String.concat ", " (List.map shape items) ^ " )"
  | TCustom (name, []) -> Data.Name.base name
  | TCustom (name, arguments) ->
      Data.Name.base name ^ " "
      ^ String.concat " " (List.map parenthesised arguments)
  | TInt | TFloat | TChar | TStr | TBool | TUnit | TRecord _ | TRowExtend _
  | TRowEmpty ->
      Reporting.Message.of_type ty

let assert_shape ~src ~name ~expected =
  match scheme_of src name with
  | Typed.Type.Scheme (_, ty) ->
      assert_equal ~printer:(fun s -> s) expected (shape ty)

let assert_type ~src ~name ~expected =
  assert_equal ~printer:(fun s -> s) expected (printed src name)

let written = Reporting.Message.of_type

let sides (problem : Reporting.Error.problem) =
  match problem with
  | Type (Bad_expression { found; expected; _ }) ->
      Some (written found, written (Reporting.Expectation.expected_type expected))
  | Type (Bad_pattern { found; expected; _ }) ->
      Some
        ( written found,
          written (Reporting.Expectation.expected_pattern_type expected) )
  | Type (Infinite_type _ | Bad_arity _ | Case_without_branches)
  | Name _ | Syntax _ ->
      None

let constraint_names = [ "number"; "comparable"; "appendable"; "compappend" ]
let is_mismatch problem = Option.is_some (sides problem)

let mismatch ~found ~expected problem =
  match sides problem with
  | Some (one, other) -> String.equal one found && String.equal other expected
  | None -> false

let unsatisfied ~required problem =
  match sides problem with
  | Some (one, other) -> String.equal one required || String.equal other required
  | None -> false

let unsatisfied_by ~found ~required problem =
  match sides problem with
  | Some (one, other) ->
      (String.equal one found && String.equal other required)
      || (String.equal one required && String.equal other found)
  | None -> false

let is_constraint_clash problem =
  match sides problem with
  | Some (one, other) ->
      List.mem one constraint_names && List.mem other constraint_names
  | None -> false

let is_constructor_arity (problem : Reporting.Error.problem) =
  match problem with
  | Type (Bad_arity { thing = A_variant; _ }) -> true
  | _ -> false

let rejection src =
  match inferred src with
  | result -> (
      match result.Infer.Declarations.errors with
      | [] -> None
      | error :: _ -> Some error)
  | exception Reporting.Error.Found error -> Some error

let assert_rejected ~src ~because =
  match rejection src with
  | Some error ->
      assert_bool
        (Printf.sprintf "unexpected rejection: %s"
           (Reporting.Error.show_problem error.problem))
        (because error.problem)
  | None -> assert_failure "expected a rejection"

let test_float_literal_is_a_float _ =
  assert_type ~src:"x = 1.5" ~name:"x" ~expected:"Float"

let test_char_literal_is_a_char _ =
  assert_type ~src:"c = 'a'" ~name:"c" ~expected:"Char"

let test_float_annotation_unifies_with_a_float_literal _ =
  assert_type ~src:{|
half : Float
half = 0.5
|} ~name:"half"
    ~expected:"Float"

let test_char_annotation_unifies_with_a_char_literal _ =
  assert_type ~src:{|
letter : Char
letter = 'a'
|} ~name:"letter"
    ~expected:"Char"

let test_float_is_not_an_int _ =
  assert_rejected ~src:{|
count : Int
count = 1.5
|}
    ~because:(mismatch ~found:"Float" ~expected:"Int")

let test_char_is_not_a_string _ =
  assert_rejected ~src:{|
letter : String
letter = 'a'
|}
    ~because:(mismatch ~found:"Char" ~expected:"String")

let test_char_pattern_forces_a_char_scrutinee _ =
  assert_rejected
    ~src:
      {|
classify : String -> Int
classify s =
    case s of
        'a' ->
            1

        _ ->
            0
|}
    ~because:is_mismatch


let test_constraint_survives_instantiation _ =
  assert_shape
    ~src:
      {|
twice : number -> number
twice n = n

again : number -> number
again n = twice (twice n)
|}
    ~name:"again" ~expected:"number -> number"

let test_a_constrained_scheme_serves_two_numeric_types _ =
  assert_shape
    ~src:
      {|
keep : number -> number
keep n = n

counted : Int
counted = keep 1

measured : Float
measured = keep 1.5
|}
    ~name:"measured" ~expected:"Float"

let test_number_does_not_become_a_string _ =
  assert_rejected
    ~src:
      {|
keep : number -> number
keep n = n

label : String
label = keep "one"
|}
    ~because:(unsatisfied_by ~found:"String" ~required:"number")

let test_comparable_does_not_accept_a_record _ =
  assert_rejected
    ~src:
      {|
smaller : comparable -> comparable
smaller x = x

placed : { x : Int }
placed = smaller { x = 1 }
|}
    ~because:(unsatisfied ~required:"comparable")

let test_appendable_does_not_accept_an_int _ =
  assert_rejected
    ~src:
      {|
glue : appendable -> appendable
glue a = a

total : Int -> Int
total n = glue n
|}
    ~because:(unsatisfied_by ~found:"Int" ~required:"appendable")

let test_appendable_does_not_accept_a_numeric_literal _ =
  assert_rejected
    ~src:{|
glue : appendable -> appendable
glue a = a

total : Int
total = glue 1
|}
    ~because:is_constraint_clash

let test_comparable_accepts_a_list_of_comparables _ =
  assert_shape
    ~src:
      {|
smaller : comparable -> comparable
smaller x = x

sorted : List Int
sorted = smaller [ 1, 2 ]
|}
    ~name:"sorted" ~expected:"List Int"

let test_comparable_rejects_a_list_of_records _ =
  assert_rejected
    ~src:
      {|
smaller : comparable -> comparable
smaller x = x

sorted : List { x : Int }
sorted = smaller [ { x = 1 } ]
|}
    ~because:(unsatisfied ~required:"comparable")

let test_comparable_accepts_a_tuple_of_comparables _ =
  assert_shape
    ~src:
      {|
smaller : comparable -> comparable
smaller x = x

placed : ( Int, String )
placed = smaller ( 1, "a" )
|}
    ~name:"placed" ~expected:"( Int, String )"

let test_comparable_rejects_a_tuple_holding_a_record _ =
  assert_rejected
    ~src:
      {|
smaller : comparable -> comparable
smaller x = x

placed : ( Int, { x : Int } )
placed = smaller ( 1, { x = 1 } )
|}
    ~because:(unsatisfied ~required:"comparable")

let test_appendable_meets_comparable_as_compappend _ =
  assert_shape
    ~src:
      {|
glue : appendable -> appendable
glue a = a

smaller : comparable -> comparable
smaller x = x

both : appendable -> appendable
both a = glue (smaller a)
|}
    ~name:"both" ~expected:"compappend -> compappend"

let test_number_and_appendable_cannot_meet _ =
  assert_rejected
    ~src:
      {|
glue : appendable -> appendable
glue a = a

double : number -> number
double n = n

confused : number -> number
confused n = glue (double n)
|}
    ~because:is_constraint_clash

let test_a_variable_named_numbers_is_unconstrained _ =
  assert_shape ~src:{|
hold : numbers -> numbers
hold n = n
|}
    ~name:"hold" ~expected:"* -> *"

let test_a_variable_named_numbers_accepts_a_string _ =
  assert_shape
    ~src:
      {|
hold : numbers -> numbers
hold n = n

label : String
label = hold "one"
|}
    ~name:"label" ~expected:"String"

let test_numbered_constraint_variables_are_independent _ =
  assert_shape
    ~src:
      {|
pick : comparable -> comparable2 -> comparable
pick a b = a

chosen : Int
chosen = pick 1 "a"
|}
    ~name:"chosen" ~expected:"Int"

let test_comparable_does_not_accept_a_custom_type _ =
  assert_rejected
    ~src:
      {|
type Shade = Light | Dark

smaller : comparable -> comparable
smaller x = x

picked : Shade
picked = smaller Light
|}
    ~because:(unsatisfied ~required:"comparable")

let test_integer_division_stays_on_ints _ =
  assert_rejected ~src:{|
half : Int
half = 1.5 // 2
|}
    ~because:is_mismatch

let test_float_division_does_not_produce_an_int _ =
  assert_rejected ~src:{|
half : Int
half = 7 / 2
|}
    ~because:is_mismatch

let test_appendable_does_not_accept_a_float _ =
  assert_rejected
    ~src:
      {|
glue : appendable -> appendable
glue a = a

total : Float -> Float
total n = glue n
|}
    ~because:(unsatisfied_by ~found:"Float" ~required:"appendable")

let test_number_does_not_accept_a_char _ =
  assert_rejected
    ~src:{|
twice : number -> number
twice n = n

letter : Char -> Char
letter c = twice c
|}
    ~because:(unsatisfied_by ~found:"Char" ~required:"number")

let test_a_list_literal_keeps_its_element_type _ =
  assert_shape ~src:{|
words = [ "x", "y" ]
|} ~name:"words"
    ~expected:"List String"

let test_a_longer_list_literal_keeps_its_element_type _ =
  assert_shape ~src:{|
words = [ "x", "y", "z" ]
|} ~name:"words"
    ~expected:"List String"

let test_an_annotated_list_pattern_keeps_its_element_type _ =
  assert_shape
    ~src:
      {|
firstWord : List String -> String
firstWord xs =
    case xs of
        [ a, b ] ->
            a

        _ ->
            "none"
|}
    ~name:"firstWord" ~expected:"List String -> String"

let recursion_without_annotations =
  {|
isEven n = if n == 0 then True else isOdd (n - 1)

isOdd n = if n == 0 then False else isEven (n - 1)
|}

let calling_a_later_declaration =
  {|
divides d n = rem n d == 0

quot a b = a // b

rem a b = a - quot a b * b

useIt : Bool
useIt = divides "a" "b"
|}

let calling_an_earlier_declaration =
  {|
divides d n = arem n d == 0

aquot a b = a // b

arem a b = a - aquot a b * b

useIt : Bool
useIt = divides "a" "b"
|}

let test_a_callee_declared_later_is_still_typed _ =
  assert_rejected ~src:calling_a_later_declaration ~because:is_mismatch

let test_a_callee_declared_earlier_is_typed_the_same_way _ =
  assert_rejected ~src:calling_an_earlier_declaration
    ~because:is_mismatch

let test_a_caller_gets_the_type_of_a_later_callee _ =
  assert_shape
    ~src:
      {|
divides d n = rem n d == 0

quot a b = a // b

rem a b = a - quot a b * b
|}
    ~name:"divides" ~expected:"Int -> Int -> Bool"

let test_mutual_recursion_without_annotations_is_typed _ =
  assert_shape ~src:recursion_without_annotations ~name:"isEven"
    ~expected:"number -> Bool"

let test_both_members_of_a_group_are_typed _ =
  assert_shape ~src:recursion_without_annotations ~name:"isOdd"
    ~expected:"number -> Bool"

let test_a_forward_call_settles_the_callee_type _ =
  assert_shape ~src:{|
f x = g x

g x = x + 1
|} ~name:"f"
    ~expected:"number -> number"

let test_a_backward_call_settles_the_callee_type _ =
  assert_shape ~src:{|
g x = f x

f x = x + 1
|} ~name:"g"
    ~expected:"number -> number"

let test_a_group_is_generalized_against_the_context_before_it _ =
  assert_shape
    ~src:{|
usedTwice = ( identity 1, identity "str" )

identity x = x
|}
    ~name:"usedTwice" ~expected:"( number, String )"

let spelled_with one other =
  Printf.sprintf
    {|
first : ( %s, %s ) -> %s
first t =
    case t of
        ( x, y ) ->
            x

used : String
used = first ( "s", 1 )
|}
    one other one

let test_a_written_type_variable_never_meets_a_generated_one _ =
  List.iter
    (fun (one, other) ->
      assert_type ~src:(spelled_with one other) ~name:"used" ~expected:"String")
    [ ("a0", "a1"); ("a1", "a2"); ("b0", "b1"); ("a", "b"); ("t", "u") ]

let test_a_polymorphic_declaration_stays_polymorphic _ =
  assert_shape ~src:{|
identity x = x

usedTwice = ( identity 1, identity "str" )
|}
    ~name:"identity" ~expected:"* -> *"

let test_a_constructor_payload_has_its_declared_type _ =
  assert_rejected
    ~src:
      {|
type Box = Box Int

bad : String
bad =
    case Box 1 of
        Box p ->
            p
|}
    ~because:is_mismatch

let test_a_payload_reached_through_a_parameter_has_its_declared_type _ =
  assert_rejected
    ~src:
      {|
type Box = Box Int

bad : Box -> String
bad b =
    case b of
        Box p ->
            p
|}
    ~because:is_mismatch

let test_a_polymorphic_payload_follows_the_scrutinee _ =
  assert_rejected
    ~src:
      {|
type Maybe a = Just a | Nothing

bad : String
bad =
    case Just 1 of
        Just n ->
            n

        Nothing ->
            "e"
|}
    ~because:(unsatisfied_by ~found:"String" ~required:"number")

let test_a_nested_constructor_payload_has_its_declared_type _ =
  assert_rejected
    ~src:
      {|
type Wrap = Wrap Int

type Box = Box Wrap

bad : String
bad =
    case Box (Wrap 1) of
        Box (Wrap n) ->
            n
|}
    ~because:is_mismatch

let test_a_tuple_pattern_binds_each_position _ =
  assert_rejected
    ~src:
      {|
bad : Int
bad =
    case ( 1, "a" ) of
        ( a, b ) ->
            b
|}
    ~because:is_mismatch

let test_a_cons_pattern_binds_the_head_to_the_element _ =
  assert_rejected
    ~src:
      {|
bad : String
bad =
    case [ 1 ] of
        x :: rest ->
            x

        [] ->
            "e"
|}
    ~because:(unsatisfied_by ~found:"String" ~required:"number")

let test_an_alias_pattern_keeps_typing_the_names_beneath_it _ =
  assert_rejected
    ~src:
      {|
type Box = Box Int

bad : String
bad =
    case Box 1 of
        (Box p) as whole ->
            p
|}
    ~because:is_mismatch

let test_a_record_pattern_binds_the_field_type _ =
  assert_rejected
    ~src:{|
bad : String
bad =
    (\{ count } -> count) { count = 1 }
|}
    ~because:(unsatisfied_by ~found:"String" ~required:"number")

let test_a_let_destructuring_binds_each_position _ =
  assert_rejected
    ~src:
      {|
bad : Int
bad =
    let
        ( a, b ) =
            ( 1, "a" )
    in
    b
|}
    ~because:is_mismatch

let test_a_destructured_declaration_parameter_carries_the_payload _ =
  assert_rejected
    ~src:{|
type Box = Box Int

bad : Box -> String
bad (Box n) =
    n
|}
    ~because:is_mismatch

let test_a_destructured_lambda_parameter_binds_each_position _ =
  assert_rejected
    ~src:{|
bad : Int
bad =
    (\( a, b ) -> b) ( 1, "a" )
|}
    ~because:is_mismatch

let test_a_unit_parameter_is_a_unit _ =
  assert_rejected ~src:{|
bad : Int -> Int
bad () =
    1
|}
    ~because:is_mismatch

let test_a_polymorphic_payload_stays_polymorphic _ =
  assert_shape
    ~src:
      {|
type Maybe a = Just a | Nothing

keep m =
    case m of
        Just n ->
            Just n

        Nothing ->
            Nothing
|}
    ~name:"keep" ~expected:"Maybe * -> Maybe *"

let test_a_constructor_pattern_checks_its_arity _ =
  assert_rejected
    ~src:{|
type Box = Box Int

bad : Box -> Int
bad b =
    case b of
        Box ->
            1
|}
    ~because:is_constructor_arity

let test_a_tuple_pattern_accepts_the_matching_positions _ =
  assert_shape
    ~src:{|
good : Int
good =
    case ( 1, "a" ) of
        ( a, b ) ->
            a
|}
    ~name:"good" ~expected:"Int"

let test_a_cons_pattern_accepts_the_matching_element _ =
  assert_shape
    ~src:
      {|
good : String
good =
    case [ "a" ] of
        x :: rest ->
            x

        [] ->
            "e"
|}
    ~name:"good" ~expected:"String"

let test_an_alias_pattern_names_the_whole_scrutinee _ =
  assert_shape
    ~src:
      {|
type Box = Box Int

good : Box
good =
    case Box 1 of
        (Box p) as whole ->
            whole
|}
    ~name:"good" ~expected:"Box"

let test_a_branch_body_complains_about_the_bound_name _ =
  assert_rejected
    ~src:
      {|
type Box = Box Int

bad : Box -> String
bad b =
    case b of
        Box p ->
            p ++ "!"
|}
    ~because:(unsatisfied ~required:"appendable")

let test_a_written_type_variable_never_collides_with_a_generated_one _ =
  assert_shape
    ~src:
      {|
first : ( a0, a1 ) -> a0
first t =
    case t of
        ( x, y ) ->
            x

used : String
used = first ( "s", 1 )
|}
    ~name:"used" ~expected:"String"

let test_a_written_number_variable_is_still_constrained _ =
  assert_rejected
    ~src:{|
twice : number0 -> number0
twice n = n

bad : String
bad = twice "a"
|}
    ~because:(unsatisfied_by ~found:"String" ~required:"number")

let suite =
  [
    "float_literal_is_a_float" >:: test_float_literal_is_a_float;
    "char_literal_is_a_char" >:: test_char_literal_is_a_char;
    "float_annotation_unifies_with_a_float_literal"
    >:: test_float_annotation_unifies_with_a_float_literal;
    "char_annotation_unifies_with_a_char_literal"
    >:: test_char_annotation_unifies_with_a_char_literal;
    "float_is_not_an_int" >:: test_float_is_not_an_int;
    "char_is_not_a_string" >:: test_char_is_not_a_string;
    "char_pattern_forces_a_char_scrutinee"
    >:: test_char_pattern_forces_a_char_scrutinee;
    "constraint_survives_instantiation"
    >:: test_constraint_survives_instantiation;
    "a_constrained_scheme_serves_two_numeric_types"
    >:: test_a_constrained_scheme_serves_two_numeric_types;
    "number_does_not_become_a_string" >:: test_number_does_not_become_a_string;
    "comparable_does_not_accept_a_record"
    >:: test_comparable_does_not_accept_a_record;
    "appendable_does_not_accept_an_int"
    >:: test_appendable_does_not_accept_an_int;
    "appendable_does_not_accept_a_numeric_literal"
    >:: test_appendable_does_not_accept_a_numeric_literal;
    "comparable_accepts_a_list_of_comparables"
    >:: test_comparable_accepts_a_list_of_comparables;
    "comparable_rejects_a_list_of_records"
    >:: test_comparable_rejects_a_list_of_records;
    "comparable_accepts_a_tuple_of_comparables"
    >:: test_comparable_accepts_a_tuple_of_comparables;
    "comparable_rejects_a_tuple_holding_a_record"
    >:: test_comparable_rejects_a_tuple_holding_a_record;
    "appendable_meets_comparable_as_compappend"
    >:: test_appendable_meets_comparable_as_compappend;
    "number_and_appendable_cannot_meet"
    >:: test_number_and_appendable_cannot_meet;
    "a_variable_named_numbers_is_unconstrained"
    >:: test_a_variable_named_numbers_is_unconstrained;
    "a_variable_named_numbers_accepts_a_string"
    >:: test_a_variable_named_numbers_accepts_a_string;
    "numbered_constraint_variables_are_independent"
    >:: test_numbered_constraint_variables_are_independent;
    "comparable_does_not_accept_a_custom_type"
    >:: test_comparable_does_not_accept_a_custom_type;
    "integer_division_stays_on_ints" >:: test_integer_division_stays_on_ints;
    "float_division_does_not_produce_an_int"
    >:: test_float_division_does_not_produce_an_int;
    "appendable_does_not_accept_a_float"
    >:: test_appendable_does_not_accept_a_float;
    "number_does_not_accept_a_char" >:: test_number_does_not_accept_a_char;
    "a_list_literal_keeps_its_element_type"
    >:: test_a_list_literal_keeps_its_element_type;
    "a_longer_list_literal_keeps_its_element_type"
    >:: test_a_longer_list_literal_keeps_its_element_type;
    "an_annotated_list_pattern_keeps_its_element_type"
    >:: test_an_annotated_list_pattern_keeps_its_element_type;
    "a_callee_declared_later_is_still_typed"
    >:: test_a_callee_declared_later_is_still_typed;
    "a_callee_declared_earlier_is_typed_the_same_way"
    >:: test_a_callee_declared_earlier_is_typed_the_same_way;
    "a_caller_gets_the_type_of_a_later_callee"
    >:: test_a_caller_gets_the_type_of_a_later_callee;
    "mutual_recursion_without_annotations_is_typed"
    >:: test_mutual_recursion_without_annotations_is_typed;
    "both_members_of_a_group_are_typed"
    >:: test_both_members_of_a_group_are_typed;
    "a_forward_call_settles_the_callee_type"
    >:: test_a_forward_call_settles_the_callee_type;
    "a_backward_call_settles_the_callee_type"
    >:: test_a_backward_call_settles_the_callee_type;
    "a_group_is_generalized_against_the_context_before_it"
    >:: test_a_group_is_generalized_against_the_context_before_it;
    "a_written_type_variable_never_meets_a_generated_one"
    >:: test_a_written_type_variable_never_meets_a_generated_one;
    "a_polymorphic_declaration_stays_polymorphic"
    >:: test_a_polymorphic_declaration_stays_polymorphic;
    "a_constructor_payload_has_its_declared_type"
    >:: test_a_constructor_payload_has_its_declared_type;
    "a_payload_reached_through_a_parameter_has_its_declared_type"
    >:: test_a_payload_reached_through_a_parameter_has_its_declared_type;
    "a_polymorphic_payload_follows_the_scrutinee"
    >:: test_a_polymorphic_payload_follows_the_scrutinee;
    "a_nested_constructor_payload_has_its_declared_type"
    >:: test_a_nested_constructor_payload_has_its_declared_type;
    "a_tuple_pattern_binds_each_position"
    >:: test_a_tuple_pattern_binds_each_position;
    "a_cons_pattern_binds_the_head_to_the_element"
    >:: test_a_cons_pattern_binds_the_head_to_the_element;
    "an_alias_pattern_keeps_typing_the_names_beneath_it"
    >:: test_an_alias_pattern_keeps_typing_the_names_beneath_it;
    "a_record_pattern_binds_the_field_type"
    >:: test_a_record_pattern_binds_the_field_type;
    "a_let_destructuring_binds_each_position"
    >:: test_a_let_destructuring_binds_each_position;
    "a_destructured_declaration_parameter_carries_the_payload"
    >:: test_a_destructured_declaration_parameter_carries_the_payload;
    "a_destructured_lambda_parameter_binds_each_position"
    >:: test_a_destructured_lambda_parameter_binds_each_position;
    "a_unit_parameter_is_a_unit" >:: test_a_unit_parameter_is_a_unit;
    "a_polymorphic_payload_stays_polymorphic"
    >:: test_a_polymorphic_payload_stays_polymorphic;
    "a_constructor_pattern_checks_its_arity"
    >:: test_a_constructor_pattern_checks_its_arity;
    "a_tuple_pattern_accepts_the_matching_positions"
    >:: test_a_tuple_pattern_accepts_the_matching_positions;
    "a_cons_pattern_accepts_the_matching_element"
    >:: test_a_cons_pattern_accepts_the_matching_element;
    "an_alias_pattern_names_the_whole_scrutinee"
    >:: test_an_alias_pattern_names_the_whole_scrutinee;
    "a_branch_body_complains_about_the_bound_name"
    >:: test_a_branch_body_complains_about_the_bound_name;
    "a_written_type_variable_never_collides_with_a_generated_one"
    >:: test_a_written_type_variable_never_collides_with_a_generated_one;
    "a_written_number_variable_is_still_constrained"
    >:: test_a_written_number_variable_is_still_constrained;
  ]
