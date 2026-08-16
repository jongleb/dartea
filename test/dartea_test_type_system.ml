open OUnit2

let canonical input =
  match Parse.Main.parse input with
  | Error e -> raise e
  | Ok impl_list ->
      Canonical.Module.of_frontend ~fallback_name:"Main"
        (Ast.Kind.Frontend.Module.of_impl impl_list)

let resolved module_ =
  match Canonicalization.Resolve_names.in_module ~dependencies:[] module_ with
  | Ok resolved -> resolved
  | Error errors ->
      assert_failure
        (String.concat "\n"
           (List.map Canonicalization.Resolve_names.show_error errors))

let inferred source =
  Infer.Infer_proc.State.reset ();
  Infer.Infer_proc.infer_toplevel ~imports:[]
    (resolved (canonical source))
    Dartea.Compiler.initial_ctx

let scheme_of source name =
  let result = inferred source in
  match
    Infer.Infer_proc.Name_map.find_opt (Data.Name.local name)
      result.Infer.Infer_proc.ctx
  with
  | Some scheme -> scheme
  | None -> assert_failure (Printf.sprintf "no type inferred for %s" name)

let printed source name =
  match scheme_of source name with
  | Typed.Type.Scheme (_, ty) -> Infer.Infer_proc.string_of_typ ty

let rec shape (ty : Typed.Type.t) =
  let parenthesised inner =
    match inner with
    | Typed.Type.TFun _ | Typed.Type.TTup _ -> "(" ^ shape inner ^ ")"
    | _ -> shape inner
  in
  match ty with
  | TVar variable -> begin
      match Data.Constraint.of_variable variable with
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
      Infer.Infer_proc.string_of_typ ty

let assert_shape ~src ~name ~expected =
  match scheme_of src name with
  | Typed.Type.Scheme (_, ty) ->
      assert_equal ~printer:(fun s -> s) expected (shape ty)

let assert_type ~src ~name ~expected =
  assert_equal ~printer:(fun s -> s) expected (printed src name)

let assert_rejected ~src ~because =
  match inferred src with
  | exception Failure message ->
      assert_bool
        (Printf.sprintf "expected a message mentioning %S, got %S" because
           message)
        (Node_runner.contains ~needle:because message)
  | _ ->
      assert_failure
        (Printf.sprintf "expected a rejection mentioning %S" because)

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
    ~because:"Unification failed for Float and Int"

let test_char_is_not_a_string _ =
  assert_rejected ~src:{|
letter : String
letter = 'a'
|}
    ~because:"Unification failed for Char and String"

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
    ~because:"Char"


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
    ~because:"String does not satisfy number"

let test_comparable_does_not_accept_a_record _ =
  assert_rejected
    ~src:
      {|
smaller : comparable -> comparable
smaller x = x

placed : { x : Int }
placed = smaller { x = 1 }
|}
    ~because:"does not satisfy comparable"

let test_appendable_does_not_accept_an_int _ =
  assert_rejected
    ~src:
      {|
glue : appendable -> appendable
glue a = a

total : Int -> Int
total n = glue n
|}
    ~because:"Int does not satisfy appendable"

let test_appendable_does_not_accept_a_numeric_literal _ =
  assert_rejected
    ~src:{|
glue : appendable -> appendable
glue a = a

total : Int
total = glue 1
|}
    ~because:"cannot be the same type variable"

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
    ~because:"does not satisfy comparable"

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
    ~because:"does not satisfy comparable"

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
    ~because:"cannot be the same type variable"

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
    ~because:"does not satisfy comparable"

let test_integer_division_stays_on_ints _ =
  assert_rejected ~src:{|
half : Int
half = 1.5 // 2
|}
    ~because:"Unification failed"

let test_float_division_does_not_produce_an_int _ =
  assert_rejected ~src:{|
half : Int
half = 7 / 2
|}
    ~because:"Unification failed"

let test_appendable_does_not_accept_a_float _ =
  assert_rejected
    ~src:
      {|
glue : appendable -> appendable
glue a = a

total : Float -> Float
total n = glue n
|}
    ~because:"Float does not satisfy appendable"

let test_number_does_not_accept_a_char _ =
  assert_rejected
    ~src:{|
twice : number -> number
twice n = n

letter : Char -> Char
letter c = twice c
|}
    ~because:"Char does not satisfy number"

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
  assert_rejected ~src:calling_a_later_declaration ~because:"Unification failed"

let test_a_callee_declared_earlier_is_typed_the_same_way _ =
  assert_rejected ~src:calling_an_earlier_declaration
    ~because:"Unification failed"

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

let test_a_polymorphic_declaration_stays_polymorphic _ =
  assert_shape ~src:{|
identity x = x

usedTwice = ( identity 1, identity "str" )
|}
    ~name:"identity" ~expected:"* -> *"

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
    "a_polymorphic_declaration_stays_polymorphic"
    >:: test_a_polymorphic_declaration_stays_polymorphic;
  ]
