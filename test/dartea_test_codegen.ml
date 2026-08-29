open OUnit2

let compiled_of src = Node_runner.output_of (Dartea.Compiler.compile_source src)

let module_source ~name src =
  Node_runner.source_of ~module_name:name (compiled_of src)

let main_source src = module_source ~name:"Main" src
let node_eval ~src ~expr = Node_runner.evaluate ~compiled:(compiled_of src) ~expr
let contains = Node_runner.contains

let assert_js ~src ~expr ~expected =
  assert_equal ~printer:(fun s -> s) expected (node_eval ~src ~expr)

let test_arithmetic _ = assert_js ~src:"x = 1 + 2 * 3" ~expr:"Main.x" ~expected:"7"

let test_ctor_unwrap _ =
  let src =
    {|
type Box = Box Int

unbox : Box -> Int
unbox b =
    case b of
        Box n ->
            n

result : Int
result = unbox (Box 42)
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"42"

let test_nullary_match _ =
  let src =
    {|
type Color = Red | Green | Blue

toInt : Color -> Int
toInt c =
    case c of
        Red ->
            1

        Green ->
            2

        Blue ->
            3

result : Int
result = toInt Green
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"2"

let test_ctor_is_defined _ =
  let src = {|
type Wrap = Wrap Int

w : Wrap
w = Wrap 7
|} in
  assert_js ~src ~expr:"Main.w._0" ~expected:"7"

let test_nested_pattern _ =
  let src =
    {|
type TestEnum = C | D

type TestEnum2 = E TestEnum | F String

testAgain : TestEnum2 -> Int
testAgain x =
    case x of
        E C ->
            1

        E D ->
            3

        F "x" ->
            4

        _ ->
            5

result : Int
result = testAgain (F "lol")
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"5"

let test_nested_distinguishes _ =
  let src =
    {|
type Inner = C | D

type Outer = E Inner | F Inner

toInt : Outer -> Int
toInt o =
    case o of
        E C ->
            1

        E D ->
            2

        F C ->
            3

        F D ->
            4

result : Int
result = toInt (E D)
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"2"

let test_let_in _ =
  let src =
    {|
result : Int
result =
    let
        y = String.length "hello"
    in
    y + 1
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"6";
  let js = main_source src in
  assert_bool "value local should hoist (const y)"
    (contains ~needle:{|const y = $$String.length("hello")|} js);
  assert_bool "value should not be an IIFE" (not (contains ~needle:"(() =>" js))

let test_if_then_else _ =
  let src =
    {|
f : Int -> Int
f x =
    if x == 0 then
        10
    else
        20

result : Int
result = f 0
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"10"

let test_comparison _ =
  let src =
    {|
f : Int -> Int
f x =
    if x < 5 then
        1
    else
        0

result : Int
result = f 2
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"1"

let test_fn_with_let _ =
  let src =
    {|
f : Int -> Int
f x =
    let
        d = x + 1
    in
    d * 2

result : Int
result = f 3
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"8"

let test_prefers_const_arrow _ =
  let src = {|
add : Int -> Int -> Int
add a b = a + b
|} in
  let js = main_source src in
  assert_bool "function should be a const binding"
    (contains ~needle:"const add = (a, b) =>" js);
  assert_bool "no function declaration" (not (contains ~needle:"function add" js))

let test_switch_dispatch _ =
  let src =
    {|
type Color = Red | Green | Blue

toInt : Color -> Int
toInt c =
    case c of
        Red ->
            1

        Green ->
            2

        Blue ->
            3

result : Int
result = toInt Blue
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"3";
  let js = main_source src in

  assert_bool "flat ADT match should switch on the scrutinee"
    (contains ~needle:"switch (c)" js)

let test_nested_let_hoist _ =
  let src =
    {|
f : Int -> Int
f x =
    let
        a =
            let
                b = x + 1
            in
            b + 2
    in
    a + 3

result : Int
result = f 10
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"16";
  let js = main_source src in
  assert_bool "inner let should hoist (const b)"
    (contains ~needle:"const b =" js);
  assert_bool "no IIFE (() =>" (not (contains ~needle:"(() =>" js));
  assert_bool "no IIFE )()" (not (contains ~needle:")()" js))

let test_reserved_words _ =
  let src =
    {|
default : Int -> Int
default x =
    let
        new = x + 1
    in
    new

result : Int
result = default 5
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"6";
  let js = main_source src in
  assert_bool "reserved name should be legalized ($$default)"
    (contains ~needle:"$$default" js);
  assert_bool "no bare `const default`"
    (not (contains ~needle:"const default " js))

let test_partial_application _ =
  let src =
    {|
add : Int -> Int -> Int
add a b =
    case a of
        0 ->
            b

        _ ->
            add (a - 1) (b + 1)

applyTimes : Int -> (Int -> Int) -> Int -> Int
applyTimes k f n =
    case k of
        0 ->
            n

        _ ->
            applyTimes (k - 1) f (f n)

result : Int
result = applyTimes 3 (add 1) 5
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"8";
  let js = main_source src in

  assert_bool "partial application is an explicit closure"
    (contains ~needle:"=> add(1, " js)

let test_direct_call _ =
  let src =
    {|
countUp : Int -> Int -> Int
countUp n acc =
    case n of
        0 ->
            acc

        _ ->
            countUp (n - 1) (acc + 1)

result : Int
result = countUp 3 4
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"7";
  let js = main_source src in
  assert_bool "saturated call is direct (countUp(3, 4))"
    (contains ~needle:"countUp(3, 4)" js)

let test_shadowing _ =
  let src =
    {|
f : Int -> Int
f x =
    let
        x = x + 1
    in
    x

result : Int
result = f 10
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"11"

let test_value_position_match _ =
  let src =
    {|
type T = A | B

pick : T -> Int
pick t =
    let
        y =
            case t of
                A ->
                    10

                B ->
                    20
    in
    y + 1

result : Int
result = pick B
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"21"

let test_tail_call_loop _ =
  let src =
    {|
loop : Int -> Int -> Int
loop n acc =
    if n == 0 then
        acc
    else
        loop (n - 1) (acc + n)

result : Int
result = loop 100000 0
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"5000050000";
  let js = main_source src in
  assert_bool "self tail recursion should compile to a while loop"
    (contains ~needle:"while (true)" js)

let test_list_patterns _ =
  let src =
    {|
describe : List Int -> Int
describe xs =
    case xs of
        [] ->
            0

        [ a ] ->
            a

        _ ->
            99

result : Int
result = describe [ 7 ]
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"7"

let test_cons_pattern _ =
  let src =
    {|
head : List Int -> Int
head xs =
    case xs of
        h :: t ->
            h

        [] ->
            0

result : Int
result = head [ 5, 6, 7 ]
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"5"

let test_decision_tree_nested _ =
  let src =
    {|
type Tree = Leaf | Node Tree Int Tree

describe : Tree -> Int
describe t =
    case t of
        Leaf ->
            0

        Node Leaf x _ ->
            x

        Node _ x Leaf ->
            x + 100

        Node _ x _ ->
            x + 200

result : Int
result = describe (Node (Node Leaf 5 Leaf) 7 Leaf)
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"107";
  let js = main_source src in
  assert_bool "decision tree recurses on the left sub-occurrence"
    (contains ~needle:"t._0 === \"Leaf\"" js);
  assert_bool "decision tree recurses on the right sub-occurrence"
    (contains ~needle:"t._2 === \"Leaf\"" js)

let test_int_literal_switch _ =
  let src =
    {|
classify : Int -> Int
classify n =
    case n of
        0 ->
            100

        1 ->
            200

        _ ->
            300

result : Int
result = classify 1
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"200";
  let js = main_source src in
  assert_bool "int literals dispatch through a switch"
    (contains ~needle:"switch (n)" js)

let test_payload_tag_switch _ =
  let src =
    {|
type Expr = Num Int | Add Int Int

eval : Expr -> Int
eval e =
    case e of
        Num n ->
            n

        Add a b ->
            a + b

result : Int
result = eval (Add 3 4)
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"7";
  let js = main_source src in
  assert_bool "payload constructors dispatch on the TAG"
    (contains ~needle:"switch (e.TAG)" js)

let test_nested_cons_bind _ =
  let src =
    {|
sum2 : List Int -> Int
sum2 xs =
    case xs of
        a :: b :: _ ->
            a + b

        a :: _ ->
            a

        [] ->
            0

result : Int
result = sum2 [ 3, 4, 9 ]
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"7"

let test_wildcard_default_share _ =
  let src =
    {|
type C = A | B | D

f : C -> Int
f c =
    case c of
        A ->
            1

        _ ->
            9

result : Int
result = f B
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"9"

let test_var_binds_scrutinee _ =
  let src =
    {|
addOne : Int -> Int
addOne n =
    case n of
        m ->
            m + 1

result : Int
result = addOne 41
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"42";
  let js = main_source src in
  assert_bool "a whole-value var pattern binds directly with no test"
    (contains ~needle:"const m = n;" js)

let test_shared_default_subtree _ =
  let src =
    {|
type T = A | B | C

type W = L T | R T

fallback : Int -> Int
fallback n =
    case n of
        0 ->
            0

        _ ->
            n + n + n

f : W -> Int
f w =
    case w of
        L A ->
            1

        R A ->
            2

        _ ->
            fallback 10

result : Int
result = f (L B)
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"30";
  let js = main_source src in
  assert_bool "the reused non-trivial default body is emitted once as a thunk"
    (contains ~needle:"const $dt0 = () => fallback(10);" js);
  assert_bool "both branches call the shared thunk, not the body"
    (contains ~needle:"return $dt0();" js)

let index_of ~needle hay =
  let nl = String.length needle and hl = String.length hay in
  let rec go i =
    if i + nl > hl then None
    else if String.sub hay i nl = needle then Some i
    else go (i + 1)
  in
  go 0

let test_pba_picks_needed_column _ =
  let base =
    {|
type Two = X | Y

type P = Pair Two Two

g : P -> Int
g p =
    case p of
        Pair X X ->
            1

        Pair _ Y ->
            2

        Pair Y _ ->
            3

|}
  in
  let result v = base ^ "r : Int\nr = g (Pair " ^ v ^ ")\n" in
  assert_js ~src:(result "X X") ~expr:"Main.r" ~expected:"1";
  assert_js ~src:(result "X Y") ~expr:"Main.r" ~expected:"2";
  assert_js ~src:(result "Y X") ~expr:"Main.r" ~expected:"3";
  assert_js ~src:(result "Y Y") ~expr:"Main.r" ~expected:"2";
  let js = main_source (result "X X") in
  match (index_of ~needle:"switch (p._1)" js, index_of ~needle:"switch (p._0)" js) with
  | Some i1, Some i0 ->
      assert_bool "pba tests the higher-necessity column (p._1) as the outer switch"
        (i1 < i0)
  | Some _, None -> ()
  | _ -> assert_failure "expected p._1 to be switched on"

let test_complete_variant_no_default _ =
  let src =
    {|
type W = L Int | R Int

f : W -> Int
f w =
    case w of
        L a ->
            a

        R b ->
            b

result : Int
result = f (R 7)
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"7";
  let js = main_source src in
  assert_bool "a complete variant match emits no default branch"
    (not (contains ~needle:"default" js))

let test_tag_omission_single_payload _ =
  let src =
    {|
type T = A | B | Wrap Int

f : T -> Int
f t =
    case t of
        Wrap n ->
            n + 1

        A ->
            10

        B ->
            20

r1 : Int
r1 = f (Wrap 5)

r2 : Int
r2 = f A
|}
  in
  assert_js ~src ~expr:"Main.r1" ~expected:"6";
  assert_js ~src ~expr:"Main.r2" ~expected:"10";
  let js = main_source src in
  assert_bool "a type with one payload constructor drops the TAG field"
    (not (contains ~needle:"TAG" js));
  assert_bool "the single payload constructor is discriminated by typeof"
    (contains ~needle:"typeof t === \"object\"" js)

let test_tag_omission_single_ctor _ =
  let src =
    {|
type Box = Box Int

unbox : Box -> Int
unbox b =
    case b of
        Box n ->
            n

result : Int
result = unbox (Box 42)
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"42";
  let js = main_source src in
  assert_bool "a single-constructor payload type carries no TAG"
    (not (contains ~needle:"TAG" js))

let test_tag_kept_for_multi_payload _ =
  let src =
    {|
type Expr = Num Int | Add Int Int

eval : Expr -> Int
eval e =
    case e of
        Num n ->
            n

        Add a b ->
            a + b

result : Int
result = eval (Num 9)
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"9";
  let js = main_source src in
  assert_bool "a type with several payload constructors keeps the TAG field"
    (contains ~needle:"TAG" js)

let test_builtin_direct _ =
  let src =
    {|
greeting : String
greeting = String.append (String.fromInt 4) "2"
|}
  in
  assert_js ~src ~expr:"Main.greeting" ~expected:{|"42"|};
  let js = main_source src in
  assert_bool "known-arity prelude value is a direct call"
    (contains ~needle:"$$String.fromInt(4)" js);
  assert_bool "no $$curry for a known-arity prelude value"
    (not (contains ~needle:"$$curry($$String.fromInt" js));
  assert_bool "the prelude value is imported, not redeclared"
    (not (contains ~needle:"const fromInt =" js))

let test_unused_prelude_module_not_imported _ =
  let js = main_source "x : Int\nx = 1 + 2" in
  assert_bool "an unused prelude module is not imported"
    (not (contains ~needle:"./Tuple.mjs" js));
  assert_bool "no prelude declaration leaks into the module"
    (not (contains ~needle:"const first =" js))

let test_constant_folding _ =
  let src = {|
x : Int
x = 1 + 2 * 3
|} in
  assert_js ~src ~expr:"Main.x" ~expected:"7";
  let js = main_source src in
  assert_bool "literal arithmetic is folded at compile time"
    (contains ~needle:"const x = 7;" js)

let test_constant_folding_comparison _ =
  let src =
    {|
r : Int
r =
    if 2 < 1 then
        1
    else
        2
|}
  in
  assert_js ~src ~expr:"Main.r" ~expected:"2";
  let js = main_source src in
  assert_bool "a literal comparison folds the whole conditional away"
    (contains ~needle:"const r = 2;" js)

let test_constant_folding_concat _ =
  let src = {|
s : String
s = "4" ++ "2"
|} in
  assert_js ~src ~expr:"Main.s" ~expected:{|"42"|};
  let js = main_source src in
  assert_bool "literal string concatenation is folded"
    (contains ~needle:{|const s = "42";|} js)

let test_constant_propagation _ =
  let src =
    {|
y : Int
y =
    let
        a = 2
    in
    a * 3
|}
  in
  assert_js ~src ~expr:"Main.y" ~expected:"6";
  let js = main_source src in
  assert_bool "the constant is propagated into its uses"
    (contains ~needle:"const y = 6;" js);
  assert_bool "the propagated binding is gone"
    (not (contains ~needle:"const a =" js))

let test_dead_let_eliminated _ =
  let src =
    {|
keep : Int -> Int
keep w =
    let
        unused = w + 1
    in
    7
|}
  in
  let js = main_source src in
  assert_bool "an unused pure binding is dropped"
    (not (contains ~needle:"unused" js))

let test_dead_let_keeps_calls _ =
  let src =
    {|
keep : String -> Int
keep w =
    let
        used = String.length w
    in
    used + 1
|}
  in
  assert_js ~src ~expr:{|Main.keep("abc")|} ~expected:"4";
  let js = main_source src in
  assert_bool "a call is not assumed pure, so its binding stays"
    (contains ~needle:"const used = $$String.length(w)" js)

let test_beta_reduction _ =
  let src = {|
beta : Int
beta = (\p -> p + 1) 41
|} in
  assert_js ~src ~expr:"Main.beta" ~expected:"42";
  let js = main_source src in
  assert_bool "an immediately applied lambda leaves no closure"
    (contains ~needle:"const beta = 42;" js)

let test_beta_reduction_avoids_capture _ =
  let src =
    {|
tricky : Int -> Int
tricky a =
    (\b c -> b + c) 10 (a + 1)

result : Int
result = tricky 5
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"16";
  assert_js ~src ~expr:"Main.tricky(5)" ~expected:"16"

let test_beta_reduction_shadowed_argument _ =
  let src =
    {|
tricky : Int -> Int
tricky a =
    (\a b -> a + b) 10 (a + 1)

result : Int
result = tricky 5
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"16";
  assert_js ~src ~expr:"Main.tricky(5)" ~expected:"16"

let test_inline_small_function _ =
  let src =
    {|
add : Int -> Int -> Int
add a b = a + b

sum : Int
sum = add 3 4
|}
  in
  assert_js ~src ~expr:"Main.sum" ~expected:"7";
  let js = main_source src in
  assert_bool "the small function is inlined at the call site"
    (not (contains ~needle:"add(3, 4)" js));
  assert_bool "the inlined call folds to a constant"
    (contains ~needle:"const sum = 7;" js);
  assert_bool "the function itself is still emitted"
    (contains ~needle:"const add = (a, b) =>" js)

let test_inline_with_dynamic_argument _ =
  let src =
    {|
double : Int -> Int
double n = n * 2

quad : Int -> Int
quad v = double (double v)

result : Int
result = quad 5
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"20";
  let js = main_source src in
  assert_bool "nested calls are inlined into plain bindings"
    (not (contains ~needle:"$$double(" js))

let test_inline_respects_shadowing _ =
  let src =
    {|
factor : Int
factor = 3

scale : Int -> Int
scale n = n * factor

result : Int
result =
    let
        factor = 10
    in
    scale 2
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"6"

let test_inline_keeps_partial_application _ =
  let src =
    {|
add3 : Int -> Int -> Int -> Int
add3 a b c = a + b + c

partial : Int -> Int
partial = add3 1 2

result : Int
result = partial 4
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"7"

let test_module_header_with_imports_compiles _ =
  let src =
    {|
module Main exposing (result)

import String as S exposing
    ( length
    , fromInt
    )
import Tuple

double : Int -> Int
double n = n * 2

result : Int
result = length (fromInt (S.length "abc")) + double 21
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"43"

let test_unit_value _ = assert_js ~src:"nothing = ()" ~expr:"Main.nothing" ~expected:"null"

let test_negation _ =
  let src =
    {|
value : Int
value = 3

negated : Int
negated = -value
|}
  in
  assert_js ~src ~expr:"Main.negated" ~expected:"-3"


let test_prelude_is_emitted_as_modules _ =
  let src = "y : Int\ny = String.length \"hello\"" in
  let emitted =
    List.map
      (fun (c : Dartea.Compiler.artifact) -> c.module_name)
      (compiled_of src)
  in
  List.iter
    (fun expected ->
      assert_bool (expected ^ " is compiled like any other module")
        (List.mem expected emitted))
    [ "Basics"; "Maybe"; "String"; "Tuple"; "Main" ];
  assert_bool "the consumer imports the prelude module it uses"
    (contains ~needle:{|import * as $$String from "./String.mjs";|}
       (main_source src));
  assert_js ~src ~expr:"Main.y" ~expected:"5"

let test_saturated_primitive_lowers_to_an_operation _ =
  let string_module = module_source ~name:"String" "x : Int\nx = 1" in
  assert_bool "a saturated primitive becomes the JS operation itself"
    (contains ~needle:"Number.isInteger(Number(" string_module);
  assert_bool "an unapplied primitive becomes an arrow"
    (contains ~needle:"const length = " string_module)

let test_prelude_value_is_shadowed_by_a_local_declaration _ =
  let src =
    {|
length : Int -> Int
length n =
    n + 1

result : Int
result = length 41
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"42";
  let js = main_source src in
  assert_bool "the local declaration wins over the prelude"
    (contains ~needle:"const length = n => n + 1;" js)

let test_maybe_module_round_trips _ =
  let src =
    {|
parsed : Int
parsed = Maybe.withDefault 0 (String.toInt "41")

missing : Int
missing = Maybe.withDefault 7 (String.toInt "nope")

sum : Int
sum = parsed + missing
|}
  in
  assert_js ~src ~expr:"Main.sum" ~expected:"48"

let test_tuple_module_round_trips _ =
  let src =
    {|
p : ( Int, String )
p = (1, "a")

result : Int
result = Tuple.first p
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"1";
  let js = main_source src in
  assert_bool "a tuple literal is an array, not a call to Tuple.pair"
    (contains ~needle:{|[1, "a"]|} js);
  assert_bool "a tuple literal needs no help from the Tuple module"
    (not (contains ~needle:"Tuple.pair" js))

let test_annotation_is_polymorphic _ =
  let src =
    {|
both : ( Int, String )
both =
    let
        i = identity
    in
    (i 1, i "str")

result : Int
result = Tuple.first both
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"1"


let test_runtime_is_imported_only_when_curried _ =
  let plain = "result : Int\nresult = 1 + 2" in
  assert_bool "a module that curries imports the runtime"
    (contains ~needle:{|import * as Dartea_runtime|}
       (module_source ~name:"Maybe" plain));
  assert_bool "a module that does not curry leaves the runtime alone"
    (not (contains ~needle:"Dartea_runtime" (main_source plain)));
  assert_bool "a string literal naming the runtime is not a reference"
    (not
       (contains ~needle:{|import * as Dartea_runtime|}
          (main_source {|note = "Dartea_runtime.$$curry"|})))


let test_kernel_application_lowers_to_an_operation _ =
  let src =
    {|
result : Int
result =
    let
        greeting = "hello"
    in
    Elm.Kernel.String.length greeting
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"5";
  let js = main_source src in
  assert_bool "a saturated kernel becomes the JS operation"
    (contains ~needle:{|"hello".length|} js);
  assert_bool "no arrow is built just to be called at once"
    (not (contains ~needle:"=> x.length" js))

let test_let_block_with_function_bindings_runs _ =
  let src =
    {|
result : Int
result =
    let
        double x = x + x

        shout n =
            double n + 1
    in
    shout 20
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"41"

let test_concrete_higher_order_call_is_direct _ =
  let src =
    {|
add : Int -> Int -> Int
add a b = a + b

twice : (Int -> Int) -> Int -> Int
twice f n =
    f (f n)

result : Int
result = twice (add 1) 5
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"7";
  let js = main_source src in
  assert_bool "a parameter of concrete function type is called directly"
    (not (contains ~needle:"$$curry" js))

let test_declaration_is_saturated_to_its_type_arity _ =
  let src =
    {|
adder : Int -> Int -> Int
adder n =
    let
        doubled = n + n
    in
    \m -> doubled + m

useAdder : (Int -> Int -> Int) -> Int
useAdder f =
    case f 3 4 of
        0 ->
            100

        n ->
            n + f 1 1

result : Int
result = useAdder adder
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"13";
  let js = main_source src in
  assert_bool "a declaration takes as many parameters as its type has arrows"
    (contains ~needle:"const adder = (n, " js);
  assert_bool "the added parameter reaches the body, not an applied arrow"
    (contains ~needle:"return doubled + " js)

let test_over_application_chains_calls _ =
  let src =
    {|
add : Int -> Int -> Int
add a b = a + b

result : Int
result = identity add 3 4
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"7";
  let js = main_source src in
  assert_bool "extra arguments are a second call, not a curry"
    (contains ~needle:"(add)(3, 4)" js)

let test_computed_callee_is_called_directly _ =
  let src =
    {|
add : Int -> Int -> Int
add a b = a + b

pick : Bool -> (Int -> Int) -> (Int -> Int) -> Int -> Int
pick c f g n =
    (if c then f else g) n

result : Int
result = pick True (add 1) (add 2) 5
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"6";
  let js = main_source src in
  assert_bool "a callee that is not an identifier still uses its type arity"
    (not (contains ~needle:"$$curry" js))

let test_function_survives_a_generic_slot _ =
  let src =
    {|
add : Int -> Int -> Int
add a b = a + b

box : (a -> b) -> Maybe (a -> b)
box f = Just f

result : Int
result =
    case box add of
        Just g ->
            g 1 2

        Nothing ->
            0
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"3"

let test_polymorphic_higher_order_still_curries _ =
  let src =
    {|
add : Int -> Int -> Int
add a b = a + b

apply : (a -> b) -> a -> b
apply f x = f x

result : Int
result = apply add 3 4
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"7";
  let js = main_source src in
  assert_bool "a call on a parameter of polymorphic type keeps the runtime"
    (contains ~needle:"$$curry" js)

let test_unapplied_kernel_becomes_an_arrow _ =
  let js = module_source ~name:"String" "x : Int\nx = 1" in
  assert_bool "an unapplied kernel is eta-expanded once, at its declaration"
    (contains ~needle:"const length = x => x.length;" js)

let test_comments_are_skipped _ =
  let src =
    {|
-- the module remark

{-| A block remark
    that spans lines.
-}
type Box
    = Box Int -- the only shape


unbox : Box -> Int
unbox b =
    {- outer {- inner -} outer -}
    case b of
        -- the only arm
        Box n ->
            n


result : Int
result =
    let
        one =
            unbox (Box 1)
                -- deeper than the block
        two =
            unbox (Box 2)
  -- shallower than the block
        three =
            unbox (Box 3)
    in
    one + two + three
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"6";
  let js = main_source src in
  assert_bool "comment text never reaches the output"
    (not (contains ~needle:"remark" js))

let test_comment_markers_inside_a_string_are_text _ =
  let src = {|
label : String
label = "-- not a comment {- either -}"
|} in
  assert_js ~src ~expr:"Main.label"
    ~expected:{|"-- not a comment {- either -}"|}

let test_operator_precedence _ =
  let src =
    {|
sum : Int
sum = 2 + 3 * 4

power : Int
power = 2 * 3 ^ 2

powerIsRightAssociative : Int
powerIsRightAssociative = 2 ^ 3 ^ 2

subtractionIsLeftAssociative : Int
subtractionIsLeftAssociative = 10 - 3 - 2

integerDivisionIsLeftAssociative : Int
integerDivisionIsLeftAssociative = 100 // 7 // 2

comparisonIsLooserThanArithmetic : Bool
comparisonIsLooserThanArithmetic = 1 + 2 == 3

andIsTighterThanOr : Bool
andIsTighterThanOr = True || False && False
|}
  in
  assert_js ~src ~expr:"Main.sum" ~expected:"14";
  assert_js ~src ~expr:"Main.power" ~expected:"18";
  assert_js ~src ~expr:"Main.powerIsRightAssociative" ~expected:"512";
  assert_js ~src ~expr:"Main.subtractionIsLeftAssociative" ~expected:"5";
  assert_js ~src ~expr:"Main.integerDivisionIsLeftAssociative" ~expected:"7";
  assert_js ~src ~expr:"Main.comparisonIsLooserThanArithmetic" ~expected:"true";
  assert_js ~src ~expr:"Main.andIsTighterThanOr" ~expected:"true"

let test_integer_division_follows_elm_core _ =
  let src =
    {|
exact : Int
exact = 7 // 2

truncatesTowardsZero : Int
truncatesTowardsZero = negate 7 // 2

byZero : Int
byZero = 1 // 0
|}
  in
  assert_js ~src ~expr:"Main.exact" ~expected:"3";
  assert_js ~src ~expr:"Main.truncatesTowardsZero" ~expected:"-3";
  assert_js ~src ~expr:"Main.byZero" ~expected:"0";
  let js = main_source src in
  assert_bool "integer division lowers to elm/core's (a / b) | 0"
    (contains ~needle:"| 0" js)

let test_division_follows_elm_core _ =
  let src =
    {|
half : Float
half = 7 / 2

floored : Int
floored = 7 // 2

negative : Int
negative = -7 // 2
|}
  in
  assert_js ~src ~expr:"Main.half" ~expected:"3.5";
  assert_js ~src ~expr:"Main.floored" ~expected:"3";
  assert_js ~src ~expr:"Main.negative" ~expected:"-3";
  let js = main_source src in
  assert_bool "float division no longer truncates"
    (not (contains ~needle:"Math.trunc" js))

let test_numeric_literals_are_number_constrained _ =
  let src =
    {|
mixed : Float
mixed = 1 + 1.5

scaled : Float
scaled = 2 * 1.25

counted : Int
counted = 1 + 2
|}
  in
  assert_js ~src ~expr:"Main.mixed" ~expected:"2.5";
  assert_js ~src ~expr:"Main.scaled" ~expected:"2.5";
  assert_js ~src ~expr:"Main.count_of" ~expected:"3"

let test_append_is_one_function_over_strings_and_lists _ =
  let src =
    {|
glue : appendable -> appendable -> appendable
glue a b = a ++ b

greeting : String
greeting = glue "he" "llo"

joined : List Int
joined = glue [ 1, 2 ] [ 3 ]

nested : List (List Int)
nested = glue [ [ 1 ] ] [ [ 2 ], [ 3 ] ]
|}
  in
  assert_js ~src ~expr:"Main.greeting" ~expected:{|"hello"|};
  assert_js ~src ~expr:"Main.joined"
    ~expected:{|{"hd":1,"tl":{"hd":2,"tl":{"hd":3,"tl":0}}}|};
  assert_js ~src ~expr:"Main.nested.tl.hd.hd" ~expected:"2";
  assert_js ~src ~expr:"Main.nested.tl.tl.hd.hd" ~expected:"3"

let test_append_on_strings_stays_a_plus _ =
  let src = {|
direct : String
direct = "a" ++ "b"

grown : String -> String
grown s = s ++ "!"
|} in
  let js = main_source src in
  assert_bool "a string append does not reach the runtime"
    (not (contains ~needle:"$$append" js));
  assert_js ~src ~expr:{|Main.grown("hi")|} ~expected:{|"hi!"|}

let test_append_on_lists_does_not_mutate_its_left_side _ =
  let src =
    {|
left : List Int
left = [ 1, 2 ]

both : List Int
both = left ++ [ 3 ]
|}
  in
  assert_js ~src ~expr:"Main.left" ~expected:{|{"hd":1,"tl":{"hd":2,"tl":0}}|};
  assert_js ~src ~expr:"Main.both"
    ~expected:{|{"hd":1,"tl":{"hd":2,"tl":{"hd":3,"tl":0}}}|}

let test_equality_is_structural _ =
  let src =
    {|
type Shape
    = Dot
    | Line Int Int

lists : Bool
lists = [ 1, 2 ] == [ 1, 2 ]

listsDiffer : Bool
listsDiffer = [ 1, 2 ] == [ 1, 3 ]

tuples : Bool
tuples = ( 1, "a" ) == ( 1, "a" )

records : Bool
records = { x = 1, y = "a" } == { x = 1, y = "a" }

recordsDiffer : Bool
recordsDiffer = { x = 1, y = "a" } == { x = 2, y = "a" }

payloads : Bool
payloads = Line 1 2 == Line 1 2

payloadsDiffer : Bool
payloadsDiffer = Line 1 2 == Line 1 3

nullary : Bool
nullary = Dot == Dot

nested : Bool
nested = [ { x = 1 } ] == [ { x = 1 } ]

nestedDiffer : Bool
nestedDiffer = [ { x = 1 } ] == [ { x = 2 } ]

notEqual : Bool
notEqual = [ 1, 2 ] /= [ 1, 3 ]

notEqualSame : Bool
notEqualSame = [ 1, 2 ] /= [ 1, 2 ]
|}
  in
  assert_js ~src ~expr:"Main.lists" ~expected:"true";
  assert_js ~src ~expr:"Main.listsDiffer" ~expected:"false";
  assert_js ~src ~expr:"Main.tuples" ~expected:"true";
  assert_js ~src ~expr:"Main.records" ~expected:"true";
  assert_js ~src ~expr:"Main.recordsDiffer" ~expected:"false";
  assert_js ~src ~expr:"Main.payloads" ~expected:"true";
  assert_js ~src ~expr:"Main.payloadsDiffer" ~expected:"false";
  assert_js ~src ~expr:"Main.nullary" ~expected:"true";
  assert_js ~src ~expr:"Main.nested" ~expected:"true";
  assert_js ~src ~expr:"Main.nestedDiffer" ~expected:"false";
  assert_js ~src ~expr:"Main.notEqual" ~expected:"true";
  assert_js ~src ~expr:"Main.notEqualSame" ~expected:"false"

let test_equality_on_primitives_stays_strict_equal _ =
  let src =
    {|
counted : Int -> Bool
counted n = n == 1

named : String -> Bool
named s = s == "a"

flagged : Bool -> Bool
flagged b = b == True

lettered : Char -> Bool
lettered c = c == 'a'
|}
  in
  let js = main_source src in
  assert_bool "a primitive equality does not reach the runtime"
    (not (contains ~needle:"$$eq" js));
  assert_bool "a primitive equality is still ===" (contains ~needle:"===" js);
  assert_js ~src ~expr:"Main.count_of(1)" ~expected:"true";
  assert_js ~src ~expr:{|Main.lettered("a")|} ~expected:"true"

let test_equality_over_a_deeply_nested_value _ =
  let src =
    {|
deep : List (List (List Int))
deep = [ [ [ 1, 2 ], [ 3 ] ], [ [ 4 ] ] ]

same : Bool
same = deep == [ [ [ 1, 2 ], [ 3 ] ], [ [ 4 ] ] ]

different : Bool
different = deep == [ [ [ 1, 2 ], [ 3 ] ], [ [ 5 ] ] ]
|}
  in
  assert_js ~src ~expr:"Main.same" ~expected:"true";
  assert_js ~src ~expr:"Main.different" ~expected:"false"

let test_equality_through_a_polymorphic_slot _ =
  let src =
    {|
same : a -> a -> Bool
same x y = x == y

lists : Bool
lists = same [ 1, 2 ] [ 1, 2 ]

numbers : Bool
numbers = same 1 1
|}
  in
  assert_js ~src ~expr:"Main.lists" ~expected:"true";
  assert_js ~src ~expr:"Main.numbers" ~expected:"true"

let test_comparison_is_structural _ =
  let src =
    {|
lists : Bool
lists = [ 1, 2 ] < [ 1, 3 ]

listsPrefix : Bool
listsPrefix = [ 1 ] < [ 1, 2 ]

nilIsSmallest : Bool
nilIsSmallest = [] < [ 1 ]

tuples : Bool
tuples = ( 1, "b" ) > ( 1, "a" )

tuplesEqual : Bool
tuplesEqual = ( 1, "a" ) >= ( 1, "a" )

nested : Bool
nested = [ ( 1, 'a' ) ] <= [ ( 1, 'b' ) ]
|}
  in
  assert_js ~src ~expr:"Main.lists" ~expected:"true";
  assert_js ~src ~expr:"Main.listsPrefix" ~expected:"true";
  assert_js ~src ~expr:"Main.nilIsSmallest" ~expected:"true";
  assert_js ~src ~expr:"Main.tuples" ~expected:"true";
  assert_js ~src ~expr:"Main.tuplesEqual" ~expected:"true";
  assert_js ~src ~expr:"Main.nested" ~expected:"true"

let test_comparison_on_primitives_stays_an_operator _ =
  let src =
    {|
smaller : Int -> Bool
smaller n = n < 1

earlier : String -> Bool
earlier s = s < "m"

lettered : Char -> Bool
lettered c = c < 'm'
|}
  in
  let js = main_source src in
  assert_bool "a primitive comparison does not reach the runtime"
    (not (contains ~needle:"$$cmp" js));
  assert_js ~src ~expr:{|Main.earlier("a")|} ~expected:"true";
  assert_js ~src ~expr:{|Main.lettered("z")|} ~expected:"false"

let test_compare_returns_an_order _ =
  let src =
    {|
lower : Order
lower = compare 1 2

same : Order
same = compare "a" "a"

higher : Order
higher = compare [ 2 ] [ 1 ]

described : Order -> String
described order =
    case order of
        LT ->
            "lt"

        EQ ->
            "eq"

        GT ->
            "gt"

spoken : String
spoken = described (compare ( 1, 2 ) ( 1, 3 ))
|}
  in
  assert_js ~src ~expr:"Main.lower" ~expected:{|"LT"|};
  assert_js ~src ~expr:"Main.same" ~expected:{|"EQ"|};
  assert_js ~src ~expr:"Main.higher" ~expected:{|"GT"|};
  assert_js ~src ~expr:"Main.spoken" ~expected:{|"lt"|}

let test_min_and_max_are_written_in_the_language _ =
  let src =
    {|
lower : Int
lower = min 4 2

upper : String
upper = max "a" "b"

shortest : List Int
shortest = min [ 1, 2 ] [ 1 ]

warmest : Float
warmest = max 1.5 2.5
|}
  in
  assert_js ~src ~expr:"Main.lower" ~expected:"2";
  assert_js ~src ~expr:"Main.upper" ~expected:{|"b"|};
  assert_js ~src ~expr:"Main.shortest" ~expected:{|{"hd":1,"tl":0}|};
  assert_js ~src ~expr:"Main.warmest" ~expected:"2.5"

let test_sorting_a_pair_through_compare _ =
  let src =
    {|
ordered : ( Int, Int ) -> ( Int, Int )
ordered pair =
    case pair of
        ( a, b ) ->
            case compare a b of
                GT ->
                    ( b, a )

                LT ->
                    ( a, b )

                EQ ->
                    ( a, b )

swapped : ( Int, Int )
swapped = ordered ( 5, 3 )

kept : ( Int, Int )
kept = ordered ( 3, 5 )
|}
  in
  assert_js ~src ~expr:"Main.swapped" ~expected:"[3,5]";
  assert_js ~src ~expr:"Main.kept" ~expected:"[3,5]"

let test_equality_specialises_records_and_tuples _ =
  let src =
    {|
type alias Point =
    { x : Int
    , y : String
    }

samePoint : Point -> Point -> Bool
samePoint a b = a == b

samePair : ( Int, String ) -> ( Int, String ) -> Bool
samePair a b = a == b

sameList : List Int -> List Int -> Bool
sameList a b = a == b

sameAny : a -> a -> Bool
sameAny a b = a == b
|}
  in
  let js = main_source src in
  let line_of name =
    match
      List.find_opt
        (fun line -> contains ~needle:("const " ^ name ^ " =") line)
        (String.split_on_char '\n' js)
    with
    | Some line -> line
    | None -> assert_failure (name ^ " was not emitted")
  in
  assert_bool "a record equality compares its fields by name"
    (contains ~needle:".x === " (line_of "samePoint"));
  assert_bool "a record equality does not reach the runtime"
    (not (contains ~needle:"$$eq" (line_of "samePoint")));
  assert_bool "a tuple equality does not reach the runtime"
    (not (contains ~needle:"$$eq" (line_of "samePair")));
  assert_bool "a list equality uses the instance generated for its element"
    (contains ~needle:"$eq$List$Int" (line_of "sameList"));
  assert_bool "a list equality does not reach the runtime"
    (not (contains ~needle:"$$eq" (line_of "sameList")));
  assert_bool "only a type variable is left on the runtime instance"
    (contains ~needle:"$$eq" (line_of "sameAny"));
  assert_js ~src
    ~expr:"Main.sameList({ hd: 1, tl: 0 }, { hd: 1, tl: 0 })"
    ~expected:"true";
  assert_js ~src
    ~expr:"Main.sameList({ hd: 1, tl: 0 }, { hd: 2, tl: 0 })"
    ~expected:"false";
  assert_js ~src ~expr:"Main.sameList({ hd: 1, tl: 0 }, 0)" ~expected:"false";
  assert_js ~src
    ~expr:{|Main.samePoint({ x: 1, y: "a" }, { x: 1, y: "a" })|}
    ~expected:"true";
  assert_js ~src
    ~expr:{|Main.samePoint({ x: 1, y: "a" }, { x: 1, y: "b" })|}
    ~expected:"false";
  assert_js ~src ~expr:{|Main.samePair([1, "a"], [1, "a"])|} ~expected:"true";
  assert_js ~src ~expr:{|Main.samePair([1, "a"], [2, "a"])|} ~expected:"false"

let test_ordering_specialises_tuples_lexicographically _ =
  let src =
    {|
before : ( Int, String ) -> ( Int, String ) -> Bool
before a b = a < b

atMost : ( Int, String ) -> ( Int, String ) -> Bool
atMost a b = a <= b
|}
  in
  let js = main_source src in
  assert_bool "a tuple ordering does not reach the runtime"
    (not (contains ~needle:"$$cmp" js));
  assert_js ~src ~expr:{|Main.before([1, "a"], [1, "b"])|} ~expected:"true";
  assert_js ~src ~expr:{|Main.before([1, "b"], [1, "a"])|} ~expected:"false";
  assert_js ~src ~expr:{|Main.before([1, "z"], [2, "a"])|} ~expected:"true";
  assert_js ~src ~expr:{|Main.atMost([1, "a"], [1, "a"])|} ~expected:"true";
  assert_js ~src ~expr:{|Main.atMost([2, "a"], [1, "a"])|} ~expected:"false"

let test_specialised_comparison_evaluates_each_operand_once _ =
  let src =
    {|
type alias Counted =
    { value : Int
    }

made : Int -> Counted
made n = { value = n }

same : Bool
same = made 1 == made 1

different : Bool
different = made 1 == made 2
|}
  in
  let js = main_source src in
  assert_bool "an expanded comparison binds its operands to temporaries"
    (contains ~needle:"const $s" js);
  assert_js ~src ~expr:"Main.same" ~expected:"true";
  assert_js ~src ~expr:"Main.different" ~expected:"false"

let test_deep_nesting_falls_back_to_the_runtime _ =
  let src =
    {|
wide : ( Int, Int, Int ) -> ( Int, Int, Int ) -> Bool
wide a b = a == b

deep : ( ( Int, Int ), ( Int, Int ) ) -> ( ( Int, Int ), ( Int, Int ) ) -> Bool
deep a b = a == b
|}
  in
  assert_js ~src ~expr:"Main.wide([1, 2, 3], [1, 2, 3])" ~expected:"true";
  assert_js ~src ~expr:"Main.wide([1, 2, 3], [1, 2, 4])" ~expected:"false";
  assert_js ~src ~expr:"Main.deep([[1, 2], [3, 4]], [[1, 2], [3, 4]])"
    ~expected:"true";
  assert_js ~src ~expr:"Main.deep([[1, 2], [3, 4]], [[1, 2], [3, 5]])"
    ~expected:"false"

let test_equality_specialises_custom_types _ =
  let src =
    {|
type Shape
    = Dot
    | Line Int Int
    | Named String

type Color
    = Red
    | Green

type Tree
    = Leaf
    | Node Tree Int Tree

type Boxed
    = Boxed Int

sameShape : Shape -> Shape -> Bool
sameShape a b = a == b

sameColor : Color -> Color -> Bool
sameColor a b = a == b

sameTree : Tree -> Tree -> Bool
sameTree a b = a == b

sameBoxed : Boxed -> Boxed -> Bool
sameBoxed a b = a == b

sameMaybe : Maybe Int -> Maybe Int -> Bool
sameMaybe a b = a == b

sameShapes : List Shape -> List Shape -> Bool
sameShapes a b = a == b

checks : List Bool
checks =
    [ sameShape Dot Dot
    , sameShape (Line 1 2) (Line 1 2)
    , sameShape (Line 1 2) (Line 1 3)
    , sameShape Dot (Line 1 2)
    , sameShape (Named "a") (Named "a")
    , sameShape (Named "a") (Line 1 2)
    , sameColor Red Red
    , sameColor Red Green
    , sameTree (Node Leaf 1 Leaf) (Node Leaf 1 Leaf)
    , sameTree (Node Leaf 1 Leaf) (Node Leaf 2 Leaf)
    , sameTree Leaf Leaf
    , sameBoxed (Boxed 1) (Boxed 1)
    , sameMaybe (Just 1) (Just 1)
    , sameMaybe (Just 1) Nothing
    , sameMaybe Nothing Nothing
    , sameShapes [ Dot, Line 1 2 ] [ Dot, Line 1 2 ]
    , sameShapes [ Dot ] [ Dot, Line 1 2 ]
    ]
|}
  in
  let js = main_source src in
  assert_bool "a custom type gets its own equality instance"
    (contains ~needle:"const $eq$Shape =" js);
  assert_bool "a recursive type gets a self-recursive instance"
    (contains ~needle:"const $eq$Tree =" js);
  assert_bool "an all-nullary type compares in place, with no instance at all"
    (not (contains ~needle:"$eq$Color" js));
  assert_bool "a module comparing only concrete types never loads the runtime"
    (not (contains ~needle:"Dartea_runtime" js));
  assert_js ~src ~expr:"Main.checks"
    ~expected:
      (let flags =
         [
           true; true; false; false; true; false; true; false; true; false;
           true; true; true; false; true; true; false;
         ]
       in
       List.fold_right
         (fun flag rest ->
           Printf.sprintf {|{"hd":%b,"tl":%s}|} flag rest)
         flags "0")

let test_numeric_primitives_follow_elm_core _ =
  let src =
    {|
area : Float -> Float
area r = pi * r * r

circle : Float
circle = area 2.0

roundTrip : Int
roundTrip = round (toFloat 7 / 2.0)

rounded : ( Int, Int, Int )
rounded = ( floor 1.8, ceiling 1.2, truncate (-1.8) )

roots : Float
roots = sqrt 16.0

logs : Float
logs = logBase 2.0 256.0

mods : ( Int, Int )
mods = ( modBy 4 (-5), remainderBy 4 (-5) )

absolutes : ( Int, Float )
absolutes = ( abs (-4), abs (-8.5) )

clamped : Int
clamped = clamp 100 200 250

angles : Float
angles = degrees 180

flags : ( Bool, Bool, Bool )
flags = ( not True, xor True False, isNaN (0.0 / 0.0) )

napier : Bool
napier = e > 2.718 && e < 2.719
|}
  in
  assert_js ~src ~expr:"Main.circle" ~expected:"12.566370614359172";
  assert_js ~src ~expr:"Main.roundTrip" ~expected:"4";
  assert_js ~src ~expr:"Main.rounded" ~expected:"[1,2,-1]";
  assert_js ~src ~expr:"Main.roots" ~expected:"4";
  assert_js ~src ~expr:"Main.logs" ~expected:"8";
  assert_js ~src ~expr:"Main.mods" ~expected:"[3,-1]";
  assert_js ~src ~expr:"Main.absolutes" ~expected:"[4,8.5]";
  assert_js ~src ~expr:"Main.clamped" ~expected:"200";
  assert_js ~src ~expr:"Main.angles" ~expected:"3.141592653589793";
  assert_js ~src ~expr:"Main.flags" ~expected:"[false,true,true]";
  assert_js ~src ~expr:"Main.napier" ~expected:"true"

let test_pi_is_a_nullary_kernel _ =
  let src = {|
half : Float
half = pi / 2.0
|} in
  let js = main_source src in
  assert_bool "pi lowers to Math.PI without a call"
    (contains ~needle:"Math.PI" (module_source ~name:"Basics" src));
  assert_bool "the module reaches pi through Basics"
    (contains ~needle:"Basics.pi" js);
  assert_js ~src ~expr:"Main.half" ~expected:"1.5707963267948966"

let test_char_module_handles_the_whole_of_unicode _ =
  let src =
    {|
import Char


emoji : Int
emoji = Char.toCode '\u{1F600}'

letter : Int
letter = Char.toCode 'A'

tree : Int
tree = Char.toCode '木'

back : Char
back = Char.fromCode 0x1F600

roundTrip : Bool
roundTrip = Char.fromCode (Char.toCode '\u{1F600}') == '\u{1F600}'

replacement : Bool
replacement = Char.fromCode (-1) == Char.fromCode 0xFFFD

classes : ( Bool, Bool, Bool )
classes = ( Char.isUpper 'A', Char.isDigit '7', Char.isHexDigit 'f' )

notClasses : ( Bool, Bool, Bool )
notClasses = ( Char.isUpper 'a', Char.isOctDigit '8', Char.isAlpha '-' )

cased : ( Char, Char )
cased = ( Char.toUpper 'a', Char.toLower 'Z' )
|}
  in
  assert_js ~src ~expr:"Main.emoji" ~expected:"128512";
  assert_js ~src ~expr:"Main.letter" ~expected:"65";
  assert_js ~src ~expr:"Main.tree" ~expected:"26408";
  assert_js ~src ~expr:"Main.back" ~expected:{|"😀"|};
  assert_js ~src ~expr:"Main.roundTrip" ~expected:"true";
  assert_js ~src ~expr:"Main.replacement" ~expected:"true";
  assert_js ~src ~expr:"Main.classes" ~expected:"[true,true,true]";
  assert_js ~src ~expr:"Main.notClasses" ~expected:"[false,false,false]";
  assert_js ~src ~expr:"Main.cased" ~expected:{|["A","z"]|}

let test_a_character_literal_holds_one_code_point _ =
  let src =
    {|
import Char


greek : Int
greek = Char.toCode 'Σ'

accented : Int
accented = Char.toCode 'é'
|}
  in
  assert_js ~src ~expr:"Main.greek" ~expected:"931";
  assert_js ~src ~expr:"Main.accented" ~expected:"233"

let test_string_carries_floats_both_ways _ =
  let src =
    {|
shown : String
shown = String.fromFloat 1.5

whole : String
whole = String.fromFloat 2.0

read : Maybe Float
read = String.toFloat "1.5"

refused : Maybe Float
refused = String.toFloat "abc"

doubled : Float
doubled =
    case String.toFloat "1.25" of
        Just value ->
            value * 2.0

        Nothing ->
            0.0
|}
  in
  assert_js ~src ~expr:"Main.shown" ~expected:{|"1.5"|};
  assert_js ~src ~expr:"Main.whole" ~expected:{|"2"|};
  assert_js ~src ~expr:"Main.read" ~expected:{|{"_0":1.5}|};
  assert_js ~src ~expr:"Main.refuse" ~expected:{|"Nothing"|};
  assert_js ~src ~expr:"Main.doubled" ~expected:"2.5"

let test_the_type_system_chapter_holds_together _ =
  let src =
    {|
import Char


area : Float -> Float
area radius =
    pi * radius * radius


roundedArea : Float -> Int
roundedArea radius =
    round (area radius)


backAndForth : Int -> Int
backAndForth n =
    round (toFloat n / 2.0) * 2


ordered : ( comparable, comparable ) -> ( comparable, comparable )
ordered pair =
    case pair of
        ( left, right ) ->
            case compare left right of
                GT ->
                    ( right, left )

                LT ->
                    ( left, right )

                EQ ->
                    ( left, right )


type alias Point =
    { x : Int
    , y : Int
    }


glue : appendable -> appendable -> appendable
glue one other =
    one ++ other


circleArea : Float
circleArea = area 2.0

roundedCircle : Int
roundedCircle = roundedArea 2.0

halved : Int
halved = backAndForth 7

sortedNumbers : ( Int, Int )
sortedNumbers = ordered ( 5, 3 )

sortedWords : ( String, String )
sortedWords = ordered ( "pear", "apple" )

samePoint : Bool
samePoint = { x = 1, y = 2 } == { x = 1, y = 2 }

differentPoint : Bool
differentPoint = { x = 1, y = 2 } == { x = 1, y = 3 }

samePoints : Bool
samePoints = [ { x = 1, y = 2 } ] == [ { x = 1, y = 2 } ]

differentPoints : Bool
differentPoints = [ { x = 1, y = 2 } ] == [ { x = 9, y = 2 } ]

joinedWords : String
joinedWords = glue "hello " "world"

joinedNumbers : List Int
joinedNumbers = glue [ 1, 2 ] [ 3 ]

grinning : Int
grinning = Char.toCode '\u{1F600}'
|}
  in
  assert_js ~src ~expr:"Main.circleArea" ~expected:"12.566370614359172";
  assert_js ~src ~expr:"Main.roundedCircle" ~expected:"13";
  assert_js ~src ~expr:"Main.halved" ~expected:"8";
  assert_js ~src ~expr:"Main.sortedNumbers" ~expected:"[3,5]";
  assert_js ~src ~expr:"Main.sortedWords" ~expected:{|["apple","pear"]|};
  assert_js ~src ~expr:"Main.samePoint" ~expected:"true";
  assert_js ~src ~expr:"Main.differentPoint" ~expected:"false";
  assert_js ~src ~expr:"Main.samePoints" ~expected:"true";
  assert_js ~src ~expr:"Main.differentPoints" ~expected:"false";
  assert_js ~src ~expr:"Main.joinedWords" ~expected:{|"hello world"|};
  assert_js ~src ~expr:"Main.joinedNumbers"
    ~expected:{|{"hd":1,"tl":{"hd":2,"tl":{"hd":3,"tl":0}}}|};
  assert_js ~src ~expr:"Main.grinning" ~expected:"128512"

let test_class_methods_resolve_at_the_call_site _ =
  let src =
    {|
type alias Point =
    { x : Int
    , y : Int
    }

lower : Int
lower = min 4 2

upper : String
upper = max "a" "b"

ordered : Order
ordered = compare ( 1, "b" ) ( 1, "a" )

listed : Order
listed = compare [ 1, 2 ] [ 1, 3 ]

held : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
held a b = min a b

generic : comparable -> comparable -> comparable
generic a b = min a b
|}
  in
  let js = main_source src in
  let line_of name =
    match
      List.find_opt
        (fun line -> contains ~needle:("const " ^ name ^ " =") line)
        (String.split_on_char '\n' js)
    with
    | Some line -> line
    | None -> assert_failure (name ^ " was not emitted")
  in
  assert_bool "min at a known type becomes a conditional"
    (contains ~needle:"(4 < 2)" (line_of "lower"));
  assert_bool "max at a known type becomes a conditional"
    (not (contains ~needle:"$$cmp" (line_of "upper")));
  assert_bool "min on a tuple does not reach the runtime"
    (not (contains ~needle:"$$cmp" (line_of "held")));
  assert_bool "only a bare comparable is left on the runtime instance"
    (contains ~needle:"$$cmp" (line_of "generic"));
  assert_js ~src ~expr:"Main.lower" ~expected:"2";
  assert_js ~src ~expr:"Main.upper" ~expected:{|"b"|};
  assert_js ~src ~expr:"Main.ordered" ~expected:{|"GT"|};
  assert_js ~src ~expr:"Main.listed" ~expected:{|"LT"|};
  assert_js ~src ~expr:"Main.held([1, 2], [1, 3])" ~expected:"[1,2]"

let test_a_number_variable_compares_in_place _ =
  let src =
    {|
sameNumbers : Bool
sameNumbers = [ 1, 2 ] == [ 1, 2 ]

orderedNumbers : Bool
orderedNumbers = [ 1, 2 ] < [ 1, 3 ]

orderedWords : Bool
orderedWords = [ "x", "y" ] < [ "x", "z" ]
|}
  in
  let js = main_source src in
  assert_bool "an unresolved number is still compared without the runtime"
    (not (contains ~needle:"$$cmp" js || contains ~needle:"$$eq" js));
  assert_js ~src ~expr:"Main.sameNumbers" ~expected:"true";
  assert_js ~src ~expr:"Main.orderedNumbers" ~expected:"true";
  assert_js ~src ~expr:"Main.orderedWords" ~expected:"true"

let test_exponent_lowers_to_the_js_operator _ =
  let src = {|
cube : Int
cube = 3 ^ 3
|} in
  assert_js ~src ~expr:"Main.cube" ~expected:"27";
  let js = main_source src in
  assert_bool "exponentiation lowers to **" (contains ~needle:"**" js)

let test_apply_left _ =
  let src =
    {|
double : Int -> Int
double n = n * 2

viaOperator : Int
viaOperator = double <| 1 + 2

isRightAssociative : Int
isRightAssociative = double <| double <| 5
|}
  in
  assert_js ~src ~expr:"Main.viaOperator" ~expected:"6";
  assert_js ~src ~expr:"Main.isRightAssociative" ~expected:"20"

let test_composition _ =
  let src =
    {|
double : Int -> Int
double n = n * 2

increment : Int -> Int
increment n = n + 1

left : Int
left = (double << increment) 5

right : Int
right = (double >> increment) 5

leftIsLeftAssociative : Int
leftIsLeftAssociative = (double << increment << increment) 5

rightIsRightAssociative : Int
rightIsRightAssociative = (increment >> increment >> double) 5

piped : Int
piped = 5 |> increment >> double
|}
  in
  assert_js ~src ~expr:"Main.left" ~expected:"12";
  assert_js ~src ~expr:"Main.right" ~expected:"11";
  assert_js ~src ~expr:"Main.leftIsLeftAssociative" ~expected:"14";
  assert_js ~src ~expr:"Main.rightIsRightAssociative" ~expected:"14";
  assert_js ~src ~expr:"Main.piped" ~expected:"12"

let test_operator_as_a_value _ =
  let src =
    {|
applyBinary : (Int -> Int -> Int) -> Int -> Int -> Int
applyBinary f a b = f a b

added : Int
added = applyBinary (+) 3 4

multiplied : Int
multiplied = applyBinary (*) 3 4

dividedExactly : Int
dividedExactly = applyBinary (//) 9 2

raised : Int
raised = applyBinary (^) 2 5

subtracted : Int
subtracted = applyBinary (-) 9 2

joined : String
joined = (++) "a" "b"

double : Int -> Int
double n = n * 2

increment : Int -> Int
increment n = n + 1

composed : Int
composed = (<<) double increment 5

pipedForward : Int
pipedForward = (|>) 5 double

appliedLeft : Int
appliedLeft = (<|) double 5
|}
  in
  assert_js ~src ~expr:"Main.added" ~expected:"7";
  assert_js ~src ~expr:"Main.multiplied" ~expected:"12";
  assert_js ~src ~expr:"Main.dividedExactly" ~expected:"4";
  assert_js ~src ~expr:"Main.raised" ~expected:"32";
  assert_js ~src ~expr:"Main.subtracted" ~expected:"7";
  assert_js ~src ~expr:"Main.joined" ~expected:{|"ab"|};
  assert_js ~src ~expr:"Main.composed" ~expected:"12";
  assert_js ~src ~expr:"Main.pipedForward" ~expected:"10";
  assert_js ~src ~expr:"Main.appliedLeft" ~expected:"10"

let test_cons_in_expressions _ =
  let src =
    {|
sum : List Int -> Int
sum xs =
    case xs of
        [] ->
            0

        h :: t ->
            h + sum t

prepended : List Int
prepended = 1 :: 2 :: [ 3, 4 ]

total : Int
total = sum prepended

ontoEmpty : List Int
ontoEmpty = 5 :: []

single : Int
single = sum ontoEmpty

build : Int -> List Int -> List Int
build n rest = n :: rest

built : Int
built = sum (build 7 (build 8 []))
|}
  in
  assert_js ~src ~expr:"Main.total" ~expected:"10";
  assert_js ~src ~expr:"Main.single" ~expected:"5";
  assert_js ~src ~expr:"Main.built" ~expected:"15";
  let js = main_source src in
  assert_bool "cons builds the same cell shape as a list literal"
    (contains ~needle:"hd:" js)

let test_cons_is_right_associative _ =
  let src =
    {|
head : List Int -> Int
head xs =
    case xs of
        [] ->
            0

        h :: _ ->
            h

tail : List Int -> List Int
tail xs =
    case xs of
        [] ->
            []

        _ :: t ->
            t

chain : List Int
chain = 1 :: 2 :: 3 :: []

second : Int
second = head (tail chain)

appendedThenConsed : List Int
appendedThenConsed = 1 :: []

firstOfAppended : Int
firstOfAppended = head appendedThenConsed
|}
  in
  assert_js ~src ~expr:"Main.second" ~expected:"2";
  assert_js ~src ~expr:"Main.firstOfAppended" ~expected:"1"

let test_triples _ =
  let src =
    {|
point : ( Int, Int, Int )
point = ( 1, 2, 3 )

middle : ( Int, Int, Int ) -> Int
middle p =
    case p of
        ( _, y, _ ) ->
            y

result : Int
result = middle point

nested : ( Int, ( Int, Int ) )
nested = ( 1, ( 2, 3 ) )

inner : Int
inner =
    case nested of
        ( _, ( _, z ) ) ->
            z
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"2";
  assert_js ~src ~expr:"Main.inner" ~expected:"3";
  let js = main_source src in
  assert_bool "a triple is a three-element array"
    (contains ~needle:"[1, 2, 3]" js)

let test_tuple_pair_is_verbatim_elm _ =
  let src =
    {|
made : ( Int, String )
made = Tuple.pair 1 "a"

result : Int
result = Tuple.first made

swapped : String
swapped = Tuple.second (Tuple.pair 1 "a")
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"1";
  assert_js ~src ~expr:"Main.swapped" ~expected:{|"a"|};
  assert_bool "Tuple.pair no longer needs a kernel primitive"
    (not (contains ~needle:"Kernel" (module_source ~name:"Tuple" src)))

let test_record_update _ =
  let src =
    {|
type alias Model =
    { count : Int
    , name : String
    }

start : Model
start = { count = 0, name = "a" }

bumped : Model
bumped = { start | count = start.count + 1 }

renamed : Model
renamed = { start | count = 9, name = "b" }

increment : Model -> Model
increment model = { model | count = model.count + 1 }

twice : Int
twice = (increment (increment start)).count
|}
  in
  assert_js ~src ~expr:"Main.bumped.count" ~expected:"1";
  assert_js ~src ~expr:{|Main.bumped.name|} ~expected:{|"a"|};
  assert_js ~src ~expr:"Main.renamed.count" ~expected:"9";
  assert_js ~src ~expr:{|Main.renamed.name|} ~expected:{|"b"|};
  assert_js ~src ~expr:"Main.twice" ~expected:"2";
  assert_js ~src ~expr:"Main.start.count" ~expected:"0";
  let js = main_source src in
  assert_bool "a record update spreads the original"
    (contains ~needle:"...start" js)

let test_record_update_keeps_the_record_type _ =
  let src =
    {|
type alias Point =
    { x : Int
    , y : Int
    }

origin : Point
origin = { x = 0, y = 0 }

moveX : Int -> Point -> Point
moveX dx point = { point | x = point.x + dx }

sumOf : Point -> Int
sumOf point = point.x + point.y

result : Int
result = sumOf (moveX 5 origin)
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"5"

let test_wildcard_and_unit_parameters _ =
  let src =
    {|
type alias Model =
    { count : Int
    }

init : () -> Model
init () = { count = 0 }

view : Model -> Int
view _ = 42

started : Int
started = (init ()).count

seen : Int
seen = view (init ())

constant : Int -> Int -> Int
constant a _ = a

kept : Int
kept = constant 3 4
|}
  in
  assert_js ~src ~expr:"Main.started" ~expected:"0";
  assert_js ~src ~expr:"Main.seen" ~expected:"42";
  assert_js ~src ~expr:"Main.kept" ~expected:"3";
  let js = main_source src in
  assert_bool "an unconditional parameter pattern needs no case"
    (not (contains ~needle:"switch" js))

let test_destructuring_parameters _ =
  let src =
    {|
type alias Model =
    { count : Int
    , name : String
    }

fst : ( Int, Int ) -> Int
fst ( a, b ) = a

snd : ( Int, Int ) -> Int
snd ( a, b ) = b

counted : Model -> Int
counted { count } = count

both : ( Int, Int ) -> ( Int, Int ) -> Int
both ( a, b ) ( c, d ) = a + b + c + d

left : Int
left = fst ( 1, 2 )

right : Int
right = snd ( 1, 2 )

fromRecord : Int
fromRecord = counted { count = 7, name = "a" }

summed : Int
summed = both ( 1, 2 ) ( 3, 4 )

swap : ( Int, Int ) -> ( Int, Int )
swap ( a, b ) = ( b, a )

swapped : Int
swapped = fst (swap ( 1, 2 ))
|}
  in
  assert_js ~src ~expr:"Main.left" ~expected:"1";
  assert_js ~src ~expr:"Main.right" ~expected:"2";
  assert_js ~src ~expr:"Main.fromRecord" ~expected:"7";
  assert_js ~src ~expr:"Main.summed" ~expected:"10";
  assert_js ~src ~expr:"Main.swapped" ~expected:"2"

let test_destructuring_lambda_parameters _ =
  let src =
    {|
apply : (( Int, Int ) -> Int) -> ( Int, Int ) -> Int
apply f p = f p

added : Int
added = apply (\( a, b ) -> a + b) ( 3, 4 )

ignored : Int
ignored = (\_ -> 5) 9

unitTaken : Int
unitTaken = (\() -> 6) ()
|}
  in
  assert_js ~src ~expr:"Main.added" ~expected:"7";
  assert_js ~src ~expr:"Main.ignored" ~expected:"5";
  assert_js ~src ~expr:"Main.unitTaken" ~expected:"6"

let test_destructuring_let_binding_parameters _ =
  let src =
    {|
result : Int
result =
    let
        fst ( a, b ) =
            a

        ignore _ =
            10
    in
    fst ( 1, 2 ) + ignore 0
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"11"

let test_refutable_parameter_throws_at_runtime _ =
  let src =
    {|
unwrap : Maybe Int -> Int
unwrap (Just n) = n

result : Int
result = unwrap (Just 5)
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"5";
  let js = main_source src in
  assert_bool "a refutable parameter keeps the failing branch"
    (contains ~needle:"Pattern match failed" js)

let assert_rejected ~src ~message =
  match compiled_of src with
  | _ -> assert_failure message
  | exception _ -> assert_bool "rejected" true

let test_let_binding_annotation _ =
  let src =
    {|
type alias Model =
    { count : Int
    }

result : Int
result =
    let
        start : Model
        start =
            { count = 4 }

        step : Int -> Int
        step n =
            n + 1

        twice : Int -> Int
        twice n =
            step (step n)
    in
    twice start.count

polymorphic : ( Int, String )
polymorphic =
    let
        keep : a -> a
        keep x =
            x
    in
    ( keep 1, keep "a" )
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"6";
  assert_js ~src ~expr:"Main.polymorphic[0]" ~expected:"1";
  assert_js ~src ~expr:"Main.polymorphic[1]" ~expected:{|"a"|}

let test_let_binding_annotation_is_checked _ =
  assert_rejected
    ~src:
      {|
result : Int
result =
    let
        y : Int
        y = "not an int"
    in
    1
|}
    ~message:"a let annotation that contradicts the body must be an error"

let test_destructuring_let_bindings _ =
  let src =
    {|
type alias Model =
    { count : Int
    , name : String
    }

pair : ( Int, Int )
pair = ( 3, 4 )

model : Model
model = { count = 7, name = "a" }

summed : Int
summed =
    let
        ( a, b ) = pair
    in
    a + b

counted : Int
counted =
    let
        { count } = model
    in
    count

mixed : Int
mixed =
    let
        ( a, b ) = pair

        c = 1

        { count } = model
    in
    a + b + c + count
|}
  in
  assert_js ~src ~expr:"Main.summed" ~expected:"7";
  assert_js ~src ~expr:"Main.count_of" ~expected:"7";
  assert_js ~src ~expr:"Main.mixed" ~expected:"15"

let test_alias_patterns _ =
  let src =
    {|
firstAndWhole : List Int -> Int
firstAndWhole xs =
    case xs of
        (h :: _) as whole ->
            h + length whole

        [] ->
            0

length : List Int -> Int
length xs =
    case xs of
        [] ->
            0

        _ :: t ->
            1 + length t

result : Int
result = firstAndWhole (5 :: 6 :: [])

type Shape = Circle Int | Square Int

named : Shape -> Int
named s =
    case s of
        (Circle r) as kept ->
            r + area kept

        Square a ->
            a

area : Shape -> Int
area s =
    case s of
        Circle r ->
            r * 3

        Square a ->
            a * a

shaped : Int
shaped = named (Circle 2)
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"7";
  assert_js ~src ~expr:"Main.shaped" ~expected:"8"

let test_negative_literal_patterns _ =
  let src =
    {|
describe : Int -> Int
describe n =
    case n of
        -1 ->
            100

        0 ->
            200

        1 ->
            300

        _ ->
            400

negative : Int
negative = describe (0 - 1)

zero : Int
zero = describe 0

other : Int
other = describe 7
|}
  in
  assert_js ~src ~expr:"Main.negative" ~expected:"100";
  assert_js ~src ~expr:"Main.zero" ~expected:"200";
  assert_js ~src ~expr:"Main.other" ~expected:"400"

let test_char_literal_has_char_type _ =
  let src =
    {|
letter : Char
letter = 'a'

quoted : Char
quoted = '\''

newline : Char
newline = '\n'

emoji : Char
emoji = '\u{1F600}'

classify : Char -> Int
classify c =
    case c of
        'a' ->
            1

        'b' ->
            2

        _ ->
            0

first : Int
first = classify 'a'

second : Int
second = classify 'b'

other : Int
other = classify 'z'
|}
  in
  assert_js ~src ~expr:"Main.letter" ~expected:{|"a"|};
  assert_js ~src ~expr:"Main.quoted" ~expected:{|"'"|};
  assert_js ~src ~expr:"Main.newline.charCodeAt(0)" ~expected:"10";
  assert_js ~src ~expr:"Main.emoji.codePointAt(0)" ~expected:"128512";
  assert_js ~src ~expr:"Main.first" ~expected:"1";
  assert_js ~src ~expr:"Main.second" ~expected:"2";
  assert_js ~src ~expr:"Main.other" ~expected:"0"

let test_block_strings _ =
  let src =
    {|
plain : String
plain = """no escaping needed for " here"""

across : String
across = """first
second"""

escaped : String
escaped = """tab\there"""
|}
  in
  assert_js ~src ~expr:"Main.plain.length" ~expected:"29";
  assert_js ~src ~expr:{|Main.across|} ~expected:{|"first\nsecond"|};
  assert_js ~src ~expr:"Main.escaped" ~expected:{|"tab\there"|}

let test_hexadecimal_literals _ =
  let src =
    {|
small : Int
small = 0x1F

upper : Int
upper = 0xFF

grouped : Int
grouped = 0xFF_FF
|}
  in
  assert_js ~src ~expr:"Main.small" ~expected:"31";
  assert_js ~src ~expr:"Main.upper" ~expected:"255";
  assert_js ~src ~expr:"Main.grouped" ~expected:"65535"

let test_minus_before_a_number_is_subtraction _ =
  let src = {|
gap : Int
gap = 5 -2

spaced : Int
spaced = 5 - 2
|} in
  assert_js ~src ~expr:"Main.gap" ~expected:"3";
  assert_js ~src ~expr:"Main.spaced" ~expected:"3"

let test_float_literals_print_as_javascript _ =
  let src = {|
whole = 2.0

fraction = 1.5

exponent = 1.0e10
|} in
  let js = main_source src in
  assert_bool "a whole float keeps its decimal part"
    (contains ~needle:"= 2.0;" js);
  assert_bool "a fractional float is printed as written"
    (contains ~needle:"= 1.5;" js);
  assert_bool "no float is printed with a trailing dot"
    (not (contains ~needle:".;" js));
  assert_js ~src ~expr:"Main.whole" ~expected:"2";
  assert_js ~src ~expr:"Main.fraction" ~expected:"1.5"

let test_float_annotation_is_a_real_type _ =
  let src =
    {|
half : Float
half = 0.5

point : Float
point = 2.0

pair : ( Float, Char )
pair = ( 1.5, 'q' )
|}
  in
  assert_js ~src ~expr:"Main.half" ~expected:"0.5";
  assert_js ~src ~expr:"Main.point" ~expected:"2";
  assert_js ~src ~expr:"Main.pair" ~expected:{|[1.5,"q"]|}

let test_char_case_over_a_char_annotation _ =
  let src =
    {|
vowel : Char -> Bool
vowel c =
    case c of
        'a' ->
            True

        'e' ->
            True

        _ ->
            False

yes : Bool
yes = vowel 'e'

no : Bool
no = vowel 'z'
|}
  in
  assert_js ~src ~expr:"Main.yes" ~expected:"true";
  assert_js ~src ~expr:"Main.no" ~expected:"false"

let test_record_alias_constructor _ =
  let src =
    {|
type alias Model =
    { count : Int
    , name : String
    }

start : Model
start = Model 0 "a"

renamed : Model
renamed = Model 5 "b"

build : Int -> Model
build n = Model n "made"

counted : Int
counted = (build 9).count

named : String
named = renamed.name

type alias Pair a =
    { first : a
    , second : a
    }

ints : Pair Int
ints = Pair 1 2

sum : Int
sum = ints.first + ints.second
|}
  in
  assert_js ~src ~expr:"Main.start.count" ~expected:"0";
  assert_js ~src ~expr:"Main.renamed.name" ~expected:{|"b"|};
  assert_js ~src ~expr:"Main.count_of" ~expected:"9";
  assert_js ~src ~expr:"Main.named" ~expected:{|"b"|};
  assert_js ~src ~expr:"Main.sum" ~expected:"3"

let test_record_alias_constructor_is_checked _ =
  assert_rejected
    ~src:
      {|
type alias Model =
    { count : Int
    , name : String
    }

wrong : Model
wrong = Model "a" 0
|}
    ~message:"the alias constructor must take the fields in declaration order"

let test_accessor_as_a_function _ =
  let src =
    {|
type alias Model =
    { count : Int
    , name : String
    }

apply : (Model -> Int) -> Model -> Int
apply f m = f m

model : Model
model = Model 3 "a"

viaAccessor : Int
viaAccessor = apply .count model

viaField : Int
viaField = model.count

composed : Model -> Int
composed = .count >> (\n -> n + 1)

bumped : Int
bumped = composed model

parenthesised : Int
parenthesised = (.count) model
|}
  in
  assert_js ~src ~expr:"Main.viaAccessor" ~expected:"3";
  assert_js ~src ~expr:"Main.viaField" ~expected:"3";
  assert_js ~src ~expr:"Main.bumped" ~expected:"4";
  assert_js ~src ~expr:"Main.parenthesised" ~expected:"3"

let test_tea_shaped_module _ =
  let src =
    {|
module Main exposing (start, afterOne, afterMany, log, label)

type Msg
    = Increment
    | Decrement
    | Rename String


type alias Model =
    { count : Int
    , name : String
    , history : List String
    }


init : () -> Model
init () =
    Model 0 "counter" []


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1, history = "up" :: model.history }

        Decrement ->
            { model | count = model.count - 1, history = "down" :: model.history }

        Rename newName ->
            { model | name = newName, history = newName :: model.history }


applyAll : List Msg -> Model -> Model
applyAll messages model =
    case messages of
        [] ->
            model

        msg :: rest ->
            applyAll rest (update msg model)


describe : Model -> String
describe model =
    let
        shown : Int
        shown =
            model.count
    in
    model.name ++ ":" ++ String.fromInt shown


view : Model -> String
view =
    .name >> String.append "model "


count : List String -> Int
count entries =
    case entries of
        [] ->
            0

        _ :: rest ->
            1 + count rest


start : Model
start =
    init ()


afterOne : String
afterOne =
    describe (update Increment start)


afterMany : String
afterMany =
    Increment :: Increment :: Decrement :: Rename "renamed" :: []
        |> (\msgs -> applyAll msgs start)
        |> describe


log : Int
log =
    count (applyAll (Increment :: Increment :: []) start).history


label : String
label =
    view start
|}
  in
  assert_js ~src ~expr:"Main.start.count" ~expected:"0";
  assert_js ~src ~expr:"Main.afterOne" ~expected:{|"counter:1"|};
  assert_js ~src ~expr:"Main.afterMany" ~expected:{|"renamed:1"|};
  assert_js ~src ~expr:"Main.log" ~expected:"2";
  assert_js ~src ~expr:"Main.label" ~expected:{|"model counter"|}

let test_a_later_callee_specialises_the_caller _ =
  let src =
    {|
isPalindrome n = reverse n == n

reverse n =
    let
        go m acc = if m == 0 then acc else go (quot m 10) (acc * 10 + rem m 10)
    in
    go n 0

quot a b = a // b

rem a b = a - quot a b * b

result = isPalindrome 12321

different = isPalindrome 12345
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"true";
  assert_js ~src ~expr:"Main.different" ~expected:"false";
  assert_bool "comparing two Ints does not go through the runtime"
    (not (contains ~needle:"$$eq" (main_source src)))

let test_mutual_recursion_without_annotations_runs _ =
  let src =
    {|
isEven n = if n == 0 then True else isOdd (n - 1)

isOdd n = if n == 0 then False else isEven (n - 1)

result = isEven 10

odd = isOdd 10
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"true";
  assert_js ~src ~expr:"Main.odd" ~expected:"false"

let test_inlining_keeps_the_call_site_type _ =
  let src =
    {|
smallest : comparable -> comparable -> comparable
smallest one other =
    if one < other then
        one

    else
        other

result : Bool
result = smallest "b" "a" == "a"
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"true";
  assert_bool "the inlined comparison of two strings is emitted inline"
    (contains
       ~needle:{|const result = (("b" < "a") ? "b" : "a") === "a";|}
       (main_source src))

let test_inlining_specialises_an_unconstrained_variable _ =
  let src =
    {|
same : a -> a -> Bool
same one other =
    one == other

result : Bool
result = same (String.length "abcd") 4
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"true";
  assert_bool "the inlined equality on Ints is emitted inline"
    (contains ~needle:"const result = one$1 === 4;" (main_source src))

let test_a_polymorphic_definition_still_uses_the_runtime _ =
  let src =
    {|
same : a -> a -> Bool
same one other =
    one == other

result : Bool
result = same [ 1, 2 ] [ 1, 2 ]
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"true";
  assert_bool "structural equality on lists stays on the runtime"
    (contains ~needle:"$$eq" (main_source src))

let test_a_payload_comparison_is_primitive _ =
  let src =
    {|
type Msg = Increment Int | Reset

same : Msg -> Msg -> Bool
same left right =
    case ( left, right ) of
        ( Increment x, Increment y ) ->
            x == y

        _ ->
            False

result : Bool
result = same (Increment 2) (Increment 2)
|}
  in
  let js = main_source src in
  assert_js ~src ~expr:"Main.result" ~expected:"true";
  assert_bool "a payload known to be Int compares with ==="
    (contains ~needle:"x === y" js);
  assert_bool "no runtime equality on a known payload"
    (not (contains ~needle:"$$eq" js))

let test_a_destructured_payload_keeps_its_type _ =
  let src =
    {|
type Box = Box Int

doubled : Box -> Int
doubled (Box n) =
    n + n

result : Int
result = doubled (Box 21)
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"42"

let test_a_tuple_pattern_keeps_each_position _ =
  let src =
    {|
result : String
result =
    case ( 1, "a" ) of
        ( a, b ) ->
            b
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:{|"a"|}

let test_a_unit_parameter_still_takes_an_argument _ =
  let src = {|
answer : () -> Int
answer () =
    42

result : Int
result = answer ()
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"42"

let test_list_folds_and_maps _ =
  let src =
    {|
result : Int
result =
    List.sum (List.map (\n -> n * n) (List.filter (\n -> n > 2) (List.range 1 5)))
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"50"

let test_list_sort_is_stable_by_key _ =
  let src =
    {|
result : String
result =
    List.foldr (\n acc -> String.fromInt n ++ acc) ""
        (List.sortBy negate [ 3, 1, 2 ])
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:{|"321"|}

let test_list_head_of_empty_is_nothing _ =
  let src =
    {|
result : String
result =
    case List.head (List.filter (\n -> n > 9) (List.range 1 5)) of
        Just n ->
            String.fromInt n

        Nothing ->
            "nothing"
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:{|"nothing"|}

let test_list_partition_splits_on_the_test _ =
  let src =
    {|
result : Int
result =
    case List.partition (\n -> n > 2) (List.range 1 5) of
        ( yes, no ) ->
            List.length yes * 10 + List.length no
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"32"

let test_result_carries_the_error_through _ =
  let src =
    {|
result : String
result =
    case Result.map2 (\a b -> a + b) (Ok 1) (Err "boom") of
        Ok n ->
            String.fromInt n

        Err e ->
            e
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:{|"boom"|}

let test_result_maps_and_chains _ =
  let src =
    {|
result : Int
result =
    Result.withDefault 0
        (Result.andThen (\n -> Ok (n * 2)) (Result.map negate (Ok 4)))
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"-8"

let test_string_splits_and_joins _ =
  let src =
    {|
result : String
result =
    String.join "-" (String.split "," "a,b,c")
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:{|"a-b-c"|}

let test_string_round_trips_through_a_char_list _ =
  let src =
    {|
result : String
result =
    String.fromList (List.reverse (String.toList "abc"))
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:{|"cba"|}

let test_string_slices_from_the_end _ =
  let src =
    {|
result : String
result =
    String.right 2 "abcdef" ++ String.slice 1 3 "abcdef"
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:{|"efbc"|}

let test_string_words_ignores_runs_of_space _ =
  let src =
    {|
result : Int
result =
    List.length (String.words "  one   two  three ")
|}
  in
  assert_js ~src ~expr:"Main.result" ~expected:"3"

let suite =
  [
    "result_carries_the_error_through" >:: test_result_carries_the_error_through;
    "result_maps_and_chains" >:: test_result_maps_and_chains;
    "string_splits_and_joins" >:: test_string_splits_and_joins;
    "string_round_trips_through_a_char_list"
    >:: test_string_round_trips_through_a_char_list;
    "string_slices_from_the_end" >:: test_string_slices_from_the_end;
    "string_words_ignores_runs_of_space"
    >:: test_string_words_ignores_runs_of_space;
    "list_folds_and_maps" >:: test_list_folds_and_maps;
    "list_sort_is_stable_by_key" >:: test_list_sort_is_stable_by_key;
    "list_head_of_empty_is_nothing" >:: test_list_head_of_empty_is_nothing;
    "list_partition_splits_on_the_test"
    >:: test_list_partition_splits_on_the_test;
    "comments_are_skipped" >:: test_comments_are_skipped;
    "comment_markers_inside_a_string_are_text"
    >:: test_comment_markers_inside_a_string_are_text;
    "operator_precedence" >:: test_operator_precedence;
    "integer_division_follows_elm_core"
    >:: test_integer_division_follows_elm_core;
    "division_follows_elm_core"
    >:: test_division_follows_elm_core;
    "numeric_literals_are_number_constrained"
    >:: test_numeric_literals_are_number_constrained;
    "append_is_one_function_over_strings_and_lists"
    >:: test_append_is_one_function_over_strings_and_lists;
    "append_on_strings_stays_a_plus" >:: test_append_on_strings_stays_a_plus;
    "append_on_lists_does_not_mutate_its_left_side"
    >:: test_append_on_lists_does_not_mutate_its_left_side;
    "equality_is_structural" >:: test_equality_is_structural;
    "equality_on_primitives_stays_strict_equal"
    >:: test_equality_on_primitives_stays_strict_equal;
    "equality_over_a_deeply_nested_value"
    >:: test_equality_over_a_deeply_nested_value;
    "equality_through_a_polymorphic_slot"
    >:: test_equality_through_a_polymorphic_slot;
    "comparison_is_structural" >:: test_comparison_is_structural;
    "comparison_on_primitives_stays_an_operator"
    >:: test_comparison_on_primitives_stays_an_operator;
    "compare_returns_an_order" >:: test_compare_returns_an_order;
    "min_and_max_are_written_in_the_language"
    >:: test_min_and_max_are_written_in_the_language;
    "sorting_a_pair_through_compare" >:: test_sorting_a_pair_through_compare;
    "equality_specialises_records_and_tuples"
    >:: test_equality_specialises_records_and_tuples;
    "ordering_specialises_tuples_lexicographically"
    >:: test_ordering_specialises_tuples_lexicographically;
    "specialised_comparison_evaluates_each_operand_once"
    >:: test_specialised_comparison_evaluates_each_operand_once;
    "deep_nesting_falls_back_to_the_runtime"
    >:: test_deep_nesting_falls_back_to_the_runtime;
    "equality_specialises_custom_types"
    >:: test_equality_specialises_custom_types;
    "numeric_primitives_follow_elm_core"
    >:: test_numeric_primitives_follow_elm_core;
    "pi_is_a_nullary_kernel" >:: test_pi_is_a_nullary_kernel;
    "char_module_handles_the_whole_of_unicode"
    >:: test_char_module_handles_the_whole_of_unicode;
    "a_character_literal_holds_one_code_point"
    >:: test_a_character_literal_holds_one_code_point;
    "string_carries_floats_both_ways" >:: test_string_carries_floats_both_ways;
    "the_type_system_chapter_holds_together"
    >:: test_the_type_system_chapter_holds_together;
    "class_methods_resolve_at_the_call_site"
    >:: test_class_methods_resolve_at_the_call_site;
    "a_number_variable_compares_in_place"
    >:: test_a_number_variable_compares_in_place;
    "exponent_lowers_to_the_js_operator"
    >:: test_exponent_lowers_to_the_js_operator;
    "apply_left" >:: test_apply_left;
    "composition" >:: test_composition;
    "operator_as_a_value" >:: test_operator_as_a_value;
    "cons_in_expressions" >:: test_cons_in_expressions;
    "cons_is_right_associative" >:: test_cons_is_right_associative;
    "triples" >:: test_triples;
    "tuple_pair_is_verbatim_elm" >:: test_tuple_pair_is_verbatim_elm;
    "a_later_callee_specialises_the_caller"
    >:: test_a_later_callee_specialises_the_caller;
    "mutual_recursion_without_annotations_runs"
    >:: test_mutual_recursion_without_annotations_runs;
    "inlining_keeps_the_call_site_type"
    >:: test_inlining_keeps_the_call_site_type;
    "inlining_specialises_an_unconstrained_variable"
    >:: test_inlining_specialises_an_unconstrained_variable;
    "a_polymorphic_definition_still_uses_the_runtime"
    >:: test_a_polymorphic_definition_still_uses_the_runtime;
    "record_update" >:: test_record_update;
    "record_update_keeps_the_record_type"
    >:: test_record_update_keeps_the_record_type;
    "wildcard_and_unit_parameters" >:: test_wildcard_and_unit_parameters;
    "destructuring_parameters" >:: test_destructuring_parameters;
    "destructuring_lambda_parameters"
    >:: test_destructuring_lambda_parameters;
    "destructuring_let_binding_parameters"
    >:: test_destructuring_let_binding_parameters;
    "refutable_parameter_throws_at_runtime"
    >:: test_refutable_parameter_throws_at_runtime;
    "let_binding_annotation" >:: test_let_binding_annotation;
    "let_binding_annotation_is_checked"
    >:: test_let_binding_annotation_is_checked;
    "destructuring_let_bindings" >:: test_destructuring_let_bindings;
    "alias_patterns" >:: test_alias_patterns;
    "negative_literal_patterns" >:: test_negative_literal_patterns;
    "char_literal_has_char_type" >:: test_char_literal_has_char_type;
    "block_strings" >:: test_block_strings;
    "hexadecimal_literals" >:: test_hexadecimal_literals;
    "minus_before_a_number_is_subtraction"
    >:: test_minus_before_a_number_is_subtraction;
    "float_literals_print_as_javascript"
    >:: test_float_literals_print_as_javascript;
    "float_annotation_is_a_real_type" >:: test_float_annotation_is_a_real_type;
    "char_case_over_a_char_annotation" >:: test_char_case_over_a_char_annotation;
    "record_alias_constructor" >:: test_record_alias_constructor;
    "record_alias_constructor_is_checked"
    >:: test_record_alias_constructor_is_checked;
    "accessor_as_a_function" >:: test_accessor_as_a_function;
    "tea_shaped_module" >:: test_tea_shaped_module;
    "arithmetic" >:: test_arithmetic;
    "unit_value" >:: test_unit_value;
    "negation" >:: test_negation;
    "module_header_with_imports_compiles"
    >:: test_module_header_with_imports_compiles;
    "prelude_is_emitted_as_modules" >:: test_prelude_is_emitted_as_modules;
    "runtime_is_imported_only_when_curried"
    >:: test_runtime_is_imported_only_when_curried;
    "kernel_application_lowers_to_an_operation"
    >:: test_kernel_application_lowers_to_an_operation;
    "unapplied_kernel_becomes_an_arrow"
    >:: test_unapplied_kernel_becomes_an_arrow;
    "let_block_with_function_bindings_runs"
    >:: test_let_block_with_function_bindings_runs;
    "concrete_higher_order_call_is_direct"
    >:: test_concrete_higher_order_call_is_direct;
    "declaration_is_saturated_to_its_type_arity"
    >:: test_declaration_is_saturated_to_its_type_arity;
    "over_application_chains_calls" >:: test_over_application_chains_calls;
    "computed_callee_is_called_directly"
    >:: test_computed_callee_is_called_directly;
    "function_survives_a_generic_slot" >:: test_function_survives_a_generic_slot;
    "polymorphic_higher_order_still_curries"
    >:: test_polymorphic_higher_order_still_curries;
    "saturated_primitive_lowers_to_an_operation"
    >:: test_saturated_primitive_lowers_to_an_operation;
    "prelude_value_is_shadowed_by_a_local_declaration"
    >:: test_prelude_value_is_shadowed_by_a_local_declaration;
    "maybe_module_round_trips" >:: test_maybe_module_round_trips;
    "tuple_module_round_trips" >:: test_tuple_module_round_trips;
    "annotation_is_polymorphic" >:: test_annotation_is_polymorphic;
    "constant_folding" >:: test_constant_folding;
    "constant_folding_comparison" >:: test_constant_folding_comparison;
    "constant_folding_concat" >:: test_constant_folding_concat;
    "constant_propagation" >:: test_constant_propagation;
    "dead_let_eliminated" >:: test_dead_let_eliminated;
    "dead_let_keeps_calls" >:: test_dead_let_keeps_calls;
    "beta_reduction" >:: test_beta_reduction;
    "beta_reduction_avoids_capture" >:: test_beta_reduction_avoids_capture;
    "beta_reduction_shadowed_argument" >:: test_beta_reduction_shadowed_argument;
    "inline_small_function" >:: test_inline_small_function;
    "inline_with_dynamic_argument" >:: test_inline_with_dynamic_argument;
    "inline_respects_shadowing" >:: test_inline_respects_shadowing;
    "inline_keeps_partial_application" >:: test_inline_keeps_partial_application;
    "builtin_direct" >:: test_builtin_direct;
    "unused_prelude_module_not_imported"
    >:: test_unused_prelude_module_not_imported;
    "complete_variant_no_default" >:: test_complete_variant_no_default;
    "pba_picks_needed_column" >:: test_pba_picks_needed_column;
    "shared_default_subtree" >:: test_shared_default_subtree;
    "wildcard_default_share" >:: test_wildcard_default_share;
    "var_binds_scrutinee" >:: test_var_binds_scrutinee;
    "decision_tree_nested" >:: test_decision_tree_nested;
    "int_literal_switch" >:: test_int_literal_switch;
    "payload_tag_switch" >:: test_payload_tag_switch;
    "nested_cons_bind" >:: test_nested_cons_bind;
    "ctor_unwrap" >:: test_ctor_unwrap;
    "nullary_match" >:: test_nullary_match;
    "ctor_is_defined" >:: test_ctor_is_defined;
    "nested_pattern" >:: test_nested_pattern;
    "nested_distinguishes" >:: test_nested_distinguishes;
    "let_in" >:: test_let_in;
    "if_then_else" >:: test_if_then_else;
    "comparison" >:: test_comparison;
    "fn_with_let" >:: test_fn_with_let;
    "prefers_const_arrow" >:: test_prefers_const_arrow;
    "reserved_words" >:: test_reserved_words;
    "nested_let_hoist" >:: test_nested_let_hoist;
    "switch_dispatch" >:: test_switch_dispatch;
    "partial_application" >:: test_partial_application;
    "direct_call" >:: test_direct_call;
    "shadowing" >:: test_shadowing;
    "list_patterns" >:: test_list_patterns;
    "cons_pattern" >:: test_cons_pattern;
    "tail_call_loop" >:: test_tail_call_loop;
    "value_position_match" >:: test_value_position_match;
    "tag_omission_single_payload" >:: test_tag_omission_single_payload;
    "tag_omission_single_ctor" >:: test_tag_omission_single_ctor;
    "tag_kept_for_multi_payload" >:: test_tag_kept_for_multi_payload;
    "a_payload_comparison_is_primitive"
    >:: test_a_payload_comparison_is_primitive;
    "a_destructured_payload_keeps_its_type"
    >:: test_a_destructured_payload_keeps_its_type;
    "a_tuple_pattern_keeps_each_position"
    >:: test_a_tuple_pattern_keeps_each_position;
    "a_unit_parameter_still_takes_an_argument"
    >:: test_a_unit_parameter_still_takes_an_argument;
  ]
