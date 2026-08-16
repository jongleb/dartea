open OUnit2

let compiled_of src = Dartea.Compiler.compile_source src

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
      (fun (c : Dartea.Compiler.compiled) -> c.module_name)
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

let test_runtime_module_is_only_curry _ =
  let runtime =
    module_source ~name:"Dartea_runtime" "x : Int\nx = 1"
  in
  assert_bool "the runtime module still ships $$curry"
    (contains ~needle:"const $$curry =" runtime);
  List.iter
    (fun leftover ->
      assert_bool (leftover ^ " no longer lives in the runtime module")
        (not (contains ~needle:leftover runtime)))
    [ "const length"; "const fromInt"; "const pair"; "const Just" ]

let test_saturated_primitive_lowers_to_an_operation _ =
  let string_module = module_source ~name:"String" "x : Int\nx = 1" in
  assert_bool "a saturated primitive becomes the JS operation itself"
    (contains ~needle:"Number.isInteger(Number(string))" string_module);
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
  assert_bool "a tuple literal is Tuple.pair"
    (contains ~needle:"Tuple.pair(1, " (main_source src))

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
    (not (contains ~needle:"Dartea_runtime" (module_source ~name:"Basics" plain)));
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

let suite =
  [
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
    "runtime_module_is_only_curry" >:: test_runtime_module_is_only_curry;
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
  ]
