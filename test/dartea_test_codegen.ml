open OUnit2

let read_all ic =
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with End_of_file -> ());
  Buffer.contents buf

let node_eval ~src ~expr : string =
  let js = Dartea.Compiler.compile_string src in
  let program = js ^ "\nconsole.log(JSON.stringify(" ^ expr ^ "));\n" in
  let fname = Filename.temp_file "dartea_codegen" ".js" in
  let oc = open_out fname in
  output_string oc program;
  close_out oc;
  let ic = Unix.open_process_in ("node " ^ Filename.quote fname ^ " 2>&1") in
  let out = read_all ic in
  let _ = Unix.close_process_in ic in
  Sys.remove fname;
  String.trim out

let assert_js ~src ~expr ~expected =
  assert_equal ~printer:(fun s -> s) expected (node_eval ~src ~expr)

let contains ~needle haystack =
  let nl = String.length needle and hl = String.length haystack in
  let rec go i =
    i + nl <= hl && (String.sub haystack i nl = needle || go (i + 1))
  in
  go 0

let test_arithmetic _ = assert_js ~src:"x = 1 + 2 * 3" ~expr:"x" ~expected:"7"

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
  assert_js ~src ~expr:"result" ~expected:"42"

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
  assert_js ~src ~expr:"result" ~expected:"2"

let test_ctor_is_defined _ =
  let src = {|
type Wrap = Wrap Int

w : Wrap
w = Wrap 7
|} in
  assert_js ~src ~expr:"w._0" ~expected:"7"

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
  assert_js ~src ~expr:"result" ~expected:"5"

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
  assert_js ~src ~expr:"result" ~expected:"2"

let test_let_in _ =
  let src = {|
result : Int
result =
    let
        y = 5
    in
    y + 1
|} in
  assert_js ~src ~expr:"result" ~expected:"6";
  let js = Dartea.Compiler.compile_string src in
  assert_bool "value local should hoist (const y)"
    (contains ~needle:"const y = 5" js);
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
  assert_js ~src ~expr:"result" ~expected:"10"

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
  assert_js ~src ~expr:"result" ~expected:"1"

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
  assert_js ~src ~expr:"result" ~expected:"8"

let test_prefers_const_arrow _ =
  let src = {|
add : Int -> Int -> Int
add a b = a + b
|} in
  let js = Dartea.Compiler.compile_string src in
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
  assert_js ~src ~expr:"result" ~expected:"3";
  let js = Dartea.Compiler.compile_string src in

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
  assert_js ~src ~expr:"result" ~expected:"16";
  let js = Dartea.Compiler.compile_string src in
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
  assert_js ~src ~expr:"result" ~expected:"6";
  let js = Dartea.Compiler.compile_string src in
  assert_bool "reserved name should be legalized ($$default)"
    (contains ~needle:"$$default" js);
  assert_bool "no bare `const default`"
    (not (contains ~needle:"const default " js))

let test_partial_application _ =
  let src =
    {|
add : Int -> Int -> Int
add a b = a + b

inc : Int -> Int
inc = add 1

result : Int
result = inc 5
|}
  in
  assert_js ~src ~expr:"result" ~expected:"6";
  let js = Dartea.Compiler.compile_string src in

  assert_bool "partial application is an explicit closure"
    (contains ~needle:"=> add(1, " js)

let test_direct_call _ =
  let src =
    {|
add : Int -> Int -> Int
add a b = a + b

result : Int
result = add 3 4
|}
  in
  assert_js ~src ~expr:"result" ~expected:"7";
  let js = Dartea.Compiler.compile_string src in
  assert_bool "saturated call is direct (add(3, 4))"
    (contains ~needle:"add(3, 4)" js)

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
  assert_js ~src ~expr:"result" ~expected:"11"

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
  assert_js ~src ~expr:"result" ~expected:"21"

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
  assert_js ~src ~expr:"result" ~expected:"5000050000";
  let js = Dartea.Compiler.compile_string src in
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
  assert_js ~src ~expr:"result" ~expected:"7"

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
  assert_js ~src ~expr:"result" ~expected:"5"

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
  assert_js ~src ~expr:"result" ~expected:"107";
  let js = Dartea.Compiler.compile_string src in
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
  assert_js ~src ~expr:"result" ~expected:"200";
  let js = Dartea.Compiler.compile_string src in
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
  assert_js ~src ~expr:"result" ~expected:"7";
  let js = Dartea.Compiler.compile_string src in
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
  assert_js ~src ~expr:"result" ~expected:"7"

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
  assert_js ~src ~expr:"result" ~expected:"9"

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
  assert_js ~src ~expr:"result" ~expected:"42";
  let js = Dartea.Compiler.compile_string src in
  assert_bool "a whole-value var pattern binds directly with no test"
    (contains ~needle:"const m = n;" js)

let test_shared_default_subtree _ =
  let src =
    {|
type T = A | B | C

type W = L T | R T

fallback : Int -> Int
fallback n =
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
  assert_js ~src ~expr:"result" ~expected:"30";
  let js = Dartea.Compiler.compile_string src in
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
  assert_js ~src:(result "X X") ~expr:"r" ~expected:"1";
  assert_js ~src:(result "X Y") ~expr:"r" ~expected:"2";
  assert_js ~src:(result "Y X") ~expr:"r" ~expected:"3";
  assert_js ~src:(result "Y Y") ~expr:"r" ~expected:"2";
  let js = Dartea.Compiler.compile_string (result "X X") in
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
  assert_js ~src ~expr:"result" ~expected:"7";
  let js = Dartea.Compiler.compile_string src in
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
  assert_js ~src ~expr:"r1" ~expected:"6";
  assert_js ~src ~expr:"r2" ~expected:"10";
  let js = Dartea.Compiler.compile_string src in
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
  assert_js ~src ~expr:"result" ~expected:"42";
  let js = Dartea.Compiler.compile_string src in
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
  assert_js ~src ~expr:"result" ~expected:"9";
  let js = Dartea.Compiler.compile_string src in
  assert_bool "a type with several payload constructors keeps the TAG field"
    (contains ~needle:"TAG" js)

let test_builtin_direct _ =
  let src =
    {|
greeting : String
greeting = append (fromInt 4) "2"
|}
  in
  assert_js ~src ~expr:"greeting" ~expected:{|"42"|};
  let js = Dartea.Compiler.compile_string src in
  assert_bool "known-arity builtin is a direct call"
    (contains ~needle:"fromInt(4)" js);
  assert_bool "no $$curry for known-arity builtin"
    (not (contains ~needle:"$$curry(fromInt" js));
  assert_bool "used builtin is emitted" (contains ~needle:"const fromInt =" js)

let test_unused_builtin_absent _ =
  let js = Dartea.Compiler.compile_string "x : Int\nx = 1 + 2" in
  assert_bool "unused builtin not emitted"
    (not (contains ~needle:"const first =" js))

let suite =
  [
    "arithmetic" >:: test_arithmetic;
    "builtin_direct" >:: test_builtin_direct;
    "unused_builtin_absent" >:: test_unused_builtin_absent;
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
