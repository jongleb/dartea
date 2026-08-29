open QCheck2
module I = Infer.Expressions
module Declarations = Infer.Declarations
module T = Typed.Type
module V = Typed.Variable
module Message = Reporting.Message
module Unify = Infer.Unify
module Types = Dartea_test_type_system
module Names = Set.Make (String)

let rec normalised ty =
  match T.head ty with
  | T.TFun (parameter, result) -> T.TFun (normalised parameter, normalised result)
  | T.TTup items -> T.TTup (List.map normalised items)
  | T.TCustom (name, arguments) -> T.TCustom (name, List.map normalised arguments)
  | T.TRecord row -> T.TRecord (normalised_row row)
  | T.TRowExtend _ as row -> normalised_row row
  | (T.TVar _ | T.TInt | T.TFloat | T.TChar | T.TStr | T.TBool | T.TUnit
    | T.TRowEmpty) as settled ->
      settled

and normalised_row row =
  let rec split row =
    match T.head row with
    | T.TRowExtend (label, typ, rest) ->
        let fields, tail = split rest in
        ((label, normalised typ) :: fields, tail)
    | tail -> ([], normalised tail)
  in
  let fields, tail = split row in
  List.fold_right
    (fun (label, typ) rest -> T.TRowExtend (label, typ, rest))
    (List.stable_sort (fun (left, _) (right, _) -> String.compare left right) fields)
    tail

let same left right = T.equal (normalised left) (normalised right)

let renaming () =
  let pairing = Hashtbl.create 16 in
  let matched = Hashtbl.create 16 in
  let paired one other =
    let here = V.identity one and there = V.identity other in
    match (Hashtbl.find_opt pairing here, Hashtbl.find_opt matched there) with
    | None, None ->
        Hashtbl.add pairing here there;
        Hashtbl.add matched there here;
        true
    | Some expected, Some expecting -> expected = there && expecting = here
    | Some _, None | None, Some _ -> false
  in
  let rec agree left right =
    match (left, right) with
    | T.TVar one, T.TVar other -> paired one other
    | T.TFun (parameter, result), T.TFun (other_parameter, other_result) ->
        agree parameter other_parameter && agree result other_result
    | T.TTup items, T.TTup other_items -> across items other_items
    | T.TCustom (name, arguments), T.TCustom (other_name, other_arguments) ->
        Data.Name.equal name other_name && across arguments other_arguments
    | T.TRecord row, T.TRecord other_row -> agree row other_row
    | T.TRowExtend (label, field, rest), T.TRowExtend (other_label, other_field, other_rest)
      ->
        String.equal label other_label
        && agree field other_field
        && agree rest other_rest
    | T.TInt, T.TInt | T.TFloat, T.TFloat | T.TChar, T.TChar | T.TStr, T.TStr
    | T.TBool, T.TBool | T.TUnit, T.TUnit | T.TRowEmpty, T.TRowEmpty ->
        true
    | ( ( T.TVar _ | T.TInt | T.TFloat | T.TChar | T.TStr | T.TBool | T.TUnit
        | T.TFun _ | T.TTup _ | T.TCustom _ | T.TRecord _ | T.TRowExtend _
        | T.TRowEmpty ),
        _ ) ->
        false
  and across items other_items =
    List.length items = List.length other_items
    && List.for_all2 agree items other_items
  in
  fun left right -> agree (normalised left) (normalised right)

let alpha_same left right = renaming () left right

let rec same_pattern agree (left : Typed.Pattern.t) (right : Typed.Pattern.t) =
  agree left.typ right.typ && same_binding agree left.pattern right.pattern

and same_binding agree left right =
  let open Typed.Pattern in
  let all one other =
    List.length one = List.length other
    && List.for_all2 (same_pattern agree) one other
  in
  match (left, right) with
  | P_T_anything, P_T_anything | P_T_unit, P_T_unit -> true
  | P_T_var one, P_T_var other -> String.equal one other
  | P_T_record one, P_T_record other -> List.equal String.equal one other
  | P_T_alias (one, name), P_T_alias (other, other_name) ->
      String.equal name other_name && same_pattern agree one other
  | P_T_tuple one, P_T_tuple other | P_T_list one, P_T_list other -> all one other
  | P_T_cons (head, tail), P_T_cons (other_head, other_tail) ->
      same_pattern agree head other_head && same_pattern agree tail other_tail
  | P_T_chr one, P_T_chr other | P_T_str one, P_T_str other ->
      String.equal one other
  | P_T_int one, P_T_int other -> Int.equal one other
  | P_T_ctor (name, arguments), P_T_ctor (other_name, other_arguments) ->
      Data.Name.equal name other_name && all arguments other_arguments
  | ( ( P_T_anything | P_T_var _ | P_T_record _ | P_T_alias _ | P_T_unit
      | P_T_tuple _ | P_T_list _ | P_T_cons _ | P_T_chr _ | P_T_str _
      | P_T_int _ | P_T_ctor _ ),
      _ ) ->
      false

let anywhere_region = Data.Region.nowhere

let unifies left right =
  try
    Unify.types ~region:anywhere_region ~category:Reporting.Category.Record
      ~expected:(Reporting.Expectation.No_expectation right) left;
    true
  with Reporting.Error.Found _ -> false

let written = Message.of_type
let written_pair (left, right) = written left ^ "  ~  " ^ written right

type sketch =
  | Sk_var of int
  | Sk_ground of T.t
  | Sk_fun of sketch * sketch
  | Sk_tup of sketch list
  | Sk_custom of string * sketch list
  | Sk_record of (string * sketch) list * sketch

let carried_by =
  [| None; None; None; Some Data.Constraint.Number;
     Some Data.Constraint.Comparable; Some Data.Constraint.Appendable |]

let builder () =
  let pool = Hashtbl.create 8 in
  let variable index =
    match Hashtbl.find_opt pool index with
    | Some variable -> variable
    | None ->
        let variable = V.fresh carried_by.(index mod Array.length carried_by) in
        Hashtbl.add pool index variable;
        variable
  in
  let rec build sketch =
    match sketch with
    | Sk_var index -> T.TVar (variable index)
    | Sk_ground settled -> settled
    | Sk_fun (parameter, result) -> T.TFun (build parameter, build result)
    | Sk_tup items -> T.TTup (List.map build items)
    | Sk_custom (name, arguments) ->
        T.TCustom (Data.Name.local name, List.map build arguments)
    | Sk_record (fields, tail) ->
        T.TRecord
          (List.fold_right
             (fun (label, field) rest -> T.TRowExtend (label, build field, rest))
             fields (build tail))
  in
  build

let materialise sketch = builder () sketch

let materialise_pair (left, right) =
  let build = builder () in
  (build left, build right)

let ground_gen =
  Gen.oneof_list
    [ T.TInt; T.TFloat; T.TChar; T.TStr; T.TBool; T.TUnit ]

let label_gen = Gen.oneof_list [ "one"; "two"; "three" ]
let custom_gen = Gen.oneof_list [ ("List", 1); ("Box", 0); ("Maybe", 1); ("Result", 2) ]

let keeping_first_label fields =
  List.rev
    (List.fold_left
       (fun kept (label, typ) ->
         if List.exists (fun (seen, _) -> String.equal seen label) kept then kept
         else (label, typ) :: kept)
       [] fields)

let rec sketch_gen depth =
  let leaf =
    Gen.oneof
      [
        Gen.map (fun settled -> Sk_ground settled) ground_gen;
        Gen.map (fun index -> Sk_var index) (Gen.int_bound 5);
      ]
  in
  if depth <= 0 then leaf
  else
    let smaller = sketch_gen (depth - 1) in
    Gen.oneof_weighted
      [
        (5, leaf);
        (2, Gen.map2 (fun parameter result -> Sk_fun (parameter, result)) smaller smaller);
        ( 2,
          Gen.map (fun items -> Sk_tup items)
            (Gen.list_size (Gen.int_range 2 3) smaller) );
        ( 2,
          Gen.bind custom_gen (fun (name, arity) ->
              Gen.map
                (fun arguments -> Sk_custom (name, arguments))
                (Gen.list_size (Gen.return arity) smaller)) );
        (1, row_sketch_gen smaller);
      ]

and row_sketch_gen field =
  Gen.map2
    (fun fields tail -> Sk_record (keeping_first_label fields, tail))
    (Gen.list_size (Gen.int_range 0 3) (Gen.pair label_gen field))
    (Gen.oneof
       [ Gen.return (Sk_ground T.TRowEmpty);
         Gen.map (fun index -> Sk_var (6 + index)) (Gen.int_bound 1) ])

let sketch_pair_gen = Gen.pair (sketch_gen 3) (sketch_gen 3)
let type_gen depth = Gen.map materialise (sketch_gen depth)
let pair_gen = Gen.map materialise_pair sketch_pair_gen

let linked_gen depth =
  Gen.map2
    (fun sketch targets ->
      let ty = materialise sketch in
      let build = builder () in
      List.iteri
        (fun index target ->
          let variable = V.fresh None in
          V.link variable (build target);
          ignore (index, variable))
        targets;
      List.fold_left
        (fun ty target ->
          let variable = V.fresh None in
          V.link variable (build target);
          T.TTup [ ty; T.TVar variable ])
        ty targets)
    (sketch_gen depth)
    (Gen.list_size (Gen.int_range 1 3) (sketch_gen (depth - 1)))

let rec has_linked_variable ty =
  match ty with
  | T.TVar variable -> begin
      match V.state variable with
      | V.Linked _ -> true
      | V.Unbound _ -> false
    end
  | T.TFun (parameter, result) ->
      has_linked_variable parameter || has_linked_variable result
  | T.TTup items | T.TCustom (_, items) -> List.exists has_linked_variable items
  | T.TRecord row -> has_linked_variable row
  | T.TRowExtend (_, field, rest) ->
      has_linked_variable field || has_linked_variable rest
  | T.TInt | T.TFloat | T.TChar | T.TStr | T.TBool | T.TUnit | T.TRowEmpty ->
      false

let law_zonk_is_idempotent =
  Test.make ~count:5000 ~name:"zonking a zonked type changes nothing"
    ~print:written (linked_gen 3)
    (fun ty -> T.equal (T.zonk (T.zonk ty)) (T.zonk ty))

let law_a_zonked_type_holds_no_link =
  Test.make ~count:5000 ~name:"a zonked type carries no linked variable"
    ~print:written (linked_gen 3)
    (fun ty -> not (has_linked_variable (T.zonk ty)))

let law_zonk_keeps_what_a_link_points_at =
  Test.make ~count:5000 ~name:"a linked variable zonks to what it was linked to"
    ~print:written (type_gen 3)
    (fun ty ->
      let variable = V.fresh None in
      V.link variable ty;
      T.equal (T.zonk (T.TVar variable)) (T.zonk ty))

let law_unification_makes_both_sides_equal =
  Test.make ~count:5000 ~name:"a successful unification makes both types equal"
    ~print:written_pair pair_gen
    (fun (left, right) -> (not (unifies left right)) || same left right)

let law_unification_is_reflexive =
  Test.make ~count:5000 ~name:"a type unifies with itself and learns nothing"
    ~print:(fun sketch -> written (materialise sketch)) (sketch_gen 3)
    (fun sketch ->
      let ty = materialise sketch in
      unifies ty ty && alpha_same ty (materialise sketch))

let law_unification_is_symmetric =
  Test.make ~count:5000 ~name:"unification succeeds in either order"
    ~print:(fun pair -> written_pair (materialise_pair pair)) sketch_pair_gen
    (fun (left, right) ->
      let one, other = materialise_pair (left, right) in
      let flipped_one, flipped_other = materialise_pair (left, right) in
      unifies one other = unifies flipped_other flipped_one)

let law_unification_resolves_what_it_learns =
  Test.make ~count:5000
    ~name:"what a unification learns is resolved, not pending"
    ~print:written_pair pair_gen
    (fun (left, right) ->
      (not (unifies left right))
      || not (has_linked_variable (T.zonk (T.TTup [ left; right ]))))

let deeper build =
  V.enter_level ();
  let made = build () in
  V.leave_level ();
  made

let law_unification_is_idempotent_as_an_effect =
  Test.make ~count:5000 ~name:"unifying twice learns nothing the first pass missed"
    ~print:written_pair pair_gen
    (fun (left, right) ->
      (not (unifies left right))
      ||
      let settled = T.zonk left in
      unifies left right && T.equal (T.zonk left) settled)

let law_unification_is_transitive =
  Test.make ~count:5000
    ~name:"what unifies with a middle type unifies with the far side"
    ~print:(fun (one, other) -> written_pair (materialise_pair (one, other)))
    sketch_pair_gen
    (fun (one, other) ->
      let left, right = materialise_pair (one, other) in
      let middle = T.TVar (V.fresh None) in
      (not (unifies left middle))
      || (not (unifies middle right))
      || same left right)

let law_a_variable_never_takes_a_type_that_holds_it =
  Test.make ~count:5000 ~name:"a variable never unifies with a type that holds it"
    ~print:written (type_gen 3)
    (fun ty ->
      let variable = V.fresh None in
      let holding = T.TTup [ T.TVar variable; ty ] in
      not (unifies (T.TVar variable) holding))

let record_sketch_gen =
  Gen.map2
    (fun fields tail -> (keeping_first_label fields, tail))
    (Gen.list_size (Gen.int_range 2 4) (Gen.pair label_gen (sketch_gen 1)))
    (Gen.oneof
       [ Gen.return (Sk_ground T.TRowEmpty);
         Gen.map (fun index -> Sk_var (6 + index)) (Gen.int_bound 1) ])

let law_a_record_does_not_care_in_which_order_it_was_written =
  Test.make ~count:5000
    ~name:"two records unify however their fields were ordered"
    ~print:(fun (fields, tail) ->
      written (materialise (Sk_record (fields, tail))))
    record_sketch_gen
    (fun (fields, tail) ->
      unifies
        (materialise (Sk_record (fields, tail)))
        (materialise (Sk_record (List.rev fields, tail))))

let law_a_row_never_takes_a_row_that_holds_it =
  Test.make ~count:2000 ~name:"a row variable never takes a row that holds it"
    ~print:(fun label -> label) label_gen
    (fun label ->
      let row = V.fresh None in
      not (unifies (T.TVar row) (T.TRowExtend (label, T.TInt, T.TVar row))))

let law_a_ground_type_unifies_only_with_itself =
  Test.make ~count:2000 ~name:"a ground type unifies only with itself"
    ~print:(fun (one, other) -> Message.of_type one ^ "  ~  " ^ Message.of_type other)
    (Gen.pair ground_gen ground_gen)
    (fun (one, other) -> unifies one other = T.equal one other)

let law_a_bound_constraint_is_satisfied =
  Test.make ~count:5000
    ~name:"what a constrained variable takes satisfies the constraint"
    ~print:written (type_gen 3)
    (fun ty ->
      let numeric = T.TVar (V.fresh (Some Data.Constraint.Number)) in
      (not (unifies numeric ty))
      ||
      match T.zonk numeric with
      | T.TInt | T.TFloat -> true
      | T.TVar variable -> V.constraint_of variable = Some Data.Constraint.Number
      | T.TChar | T.TStr | T.TBool | T.TUnit | T.TFun _ | T.TTup _
      | T.TCustom _ | T.TRecord _ | T.TRowExtend _ | T.TRowEmpty ->
          false)

let constraint_gen = Gen.oneof_list Data.Constraint.all

let law_a_constraint_meets_another_the_same_way_round =
  Test.make ~count:2000 ~name:"two constraints meet the same way in either order"
    ~print:(fun (one, other) ->
      Data.Constraint.name one ^ " & " ^ Data.Constraint.name other)
    (Gen.pair constraint_gen constraint_gen)
    (fun (one, other) ->
      Data.Constraint.combine one other = Data.Constraint.combine other one)

let law_a_constraint_meets_itself =
  Test.make ~count:100 ~name:"a constraint met with itself is unchanged"
    ~print:Data.Constraint.name constraint_gen
    (fun carried -> Data.Constraint.combine carried carried = Some carried)

let law_two_constrained_variables_keep_both_constraints =
  Test.make ~count:2000
    ~name:"unifying two constrained variables keeps what both asked for"
    ~print:(fun (one, other) ->
      Data.Constraint.name one ^ " & " ^ Data.Constraint.name other)
    (Gen.pair constraint_gen constraint_gen)
    (fun (one, other) ->
      let left = V.fresh (Some one) and right = V.fresh (Some other) in
      match
        (unifies (T.TVar left) (T.TVar right), Data.Constraint.combine one other)
      with
      | false, None -> true
      | false, Some _ | true, None -> false
      | true, Some together -> begin
          match T.zonk (T.TVar left) with
          | T.TVar settled -> V.constraint_of settled = Some together
          | T.TInt | T.TFloat | T.TChar | T.TStr | T.TBool | T.TUnit | T.TFun _
          | T.TTup _ | T.TCustom _ | T.TRecord _ | T.TRowExtend _ | T.TRowEmpty
            ->
              false
        end)

let law_using_a_scheme_does_not_spend_it =
  Test.make ~count:5000 ~name:"using a scheme once leaves it whole for the next use"
    ~print:(fun sketch -> written (materialise sketch)) (sketch_gen 3)
    (fun sketch ->
      let scheme = I.generalize (deeper (fun () -> materialise sketch)) in
      let spent = I.instantiate scheme in
      let ground = T.TTup [ T.TInt; T.TStr ] in
      ignore (unifies spent ground);
      alpha_same (I.instantiate scheme) (I.instantiate scheme))

let quantifying ty = T.Scheme (T.Variables.elements (I.ftv_typ ty), ty)

let printed_items ty =
  let written = Message.of_type ty in
  let inner =
    String.sub written 2 (max 0 (String.length written - 4))
  in
  String.split_on_char ',' inner |> List.map String.trim

let law_printing_names_each_variable_apart =
  Test.make ~count:5000
    ~name:"one variable prints as one name, two variables print apart"
    ~print:(fun indices ->
      Message.of_type
        (materialise (Sk_tup (List.map (fun index -> Sk_var index) indices))))
    (Gen.list_size (Gen.int_range 2 6) (Gen.int_bound 5))
    (fun indices ->
      let printed =
        printed_items
          (materialise (Sk_tup (List.map (fun index -> Sk_var index) indices)))
      in
      List.length printed = List.length indices
      && List.for_all2
           (fun index name ->
             List.for_all2
               (fun other other_name ->
                 String.equal name other_name = (index = other))
               indices printed)
           indices printed)

let law_printing_does_not_depend_on_identity =
  Test.make ~count:5000
    ~name:"a type and a fresh copy of it print the same"
    ~print:written (type_gen 3)
    (fun ty ->
      String.equal
        (Message.of_type ty)
        (Message.of_type (I.instantiate (quantifying ty))))

let law_instantiation_only_renames =
  Test.make ~count:5000 ~name:"instantiation renames variables without reshaping"
    ~print:written (type_gen 3)
    (fun ty -> alpha_same (I.instantiate (quantifying ty)) ty)

let law_instantiation_is_fresh =
  Test.make ~count:5000 ~name:"instantiation shares no variable with its scheme"
    ~print:written (type_gen 3)
    (fun ty ->
      T.Variables.is_empty
        (T.Variables.inter
           (I.ftv_typ (I.instantiate (quantifying ty)))
           (I.ftv_typ ty)))

let law_instantiation_keeps_the_constraints =
  Test.make ~count:5000 ~name:"instantiation carries every constraint over"
    ~print:written (type_gen 3)
    (fun ty ->
      let carried settled =
        List.filter_map V.constraint_of (T.Variables.elements (I.ftv_typ settled))
        |> List.map Data.Constraint.name |> List.sort String.compare
      in
      carried (I.instantiate (quantifying ty)) = carried ty)

let levels_of ty =
  T.fold_variables
    (fun collected variable ->
      match V.state variable with
      | V.Linked _ -> collected
      | V.Unbound { level; _ } -> level :: collected)
    [] ty

let law_generalization_keeps_what_the_binding_holds =
  Test.make ~count:5000
    ~name:"a variable the binding still holds is not generalized" ~print:written
    (type_gen 3)
    (fun ty ->
      match I.generalize ty with
      | T.Scheme (quantified, _) -> quantified = [])

let law_generalization_takes_what_is_deeper =
  Test.make ~count:5000
    ~name:"a variable born deeper than the binding is generalized"
    ~print:(fun sketch -> written (materialise sketch)) (sketch_gen 3)
    (fun sketch ->
      let ty = deeper (fun () -> materialise sketch) in
      match I.generalize ty with
      | T.Scheme (quantified, _) ->
          T.Variables.equal (T.Variables.of_list quantified) (I.ftv_typ ty))

let law_instantiation_lands_on_the_current_level =
  Test.make ~count:5000
    ~name:"instantiation gives variables at the level that asks for them"
    ~print:(fun sketch -> written (materialise sketch)) (sketch_gen 3)
    (fun sketch ->
      let scheme = I.generalize (deeper (fun () -> materialise sketch)) in
      List.for_all
        (fun level -> level = V.current_level ())
        (levels_of (I.instantiate scheme)))

let law_binding_only_lowers_levels =
  Test.make ~count:5000
    ~name:"binding a variable never raises the level of what it holds"
    ~print:(fun sketch -> written (materialise sketch)) (sketch_gen 3)
    (fun sketch ->
      let variable = V.fresh None in
      let held = deeper (fun () -> materialise sketch) in
      let shallow = V.current_level () in
      (not (unifies (T.TVar variable) held))
      || List.for_all (fun level -> level <= shallow) (levels_of held))


module P = Canonical.Pattern

let anywhere kind : P.t = Data.Located.dummy kind

let pattern_env =
  Infer.Type_env.build ~imports:[]
    (Types.resolved
       (Utils.canonical
          {|
type Box = Box Int

type Pair = Pair Int String

type Maybe a = Just a | Nothing

type Colour = Red | Green
|}))

type flavour = Free | Number | Text

type constructor = {
  written : string;
  payloads : flavour list;
  owner : string;
  owner_arity : int;
}

let constructors =
  [
    { written = "Box"; payloads = [ Number ]; owner = "Box"; owner_arity = 0 };
    { written = "Pair"; payloads = [ Number; Text ]; owner = "Pair"; owner_arity = 0 };
    { written = "Just"; payloads = [ Free ]; owner = "Maybe"; owner_arity = 1 };
    { written = "Nothing"; payloads = []; owner = "Maybe"; owner_arity = 1 };
    { written = "Red"; payloads = []; owner = "Colour"; owner_arity = 0 };
    { written = "Green"; payloads = []; owner = "Colour"; owner_arity = 0 };
  ]

let constructor_gen = Gen.oneof_list constructors

let unnamed_gen =
  Gen.oneof [ Gen.return (anywhere P.P_anything); Gen.return (anywhere (P.P_var "unnamed")) ]

let number_gen = Gen.map (fun value -> anywhere (P.P_int value)) (Gen.int_range 0 5)
let text_gen = Gen.map (fun text -> anywhere (P.P_str text)) (Gen.oneof_list [ "a"; "b" ])
let letter_gen = Gen.map (fun letter -> anywhere (P.P_chr letter)) (Gen.oneof_list [ "x"; "y" ])

let rec pattern_gen depth =
  let leaf =
    Gen.oneof
      [ unnamed_gen; Gen.return (anywhere P.P_unit); number_gen; text_gen; letter_gen ]
  in
  if depth <= 0 then leaf
  else
    let smaller = pattern_gen (depth - 1) in
    Gen.oneof_weighted
      [
        (5, leaf);
        ( 2,
          Gen.map
            (fun items -> anywhere (P.P_tuple items))
            (Gen.list_size (Gen.int_range 2 3) smaller) );
        (2, list_gen (depth - 1));
        (2, cons_gen (depth - 1));
        (2, Gen.map (fun inner -> anywhere (P.P_alias (inner, "aliased"))) smaller);
        ( 2,
          Gen.map
            (fun fields -> anywhere (P.P_record fields))
            (Gen.list_size (Gen.int_range 1 3) (Gen.return "field")) );
        (3, constructor_gen_of depth);
      ]

and of_flavour depth = function
  | Free -> if depth <= 0 then unnamed_gen else pattern_gen depth
  | Number -> Gen.oneof [ unnamed_gen; number_gen ]
  | Text -> Gen.oneof [ unnamed_gen; text_gen ]

and constructor_gen_of depth =
  Gen.bind constructor_gen (fun constructor ->
      Gen.map
        (fun arguments -> anywhere (P.P_ctor (Data.Name.local constructor.written, arguments)))
        (Gen.flatten_list (List.map (of_flavour (depth - 1)) constructor.payloads)))

and element_gen depth =
  Gen.oneof
    [
      Gen.return unnamed_gen;
      Gen.return number_gen;
      Gen.return text_gen;
      Gen.return letter_gen;
      Gen.map
        (fun constructor ->
          Gen.map
            (fun arguments ->
              anywhere (P.P_ctor (Data.Name.local constructor.written, arguments)))
            (Gen.flatten_list (List.map (of_flavour (depth - 1)) constructor.payloads)))
        constructor_gen;
    ]

and list_gen depth =
  Gen.bind (element_gen depth) (fun element ->
      Gen.map
        (fun items -> anywhere (P.P_list items))
        (Gen.list_size (Gen.int_range 0 3) element))

and cons_gen depth =
  Gen.bind (element_gen depth) (fun element ->
      Gen.map2
        (fun head tail -> anywhere (P.P_cons (head, tail)))
        element
        (Gen.oneof
           [
             unnamed_gen;
             Gen.map
               (fun items -> anywhere (P.P_list items))
               (Gen.list_size (Gen.int_range 0 2) element);
           ]))

let with_distinct_binders pattern =
  let taken = ref 0 in
  let fresh prefix =
    let name = prefix ^ string_of_int !taken in
    incr taken;
    name
  in
  let rec go (pattern : P.t) : P.t =
    anywhere
      (match pattern.thing with
      | P.P_var _ -> P.P_var (fresh "v")
      | P.P_alias (inner, _) ->
          let inner = go inner in
          P.P_alias (inner, fresh "v")
      | P.P_record fields -> P.P_record (List.map (fun _ -> fresh "f") fields)
      | P.P_tuple items -> P.P_tuple (List.map go items)
      | P.P_list items -> P.P_list (List.map go items)
      | P.P_cons (head, tail) ->
          let head = go head in
          let tail = go tail in
          P.P_cons (head, tail)
      | P.P_ctor (name, arguments) -> P.P_ctor (name, List.map go arguments)
      | (P.P_anything | P.P_unit | P.P_chr _ | P.P_str _ | P.P_int _) as leaf ->
          leaf)
  in
  go pattern

let named_pattern_gen depth = Gen.map with_distinct_binders (pattern_gen depth)

let rec binders pattern =
  let across items =
    List.fold_left (fun known item -> Names.union known (binders item)) Names.empty
      items
  in
  match pattern.Data.Located.thing with
  | P.P_var name -> Names.singleton name
  | P.P_alias (inner, name) -> Names.add name (binders inner)
  | P.P_record fields -> Names.of_list fields
  | P.P_tuple items | P.P_list items -> across items
  | P.P_cons (head, tail) -> Names.union (binders head) (binders tail)
  | P.P_ctor (_, arguments) -> across arguments
  | P.P_anything | P.P_unit | P.P_chr _ | P.P_str _ | P.P_int _ -> Names.empty

let rec field_of row label =
  match row with
  | T.TRecord inner -> field_of inner label
  | T.TRowExtend (found, typ, _) when String.equal found label -> Some typ
  | T.TRowExtend (_, _, rest) -> field_of rest label
  | T.TVar _ | T.TRowEmpty | T.TInt | T.TFloat | T.TChar | T.TStr | T.TBool
  | T.TUnit | T.TFun _ | T.TTup _ | T.TCustom _ ->
      None

let positions typed =
  let rec go found (node : Typed.Pattern.t) =
    match node.pattern with
    | Typed.Pattern.P_T_var name -> (name, Some node.typ) :: found
    | Typed.Pattern.P_T_alias (aliased, name) -> go ((name, Some node.typ) :: found) aliased
    | Typed.Pattern.P_T_record fields ->
        List.fold_left
          (fun found label -> (label, field_of node.typ label) :: found)
          found fields
    | Typed.Pattern.P_T_tuple items | Typed.Pattern.P_T_list items ->
        List.fold_left go found items
    | Typed.Pattern.P_T_cons (head, tail) -> go (go found head) tail
    | Typed.Pattern.P_T_ctor (_, arguments) -> List.fold_left go found arguments
    | Typed.Pattern.P_T_anything | Typed.Pattern.P_T_unit | Typed.Pattern.P_T_chr _
    | Typed.Pattern.P_T_str _ | Typed.Pattern.P_T_int _ ->
        found
  in
  go [] typed

let inferred_pattern pattern = I.infer_pattern pattern_env pattern

let typeable pattern =
  match inferred_pattern pattern with
  | _ -> true
  | exception Reporting.Error.Found _ -> false

let about pattern holds =
  match inferred_pattern pattern with
  | typed, bound -> holds typed bound
  | exception Reporting.Error.Found _ -> true

let drawn = Format.asprintf "%a" P.pp

let law_a_bound_name_is_the_position_it_binds =
  Test.make ~count:5000
    ~name:"a name bound by a pattern has the type of the position it binds"
    ~print:drawn (named_pattern_gen 3)
    (fun pattern ->
      about pattern @@ fun typed bound ->
      List.for_all
        (fun (name, at_position) ->
          match (at_position, Infer.Value_env.find (Data.Name.local name) bound) with
          | Some at_position, Some (T.Scheme ([], declared)) ->
              same at_position declared
          | Some _, Some (T.Scheme (_ :: _, _)) | Some _, None | None, _ -> false)
        (positions typed))

let law_a_pattern_invents_no_type_of_its_own =
  Test.make ~count:5000
    ~name:"every variable a bound name carries is one the pattern type carries"
    ~print:drawn (named_pattern_gen 3)
    (fun pattern ->
      about pattern @@ fun typed bound ->
      T.Variables.subset
        (List.fold_left
           (fun collected (T.Scheme (_, carried)) ->
             T.Variables.union collected (I.ftv_typ carried))
           T.Variables.empty
           (Infer.Value_env.schemes bound))
        (I.ftv_typ typed.Typed.Pattern.typ))

let law_a_pattern_binds_exactly_its_names =
  Test.make ~count:5000 ~name:"a pattern binds exactly the names it writes"
    ~print:drawn (named_pattern_gen 3)
    (fun pattern ->
      about pattern @@ fun _ bound ->
      Names.equal (binders pattern)
        (Names.of_list (List.map Data.Name.base (Infer.Value_env.names bound))))

let law_a_pattern_is_inferred_the_same_way_twice =
  Test.make ~count:5000 ~name:"inferring a pattern twice gives the same answer"
    ~print:drawn (named_pattern_gen 3)
    (fun pattern ->
      about pattern @@ fun left left_bound ->
      let right, right_bound = inferred_pattern pattern in
      let agree = renaming () in
      same_pattern agree left right
      && Infer.Value_env.equal
           (fun (T.Scheme (_, one)) (T.Scheme (_, other)) -> agree one other)
           left_bound right_bound)

let saturated_gen =
  Gen.bind constructor_gen (fun constructor ->
      Gen.map
        (fun arguments ->
          ( constructor,
            with_distinct_binders
              (anywhere (P.P_ctor (Data.Name.local constructor.written, arguments))) ))
        (Gen.flatten_list (List.map (of_flavour 2) constructor.payloads)))

let law_a_constructor_pattern_takes_its_declared_type =
  Test.make ~count:5000
    ~name:"a constructor pattern has the type its declaration gives it"
    ~print:(fun (constructor, pattern) -> constructor.written ^ ": " ^ drawn pattern)
    saturated_gen
    (fun (constructor, pattern) ->
      about pattern @@ fun typed _ ->
      match T.zonk typed.Typed.Pattern.typ with
      | T.TCustom (found, arguments) ->
          Data.Name.equal found (Data.Name.local constructor.owner)
          && List.length arguments = constructor.owner_arity
      | T.TVar _ | T.TInt | T.TFloat | T.TChar | T.TStr | T.TBool | T.TUnit
      | T.TFun _ | T.TTup _ | T.TRecord _ | T.TRowExtend _ | T.TRowEmpty ->
          false)

let law_a_constructor_pattern_checks_its_arity =
  Test.make ~count:2000
    ~name:"a constructor pattern of the wrong arity is reported, not crashed"
    ~print:(fun (constructor, given) ->
      Printf.sprintf "%s takes %d, given %d" constructor.written
        (List.length constructor.payloads) given)
    (Gen.pair constructor_gen (Gen.int_range 0 3))
    (fun (constructor, given) ->
      let arity = List.length constructor.payloads in
      let pattern =
        anywhere
          (P.P_ctor
             ( Data.Name.local constructor.written,
               List.init given (fun _ -> anywhere P.P_anything) ))
      in
      match inferred_pattern pattern with
      | _ -> given = arity
      | exception
          Reporting.Error.Found { problem = Type (Bad_arity _); _ } ->
          given <> arity)

let law_the_patterns_generated_are_mostly_typeable =
  Test.make ~count:1 ~name:"the pattern generator keeps the laws off vacuum"
    ~print:(fun () -> "") (Gen.return ())
    (fun () ->
      let sample = Gen.generate ~n:2000 (named_pattern_gen 3) in
      List.length (List.filter typeable sample) * 2 > List.length sample)


module Layout = Dartea_test_layout_laws

type ground = Whole | Fraction | Sentence | Truth

let annotation = function
  | Whole -> "Int"
  | Fraction -> "Float"
  | Sentence -> "String"
  | Truth -> "Bool"

let ground_gen = Gen.oneof_list [ Whole; Fraction; Sentence; Truth ]

let clashing = function
  | Whole -> Gen.oneof_list [ Sentence; Truth ]
  | Fraction -> Gen.oneof_list [ Sentence; Truth ]
  | Sentence -> Gen.oneof_list [ Whole; Truth ]
  | Truth -> Gen.oneof_list [ Whole; Sentence ]

let literal = function
  | Whole -> Layout.Int 7
  | Fraction -> Layout.Var "1.5"
  | Sentence -> Layout.Text "hi"
  | Truth -> Layout.Var "True"

let operators = function
  | Whole | Fraction -> [ "+"; "-"; "*" ]
  | Sentence -> [ "++" ]
  | Truth -> []

let helper = function
  | Whole -> Some ("twice", Whole)
  | Sentence -> Some ("shout", Sentence)
  | Truth -> Some ("isEmpty", Sentence)
  | Fraction -> None

let known ~scope ground =
  List.filter_map
    (fun (name, carried) -> if carried = ground then Some name else None)
    scope

let rec value_gen ~scope ~blocks ~depth ground =
  let leaf =
    match known ~scope ground with
    | [] -> Gen.return (literal ground)
    | names ->
        Gen.oneof
          [
            Gen.return (literal ground);
            Gen.map (fun name -> Layout.Var name) (Gen.oneof_list names);
          ]
  in
  if depth <= 0 then leaf
  else
    let smaller ground = value_gen ~scope ~blocks:false ~depth:(depth - 1) ground in
    let inline_only = smaller in
    let arithmetic =
      match operators ground with
      | [] -> []
      | available ->
          [
            ( 3,
              Gen.map3
                (fun operator left right -> Layout.Binop (operator, left, right))
                (Gen.oneof_list available) (smaller ground) (smaller ground) );
          ]
    in
    let comparison =
      match ground with
      | Truth ->
          [
            ( 3,
              Gen.bind ground_gen (fun operand ->
                  Gen.map2
                    (fun left right -> Layout.Binop ("==", left, right))
                    (smaller operand) (smaller operand)) );
          ]
      | Whole | Fraction | Sentence -> []
    in
    let applying =
      match helper ground with
      | None -> []
      | Some (name, takes) ->
          [ (2, Gen.map (fun argument -> Layout.App (name, [ argument ]))
                  (smaller takes)) ]
    in
    let branching =
      [
        ( 2,
          Gen.map3
            (fun condition yes no -> Layout.If (condition, yes, no))
            (smaller Truth) (smaller ground) (smaller ground) );
        ( 2,
          Gen.bind ground_gen (fun carried ->
              let name = "bound" ^ string_of_int depth in
              Gen.map2
                (fun bound body -> Layout.Let (name, bound, body))
                (inline_only carried)
                (value_gen ~scope:((name, carried) :: scope) ~blocks:false
                   ~depth:(depth - 1) ground)) );
      ]
    in
    let matching =
      if not blocks then []
      else
        [ (4, match_gen ~scope ~depth ground) ]
    in
    Gen.oneof_weighted
      (((4, leaf) :: arithmetic) @ comparison @ applying @ branching @ matching)

and match_gen ~scope ~depth ground =
  let subject = "subject" ^ string_of_int depth in
  let first = "first" ^ string_of_int depth in
  let second = "second" ^ string_of_int depth in
  let inline_only carried = value_gen ~scope ~blocks:false ~depth:(depth - 1) carried in
  let body ~bound = value_gen ~scope:(bound @ scope) ~blocks:true ~depth:(depth - 1) ground in
  let cased scrutinee arms =
    Gen.map2
      (fun scrutinee arms -> Layout.Block_let ([ (subject, scrutinee) ], Layout.Case (subject, arms)))
      scrutinee arms
  in
  let one_armed scrutinee written bound =
    cased scrutinee (Gen.map (fun taken -> [ (written, taken) ]) (body ~bound))
  in
  Gen.bind ground_gen (fun carried ->
      Gen.oneof
        [
          one_armed
            (Gen.map2
               (fun left right ->
                 Layout.Var
                   (Printf.sprintf "( %s, %s )" (Layout.inline left) (Layout.inline right)))
               (inline_only carried) (inline_only Sentence))
            (Printf.sprintf "( %s, %s )" first second)
            [ (first, carried); (second, Sentence) ];
          one_armed
            (Gen.map (fun payload -> Layout.App ("Box", [ payload ])) (inline_only Whole))
            (Printf.sprintf "Box %s" first)
            [ (first, Whole) ];
          one_armed
            (Gen.map2
               (fun left right -> Layout.App ("Pair", [ left; right ]))
               (inline_only Whole) (inline_only Sentence))
            (Printf.sprintf "Pair %s %s" first second)
            [ (first, Whole); (second, Sentence) ];
          one_armed
            (Gen.map (fun payload -> Layout.App ("Box", [ payload ])) (inline_only Whole))
            (Printf.sprintf "(Box %s) as %s" first second)
            [ (first, Whole) ];
          cased
            (Gen.map (fun payload -> Layout.App ("Just", [ payload ])) (inline_only carried))
            (Gen.map2
               (fun taken missing ->
                 [ (Printf.sprintf "Just %s" first, taken); ("Nothing", missing) ])
               (body ~bound:[ (first, carried) ])
               (body ~bound:[]));
          cased
            (Gen.map2
               (fun head rest -> Layout.Binop ("::", head, Layout.Items rest))
               (inline_only carried)
               (Gen.list_size (Gen.int_range 0 2) (inline_only carried)))
            (Gen.map2
               (fun taken empty ->
                 [ (Printf.sprintf "%s :: %s" first second, taken); ("[]", empty) ])
               (body ~bound:[ (first, carried) ])
               (body ~bound:[]));
        ])

let declared_types =
  String.concat "\n\n"
    [
      "type Box = Box Int";
      "type Pair = Pair Int String";
      "type Maybe a = Just a | Nothing";
      "twice : Int -> Int\ntwice n = n + n";
      "shout : String -> String\nshout s = s ++ \"!\"";
      "isEmpty : String -> Bool\nisEmpty s = s == \"\"";
    ]


let program_of ~style ~name ~written body =
  let heading = match written with None -> [] | Some ground -> [ name ^ " : " ^ ground ] in
  let definition =
    String.concat "\n"
      (Layout.render_declaration
         (Layout.Value { name; signature = None; body })
         ~style)
  in
  "\n" ^ declared_types ^ "\n\n"
  ^ String.concat "\n" (heading @ [ definition ])
  ^ "\n"

let outcome source =
  match Types.inferred source with
  | result when result.Declarations.errors <> [] -> None
  | result ->
      Some
        (List.sort compare
           (List.map
              (fun (declaration : Typed.Declaration.t) ->
                ( Data.Located.unwrap declaration.name,
                  Types.shape declaration.typ ))
              result.Declarations.declarations))
  | exception _ -> None

let body_gen ground = value_gen ~scope:[] ~blocks:true ~depth:3 ground

let well_typed_gen =
  Gen.bind ground_gen (fun ground ->
      Gen.map2 (fun body style -> (ground, body, style)) (body_gen ground)
        Layout.style_gen)

let clashing_gen =
  Gen.bind ground_gen (fun ground ->
      Gen.bind (clashing ground) (fun wrong ->
          Gen.map2
            (fun body style -> (ground, wrong, body, style))
            (body_gen ground) Layout.style_gen))

let answered ~style ~written body =
  program_of ~style ~name:"answer" ~written:(Some (annotation written)) body

let law_a_well_typed_program_is_accepted =
  Test.make ~count:2000 ~name:"a program built to a type is accepted at that type"
    ~print:(fun (ground, body, style) -> answered ~style ~written:ground body)
    well_typed_gen
    (fun (ground, body, style) ->
      match outcome (answered ~style ~written:ground body) with
      | None -> false
      | Some inferred ->
          List.exists
            (fun (name, shape) ->
              String.equal name "answer" && String.equal shape (annotation ground))
            inferred)

let law_a_clashing_annotation_is_rejected =
  Test.make ~count:2000 ~name:"a program built to a type is refused at another"
    ~print:(fun (_, wrong, body, style) -> answered ~style ~written:wrong body)
    clashing_gen
    (fun (_, wrong, body, style) -> outcome (answered ~style ~written:wrong body) = None)

let law_layout_does_not_change_the_inferred_type =
  Test.make ~count:2000 ~name:"whitespace does not change what is inferred"
    ~print:(fun ((ground, body, style), _) -> answered ~style ~written:ground body)
    (Gen.pair well_typed_gen Layout.style_gen)
    (fun ((ground, body, style), other) ->
      outcome (answered ~style ~written:ground body)
      = outcome (answered ~style:other ~written:ground body))

let spelling_gen =
  Gen.oneof_list
    [ "a"; "b"; "t"; "u"; "a0"; "a1"; "a2"; "a3"; "b0"; "b1"; "x"; "y"; "elem";
      "value" ]

let law_a_spelling_does_not_change_the_verdict =
  Test.make ~count:2000
    ~name:"how a type variable is spelled does not change the verdict"
    ~print:(fun (one, other) -> Utils.spelled_with one other)
    (Gen.pair spelling_gen spelling_gen)
    (fun (one, other) ->
      String.equal one other
      ||
      match outcome (Utils.spelled_with one other) with
      | None -> false
      | Some inferred ->
          List.exists
            (fun (name, shape) ->
              String.equal name "used" && String.equal shape "String")
            inferred)

let suite =
  QCheck_ounit.to_ounit2_test_list
    [
      law_zonk_is_idempotent;
      law_a_zonked_type_holds_no_link;
      law_zonk_keeps_what_a_link_points_at;
      law_unification_makes_both_sides_equal;
      law_unification_is_reflexive;
      law_unification_is_symmetric;
      law_unification_resolves_what_it_learns;
      law_unification_is_idempotent_as_an_effect;
      law_unification_is_transitive;
      law_a_variable_never_takes_a_type_that_holds_it;
      law_a_ground_type_unifies_only_with_itself;
      law_a_record_does_not_care_in_which_order_it_was_written;
      law_a_row_never_takes_a_row_that_holds_it;
      law_a_bound_constraint_is_satisfied;
      law_a_constraint_meets_another_the_same_way_round;
      law_a_constraint_meets_itself;
      law_two_constrained_variables_keep_both_constraints;
      law_using_a_scheme_does_not_spend_it;
      law_printing_names_each_variable_apart;
      law_printing_does_not_depend_on_identity;
      law_instantiation_only_renames;
      law_instantiation_is_fresh;
      law_instantiation_keeps_the_constraints;
      law_generalization_keeps_what_the_binding_holds;
      law_generalization_takes_what_is_deeper;
      law_instantiation_lands_on_the_current_level;
      law_binding_only_lowers_levels;
      law_a_bound_name_is_the_position_it_binds;
      law_a_pattern_invents_no_type_of_its_own;
      law_a_pattern_binds_exactly_its_names;
      law_a_pattern_is_inferred_the_same_way_twice;
      law_a_constructor_pattern_takes_its_declared_type;
      law_a_constructor_pattern_checks_its_arity;
      law_the_patterns_generated_are_mostly_typeable;
      law_a_well_typed_program_is_accepted;
      law_a_clashing_annotation_is_rejected;
      law_layout_does_not_change_the_inferred_type;
      law_a_spelling_does_not_change_the_verdict;
    ]
