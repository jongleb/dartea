open QCheck2
module I = Infer.Infer_proc
module T = Typed.Type
module Types = Dartea_test_type_system
module Names = Set.Make (String)

let rec normalised ty =
  match ty with
  | T.TFun (parameter, result) -> T.TFun (normalised parameter, normalised result)
  | T.TTup items -> T.TTup (List.map normalised items)
  | T.TCustom (name, arguments) -> T.TCustom (name, List.map normalised arguments)
  | T.TRecord row -> T.TRecord (normalised_row row)
  | T.TRowExtend _ -> normalised_row ty
  | (T.TVar _ | T.TInt | T.TFloat | T.TChar | T.TStr | T.TBool | T.TUnit
    | T.TRowEmpty) as settled ->
      settled

and normalised_row row =
  let rec split = function
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

let alpha ty =
  let seen = Hashtbl.create 16 in
  let renamed variable =
    match Hashtbl.find_opt seen variable with
    | Some already -> already
    | None ->
        let carried =
          match Data.Constraint.of_variable variable with
          | Some constraint_ -> Data.Constraint.name constraint_
          | None -> "any"
        in
        let fresh = Printf.sprintf "%s$%d" carried (Hashtbl.length seen) in
        Hashtbl.add seen variable fresh;
        fresh
  in
  let rec go ty =
    match ty with
    | T.TVar variable -> T.TVar (renamed variable)
    | T.TFun (parameter, result) -> T.TFun (go parameter, go result)
    | T.TTup items -> T.TTup (List.map go items)
    | T.TCustom (name, arguments) -> T.TCustom (name, List.map go arguments)
    | T.TRecord row -> T.TRecord (go row)
    | T.TRowExtend (label, field, rest) -> T.TRowExtend (label, go field, go rest)
    | (T.TInt | T.TFloat | T.TChar | T.TStr | T.TBool | T.TUnit | T.TRowEmpty) as
      settled ->
        settled
  in
  go ty

let same left right = T.equal (normalised left) (normalised right)

let alpha_same left right =
  T.equal (alpha (normalised left)) (alpha (normalised right))
let unified left right = try Some (I.unify left right) with Failure _ -> None
let written = I.string_of_typ
let written_pair (left, right) = written left ^ "  ~  " ^ written right

let variable_gen =
  Gen.oneof_list [ "a0"; "a1"; "b0"; "number0"; "comparable0"; "appendable0" ]

let row_variable_gen = Gen.oneof_list [ "r0"; "r1" ]
let label_gen = Gen.oneof_list [ "one"; "two"; "three" ]
let custom_gen = Gen.oneof_list [ ("List", 1); ("Box", 0); ("Maybe", 1); ("Result", 2) ]

let keeping_first_label fields =
  List.rev
    (List.fold_left
       (fun kept (label, typ) ->
         if List.exists (fun (seen, _) -> String.equal seen label) kept then kept
         else (label, typ) :: kept)
       [] fields)

let rec type_gen depth =
  let leaf =
    Gen.oneof
      [
        Gen.return T.TInt; Gen.return T.TFloat; Gen.return T.TChar;
        Gen.return T.TStr; Gen.return T.TBool; Gen.return T.TUnit;
        Gen.map (fun variable -> T.TVar variable) variable_gen;
      ]
  in
  if depth <= 0 then leaf
  else
    let smaller = type_gen (depth - 1) in
    Gen.oneof_weighted
      [
        (5, leaf);
        (2, Gen.map2 (fun parameter result -> T.TFun (parameter, result)) smaller smaller);
        ( 2,
          Gen.map
            (fun items -> T.TTup items)
            (Gen.list_size (Gen.int_range 2 3) smaller) );
        ( 2,
          Gen.bind custom_gen (fun (name, arity) ->
              Gen.map
                (fun arguments -> T.TCustom (Data.Name.local name, arguments))
                (Gen.list_size (Gen.return arity) smaller)) );
        (1, Gen.map (fun row -> T.TRecord row) (row_gen smaller));
      ]

and row_gen field =
  Gen.map2
    (fun fields tail ->
      List.fold_right
        (fun (label, typ) rest -> T.TRowExtend (label, typ, rest))
        (keeping_first_label fields) tail)
    (Gen.list_size (Gen.int_range 0 3) (Gen.pair label_gen field))
    (Gen.oneof
       [ Gen.return T.TRowEmpty; Gen.map (fun name -> T.TVar name) row_variable_gen ])

let pair_gen = Gen.pair (type_gen 3) (type_gen 3)

let law_unification_makes_both_sides_equal =
  Test.make ~count:5000 ~name:"a successful unification makes both types equal"
    ~print:written_pair pair_gen
    (fun (left, right) ->
      match unified left right with
      | None -> true
      | Some settling -> same (I.Subst.apply left settling) (I.Subst.apply right settling))

let law_unification_is_reflexive =
  Test.make ~count:5000 ~name:"a type unifies with itself and learns nothing"
    ~print:written (type_gen 3)
    (fun ty ->
      match unified ty ty with
      | None -> false
      | Some settling -> same (I.Subst.apply ty settling) ty)

let law_unification_is_symmetric =
  Test.make ~count:5000 ~name:"unification succeeds in either order"
    ~print:written_pair pair_gen
    (fun (left, right) ->
      Option.is_some (unified left right) = Option.is_some (unified right left))

let law_unification_settles_in_one_pass =
  Test.make ~count:5000 ~name:"the substitution a unification returns is idempotent"
    ~print:written_pair pair_gen
    (fun (left, right) ->
      match unified left right with
      | None -> true
      | Some settling ->
          let once = I.Subst.apply left settling in
          same (I.Subst.apply once settling) once)

let quantifying ty = T.Scheme (I.Str_set.elements (I.ftv_typ ty), ty)

let law_instantiation_only_renames =
  Test.make ~count:5000 ~name:"instantiation renames variables without reshaping"
    ~print:written (type_gen 3)
    (fun ty -> alpha_same (snd (I.instantiate (quantifying ty))) ty)

let law_instantiation_is_fresh =
  Test.make ~count:5000 ~name:"instantiation shares no variable with its scheme"
    ~print:written (type_gen 3)
    (fun ty ->
      I.Str_set.is_empty
        (I.Str_set.inter
           (I.ftv_typ (snd (I.instantiate (quantifying ty))))
           (I.ftv_typ ty)))

let law_instantiation_keeps_the_constraints =
  Test.make ~count:5000 ~name:"instantiation carries every constraint over"
    ~print:written (type_gen 3)
    (fun ty ->
      let carried settled =
        List.filter_map Data.Constraint.of_variable
          (I.Str_set.elements (I.ftv_typ settled))
        |> List.map Data.Constraint.name |> List.sort String.compare
      in
      carried (snd (I.instantiate (quantifying ty))) = carried ty)

let law_generalization_keeps_what_the_context_holds =
  Test.make ~count:5000
    ~name:"a type the context still holds is not generalized" ~print:written
    (type_gen 3)
    (fun ty ->
      let held =
        I.Name_map.singleton (Data.Name.local "held") (T.Scheme ([], ty))
      in
      T.equal (snd (I.instantiate (I.generalize ty held))) ty)

let law_generalization_takes_everything_the_context_lacks =
  Test.make ~count:5000
    ~name:"a free variable outside the context is generalized" ~print:written
    (type_gen 3)
    (fun ty ->
      match I.generalize ty I.Name_map.empty with
      | T.Scheme (quantified, _) ->
          I.Str_set.equal
            (I.Str_set.of_list quantified)
            (I.ftv_typ ty))


module P = Canonical.Pattern

let pattern_env =
  I.build_type_env ~imports:[]
    (Types.resolved
       (Types.canonical
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
  Gen.oneof [ Gen.return P.P_anything; Gen.return (P.P_var "unnamed") ]

let number_gen = Gen.map (fun value -> P.P_int value) (Gen.int_range 0 5)
let text_gen = Gen.map (fun text -> P.P_str text) (Gen.oneof_list [ "a"; "b" ])
let letter_gen = Gen.map (fun letter -> P.P_chr letter) (Gen.oneof_list [ "x"; "y" ])

let rec pattern_gen depth =
  let leaf =
    Gen.oneof
      [ unnamed_gen; Gen.return P.P_unit; number_gen; text_gen; letter_gen ]
  in
  if depth <= 0 then leaf
  else
    let smaller = pattern_gen (depth - 1) in
    Gen.oneof_weighted
      [
        (5, leaf);
        ( 2,
          Gen.map
            (fun items -> P.P_tuple items)
            (Gen.list_size (Gen.int_range 2 3) smaller) );
        (2, list_gen (depth - 1));
        (2, cons_gen (depth - 1));
        (2, Gen.map (fun inner -> P.P_alias (inner, "aliased")) smaller);
        ( 2,
          Gen.map
            (fun fields -> P.P_record fields)
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
        (fun arguments -> P.P_ctor (Data.Name.local constructor.written, arguments))
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
              P.P_ctor (Data.Name.local constructor.written, arguments))
            (Gen.flatten_list (List.map (of_flavour (depth - 1)) constructor.payloads)))
        constructor_gen;
    ]

and list_gen depth =
  Gen.bind (element_gen depth) (fun element ->
      Gen.map
        (fun items -> P.P_list items)
        (Gen.list_size (Gen.int_range 0 3) element))

and cons_gen depth =
  Gen.bind (element_gen depth) (fun element ->
      Gen.map2
        (fun head tail -> P.P_cons (head, tail))
        element
        (Gen.oneof
           [
             unnamed_gen;
             Gen.map
               (fun items -> P.P_list items)
               (Gen.list_size (Gen.int_range 0 2) element);
           ]))

let with_distinct_binders pattern =
  let taken = ref 0 in
  let fresh prefix =
    let name = prefix ^ string_of_int !taken in
    incr taken;
    name
  in
  let rec go pattern =
    match pattern with
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
    | (P.P_anything | P.P_unit | P.P_chr _ | P.P_str _ | P.P_int _) as leaf -> leaf
  in
  go pattern

let named_pattern_gen depth = Gen.map with_distinct_binders (pattern_gen depth)

let rec binders pattern =
  let across items =
    List.fold_left (fun known item -> Names.union known (binders item)) Names.empty
      items
  in
  match pattern with
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

let inferred_pattern pattern =
  I.Fresh.reset ();
  I.infer_pattern pattern_env pattern

let typeable pattern =
  match inferred_pattern pattern with
  | _ -> true
  | exception Failure _ -> false

let about pattern holds =
  match inferred_pattern pattern with
  | settling, typed, bound -> holds settling typed bound
  | exception Failure _ -> true

let drawn = Format.asprintf "%a" P.pp

let law_a_bound_name_is_the_position_it_binds =
  Test.make ~count:5000
    ~name:"a name bound by a pattern has the type of the position it binds"
    ~print:drawn (named_pattern_gen 3)
    (fun pattern ->
      about pattern @@ fun settling typed bound ->
      List.for_all
        (fun (name, at_position) ->
          match (at_position, I.Name_map.find_opt (Data.Name.local name) bound) with
          | Some at_position, Some (T.Scheme ([], declared)) ->
              same (I.Subst.apply at_position settling) (I.Subst.apply declared settling)
          | Some _, Some (T.Scheme (_ :: _, _)) | Some _, None | None, _ -> false)
        (positions typed))

let law_a_pattern_invents_no_type_of_its_own =
  Test.make ~count:5000
    ~name:"every variable a bound name carries is one the pattern type carries"
    ~print:drawn (named_pattern_gen 3)
    (fun pattern ->
      about pattern @@ fun settling typed bound ->
      I.Str_set.subset
        (I.ftv_ctx (I.Subst.to_ctx bound settling))
        (I.ftv_typ (I.Subst.apply typed.Typed.Pattern.typ settling)))

let law_a_pattern_binds_exactly_its_names =
  Test.make ~count:5000 ~name:"a pattern binds exactly the names it writes"
    ~print:drawn (named_pattern_gen 3)
    (fun pattern ->
      about pattern @@ fun _ _ bound ->
      Names.equal (binders pattern)
        (I.Name_map.fold
           (fun name _ known -> Names.add (Data.Name.base name) known)
           bound Names.empty))

let law_a_pattern_is_inferred_the_same_way_twice =
  Test.make ~count:5000 ~name:"inferring a pattern twice gives the same answer"
    ~print:drawn (named_pattern_gen 3)
    (fun pattern ->
      about pattern @@ fun _ left left_bound ->
      let _, right, right_bound = inferred_pattern pattern in
      Typed.Pattern.equal left right
      && I.Name_map.equal T.equal_scheme left_bound right_bound)

let saturated_gen =
  Gen.bind constructor_gen (fun constructor ->
      Gen.map
        (fun arguments ->
          ( constructor,
            with_distinct_binders
              (P.P_ctor (Data.Name.local constructor.written, arguments)) ))
        (Gen.flatten_list (List.map (of_flavour 2) constructor.payloads)))

let law_a_constructor_pattern_takes_its_declared_type =
  Test.make ~count:5000
    ~name:"a constructor pattern has the type its declaration gives it"
    ~print:(fun (constructor, pattern) -> constructor.written ^ ": " ^ drawn pattern)
    saturated_gen
    (fun (constructor, pattern) ->
      about pattern @@ fun settling typed _ ->
      match I.Subst.apply typed.Typed.Pattern.typ settling with
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
        P.P_ctor
          (Data.Name.local constructor.written, List.init given (fun _ -> P.P_anything))
      in
      match inferred_pattern pattern with
      | _ -> given = arity
      | exception Failure message ->
          given <> arity && Node_runner.contains ~needle:"takes" message)

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
  | result ->
      Some
        (List.sort compare
           (List.map
              (fun (declaration : Typed.Declaration.t) ->
                ( Data.Located.unwrap declaration.name,
                  Types.shape declaration.typ ))
              result.I.declarations))
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

let suite =
  QCheck_ounit.to_ounit2_test_list
    [
      law_unification_makes_both_sides_equal;
      law_unification_is_reflexive;
      law_unification_is_symmetric;
      law_unification_settles_in_one_pass;
      law_instantiation_only_renames;
      law_instantiation_is_fresh;
      law_instantiation_keeps_the_constraints;
      law_generalization_keeps_what_the_context_holds;
      law_generalization_takes_everything_the_context_lacks;
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
    ]
