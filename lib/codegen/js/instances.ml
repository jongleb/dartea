module J = Ast
module O = Optimized

type t = {
  types : (Data.Name.t, O.Typedecl.t) Hashtbl.t;
  claimed : (string, unit) Hashtbl.t;
  mutable define : J.stmt list;
}

let create typedecls =
  let types = Hashtbl.create 16 in
  List.iter
    (fun (decl : O.Typedecl.t) -> Hashtbl.replace types decl.name decl)
    typedecls;
  { types; claimed = Hashtbl.create 16; define = [] }

let declarations instances = List.rev instances.define

let instance instances name define =
  if not (Hashtbl.mem instances.claimed name) then begin
    Hashtbl.replace instances.claimed name ();
    let definition = define () in
    instances.define <- definition :: instances.define
  end;
  J.Identifier name

let expansion_budget = 8

let row_fields row =
  let rec walk collected (row : O.Type.t) =
    match row with
    | O.Type.TRowEmpty -> Some (List.rev collected)
    | O.Type.TRowExtend (label, field, rest) ->
        walk ((label, field) :: collected) rest
    | O.Type.TVar _ | O.Type.TInt | O.Type.TFloat | O.Type.TChar
    | O.Type.TStr | O.Type.TBool | O.Type.TUnit | O.Type.TFun _
    | O.Type.TTup _ | O.Type.TCustom _ | O.Type.TRecord _ ->
        None
  in
  walk [] row

let sorted_fields row =
  Option.map
    (List.sort (fun (one, _) (other, _) -> String.compare one other))
    (row_fields row)

let is_list_type name = Data.Name.equal name Typed.Type.list_name

let variant_of instances (name : Data.Name.t) arguments =
  Option.bind (Hashtbl.find_opt instances.types name) (fun decl ->
      O.Typedecl.constructors decl ~arguments)

let carries_payload (ctor : O.Typedecl.ctor) = not (List.is_empty ctor.payload)

let all_ctors_are_nullary instances (name : Data.Name.t) arguments =
  match variant_of instances name arguments with
  | None -> false
  | Some ctors -> not (List.exists carries_payload ctors)

let every_key keys =
  List.fold_right
    (fun key collected ->
      Option.bind collected (fun rest ->
          Option.map (fun key -> key :: rest) key))
    keys (Some [])

let is_numeric_variable variable =
  match Typed.Variable.constraint_of variable with
  | Some Data.Constraint.Number -> true
  | Some
      ( Data.Constraint.Comparable | Data.Constraint.Appendable
      | Data.Constraint.Comp_appendable )
  | None ->
      false

let rec key (ty : O.Type.t) : string option =
  let applied_to head arguments =
    Option.map
      (fun keys -> String.concat "$" (head :: keys))
      (every_key (List.map key arguments))
  in
  match ty with
  | O.Type.TInt | O.Type.TFloat | O.Type.TChar | O.Type.TStr | O.Type.TBool
  | O.Type.TUnit ->
      Option.map Primitives.name_of_builtin (Primitives.builtin_of_scalar ty)
  | O.Type.TTup components -> applied_to "Tuple" components
  | O.Type.TCustom (name, arguments) ->
      applied_to (Names.type_ident name) arguments
  | O.Type.TRecord row ->
      Option.bind (sorted_fields row) (fun fields ->
          let label (label, part) =
            Option.map (fun found -> label ^ "$" ^ found) (key part)
          in
          Option.map
            (fun keys -> String.concat "$" ("Record" :: keys))
            (every_key (List.map label fields)))
  | O.Type.TVar variable ->
      if is_numeric_variable variable then
        Some (Data.Constraint.name Data.Constraint.Number)
      else None
  | O.Type.TFun _ | O.Type.TRowExtend _ | O.Type.TRowEmpty -> None

let compared_in_place instances (operand : O.Type.t) =
  match operand with
  | O.Type.TInt | O.Type.TFloat | O.Type.TChar | O.Type.TStr | O.Type.TBool
  | O.Type.TUnit ->
      true
  | O.Type.TVar variable -> is_numeric_variable variable
  | O.Type.TCustom (name, arguments) ->
      (not (is_list_type name))
      && all_ctors_are_nullary instances name arguments
  | O.Type.TFun _ | O.Type.TTup _ | O.Type.TRecord _ | O.Type.TRowExtend _
  | O.Type.TRowEmpty ->
      false

let product_parts (ty : O.Type.t) =
  let at index subject = J.at_index subject index in
  let field label subject = J.member subject label in
  match ty with
  | O.Type.TTup components ->
      Some (List.mapi (fun index part -> (at index, part)) components)
  | O.Type.TRecord row ->
      Option.map
        (List.map (fun (label, part) -> (field label, part)))
        (sorted_fields row)
  | O.Type.TVar _ | O.Type.TInt | O.Type.TFloat | O.Type.TChar | O.Type.TStr
  | O.Type.TBool | O.Type.TUnit | O.Type.TFun _ | O.Type.TCustom _
  | O.Type.TRowExtend _ | O.Type.TRowEmpty ->
      None

let parts_within_budget ~budget operand =
  match product_parts operand with
  | Some parts when List.length parts <= budget -> Some parts
  | Some _ | None -> None

let conjunction = function
  | [] -> J.bool true
  | first :: rest -> List.fold_left (J.binary J.And) first rest

let strictly = function
  | J.LessThanOrEqual -> J.LessThan
  | J.GreaterThanOrEqual -> J.GreaterThan
  | operator -> operator

let operator_admits_equality = function
  | J.LessThanOrEqual | J.GreaterThanOrEqual -> true
  | J.Plus | J.Minus | J.Multiply | J.Divide | J.Modulo | J.Exponent | J.BitOr
  | J.Equal | J.NotEqual | J.StrictEqual | J.StrictNotEqual | J.LessThan
  | J.GreaterThan | J.And | J.Or ->
      false

let structurally instances ~budget operand ~in_place ~combining ~otherwise =
  if compared_in_place instances operand then in_place ()
  else
    match parts_within_budget ~budget operand with
    | Some parts -> combining ~budget:(budget - List.length parts) parts
    | None -> otherwise ()

let arrow_declaration name (parameters, body) =
  J.ConstDecl
    { name; init = J.Arrow { params = parameters; body = J.ArrowBlock body } }

let define instances name body =
  instance instances name (fun () -> arrow_declaration name (body ()))

let define_for instances tag ~keyed_by body =
  Option.map (fun found -> define instances (tag ^ found) body) (key keyed_by)

let head_of subject = J.member (J.Identifier subject) Runtime.head

let walking_both_lists step =
  let is_cons subject = J.binary J.StrictNotEqual (J.Identifier subject) (J.int 0) in
  [
    J.VarDecl { name = "left"; init = Some (J.Identifier "xs") };
    J.VarDecl { name = "right"; init = Some (J.Identifier "ys") };
    J.While
      {
        test = J.binary J.And (is_cons "left") (is_cons "right");
        body =
          step
          @ [
              J.assign "left" (J.member (J.Identifier "left") Runtime.tail);
              J.assign "right" (J.member (J.Identifier "right") Runtime.tail);
            ];
      };
  ]

let returning_first_difference name order =
  [
    J.ConstDecl { name; init = order };
    J.returning_when
      (J.binary J.StrictNotEqual (J.Identifier name) (J.int 0))
      (J.Identifier name);
  ]

let list_append () =
  let cell head tail = J.Object [ J.Field (Runtime.head, head); J.Field (Runtime.tail, tail) ] in
  let assign target value =
    J.ExprStmt (J.Assignment { left = target; right = value })
  in
  ( [ "xs"; "ys" ],
    [
      J.returning_when
        (J.binary J.StrictEqual (J.Identifier "xs") (J.int 0))
        (J.Identifier "ys");
      J.ConstDecl
        { name = "root"; init = cell (head_of "xs") (J.Identifier "ys") };
      J.VarDecl { name = "last"; init = Some (J.Identifier "root") };
      J.VarDecl
        { name = "rest"; init = Some (J.member (J.Identifier "xs") Runtime.tail) };
      J.While
        {
          test = J.binary J.StrictNotEqual (J.Identifier "rest") (J.int 0);
          body =
            [
              J.ConstDecl
                {
                  name = "copied";
                  init = cell (head_of "rest") (J.Identifier "ys");
                };
              assign (J.member (J.Identifier "last") Runtime.tail) (J.Identifier "copied");
              J.assign "last" (J.Identifier "copied");
              J.assign "rest" (J.member (J.Identifier "rest") Runtime.tail);
            ];
        };
      J.Return (Some (J.Identifier "root"));
    ] )

let rec equality_of instances ~budget (operand : O.Type.t) left right =
  structurally instances ~budget operand
    ~in_place:(fun () -> J.binary J.StrictEqual left right)
    ~combining:(fun ~budget parts ->
      let compare (read, part) =
        equality_of instances ~budget part (read left) (read right)
      in
      conjunction (List.map compare parts))
    ~otherwise:(fun () ->
      J.call
        (Option.value
           (equality_instance instances operand)
           ~default:Names.equal_reference)
        [ left; right ])

and three_way_of instances (operand : O.Type.t) left right =
  if compared_in_place instances operand then
    J.Conditional
      {
        test = J.binary J.StrictEqual left right;
        consequent = J.int 0;
        alternate =
          J.Conditional
            {
              test = J.binary J.LessThan left right;
              consequent = J.Unary { op = J.Negative; arg = J.int 1 };
              alternate = J.int 1;
            };
      }
  else
    J.call
      (Option.value
         (ordering_instance instances operand)
         ~default:Names.compare_reference)
      [ left; right ]

and ordering_of instances ~budget ~operator (operand : O.Type.t) left right =
  structurally instances ~budget operand
    ~in_place:(fun () -> J.binary operator left right)
    ~combining:(fun ~budget parts ->
      let rec lexicographic = function
        | [] -> J.bool (operator_admits_equality operator)
        | [ (read, last) ] ->
            ordering_of instances ~budget ~operator last (read left)
              (read right)
        | (read, part) :: rest ->
            J.Conditional
              {
                test = equality_of instances ~budget part (read left) (read right);
                consequent = lexicographic rest;
                alternate =
                  ordering_of instances ~budget ~operator:(strictly operator)
                    part (read left) (read right);
              }
      in
      lexicographic parts)
    ~otherwise:(fun () ->
      J.binary operator
        (J.call
           (Option.value
              (ordering_instance instances operand)
              ~default:Names.compare_reference)
           [ left; right ])
        (J.int 0))

and equality_instance instances (operand : O.Type.t) =
  match operand with
  | O.Type.TCustom (name, [ element ]) when is_list_type name ->
      define_for instances "$eq$List$" ~keyed_by:element
        (list_equality instances element)
  | O.Type.TCustom (name, arguments) ->
      Option.bind (variant_of instances name arguments) (fun ctors ->
          define_for instances "$eq$" ~keyed_by:operand
            (variant_equality instances ctors))
  | O.Type.TTup _ | O.Type.TRecord _ ->
      Option.bind (product_parts operand) (fun parts ->
          define_for instances "$eq$" ~keyed_by:operand
            (product_equality instances parts))
  | O.Type.TVar _ | O.Type.TInt | O.Type.TFloat | O.Type.TChar | O.Type.TStr
  | O.Type.TBool | O.Type.TUnit | O.Type.TFun _ | O.Type.TRowExtend _
  | O.Type.TRowEmpty ->
      None

and ordering_instance instances (operand : O.Type.t) =
  match operand with
  | O.Type.TCustom (name, [ element ]) when is_list_type name ->
      define_for instances "$cmp$List$" ~keyed_by:element
        (list_order instances element)
  | O.Type.TTup _ ->
      Option.bind (product_parts operand) (fun parts ->
          define_for instances "$cmp$" ~keyed_by:operand
            (product_order instances parts))
  | O.Type.TVar _ | O.Type.TInt | O.Type.TFloat | O.Type.TChar | O.Type.TStr
  | O.Type.TBool | O.Type.TUnit | O.Type.TFun _ | O.Type.TCustom _
  | O.Type.TRecord _ | O.Type.TRowExtend _ | O.Type.TRowEmpty ->
      None

and list_equality instances element () =
  ( [ "xs"; "ys" ],
    walking_both_lists
      [
        J.returning_when
          (J.Unary
             {
               op = J.Not;
               arg =
                 equality_of instances ~budget:expansion_budget element
                   (head_of "left") (head_of "right");
             })
          (J.bool false);
      ]
    @ [
        J.Return
          (Some
             (J.binary J.StrictEqual (J.Identifier "left")
                (J.Identifier "right")));
      ] )

and list_order instances element () =
  let exhaustion subject other =
    J.Conditional
      {
        test = J.binary J.StrictNotEqual (J.Identifier subject) (J.int 0);
        consequent = J.int 1;
        alternate =
          J.Conditional
            {
              test = J.binary J.StrictNotEqual (J.Identifier other) (J.int 0);
              consequent = J.Unary { op = J.Negative; arg = J.int 1 };
              alternate = J.int 0;
            };
      }
  in
  ( [ "xs"; "ys" ],
    walking_both_lists
      (returning_first_difference "ordering"
         (three_way_of instances element (head_of "left") (head_of "right")))
    @ [ J.Return (Some (exhaustion "left" "right")) ] )

and product_equality instances parts () =
  let compare (read, part) =
    equality_of instances ~budget:max_int part
      (read (J.Identifier "a"))
      (read (J.Identifier "b"))
  in
  ([ "a"; "b" ], [ J.Return (Some (conjunction (List.map compare parts))) ])

and product_order instances parts () =
  let compare index (read, part) =
    returning_first_difference
      (Printf.sprintf "ordering%d" index)
      (three_way_of instances part
         (read (J.Identifier "a"))
         (read (J.Identifier "b")))
  in
  ( [ "a"; "b" ],
    List.concat (List.mapi compare parts) @ [ J.Return (Some (J.int 0)) ] )

and payload_equality instances (ctor : O.Typedecl.ctor) =
  let compare index part =
    let payload subject =
      J.member (J.Identifier subject) (Runtime.payload index)
    in
    equality_of instances ~budget:expansion_budget part (payload "a")
      (payload "b")
  in
  conjunction (List.mapi compare ctor.payload)

and variant_equality instances ctors () =
  let identical = J.binary J.StrictEqual (J.Identifier "a") (J.Identifier "b") in
  let returning_unless test result =
    J.returning_when (J.Unary { op = J.Not; arg = test }) result
  in
  let guards =
    [
      returning_unless (J.is_object (J.Identifier "a")) identical;
      returning_unless (J.is_object (J.Identifier "b")) (J.bool false);
    ]
  in
  let tag subject = J.member (J.Identifier subject) Runtime.tag in
  let case (ctor : O.Typedecl.ctor) =
    {
      J.test = Some (J.string (Data.Name.base ctor.id));
      consequent = [ J.Return (Some (payload_equality instances ctor)) ];
    }
  in
  let rec cases = function
    | [] -> []
    | [ last ] -> [ { (case last) with J.test = None } ]
    | ctor :: rest -> case ctor :: cases rest
  in
  let body =
    match List.filter carries_payload ctors with
    | [] -> [ J.Return (Some identical) ]
    | [ only ] -> guards @ [ J.Return (Some (payload_equality instances only)) ]
    | carrying ->
        guards
        @ [
            J.returning_when
              (J.binary J.StrictNotEqual (tag "a") (tag "b"))
              (J.bool false);
            J.Switch { discriminant = tag "a"; cases = cases carrying };
          ]
  in
  ([ "a"; "b" ], body)

let append_instance instances (operand : O.Type.t) =
  match operand with
  | O.Type.TCustom (name, [ _ ]) when is_list_type name ->
      Some (define instances "$append$List" list_append)
  | O.Type.TVar _ | O.Type.TInt | O.Type.TFloat | O.Type.TChar | O.Type.TStr
  | O.Type.TBool | O.Type.TUnit | O.Type.TFun _ | O.Type.TTup _
  | O.Type.TCustom _ | O.Type.TRecord _ | O.Type.TRowExtend _
  | O.Type.TRowEmpty ->
      None

let append instances (operand : O.Type.t) left right =
  match operand with
  | O.Type.TStr -> J.binary J.Plus left right
  | O.Type.TVar _ | O.Type.TInt | O.Type.TFloat | O.Type.TChar | O.Type.TBool
  | O.Type.TUnit | O.Type.TFun _ | O.Type.TTup _ | O.Type.TCustom _
  | O.Type.TRecord _ | O.Type.TRowExtend _ | O.Type.TRowEmpty ->
      J.call
        (Option.value
           (append_instance instances operand)
           ~default:Names.append_reference)
        [ left; right ]

let equality instances operand left right =
  equality_of instances ~budget:expansion_budget operand left right

let inequality instances operand left right =
  if compared_in_place instances operand then J.binary J.StrictNotEqual left right
  else J.Unary { op = J.Not; arg = equality instances operand left right }

let order instances operand operator left right =
  ordering_of instances ~budget:expansion_budget ~operator operand left right

let integer_division left right =
  J.binary J.BitOr (J.binary J.Divide left right) (J.int 0)

let lower instances ~(operand : O.Type.t) :
    Data.Operator.t -> J.expr -> J.expr -> J.expr = function
  | Add -> J.binary J.Plus
  | Subtract -> J.binary J.Minus
  | Multiply -> J.binary J.Multiply
  | Divide -> J.binary J.Divide
  | Integer_divide -> integer_division
  | Power -> J.binary J.Exponent
  | Append -> append instances operand
  | Equal -> equality instances operand
  | Not_equal -> inequality instances operand
  | Less -> order instances operand J.LessThan
  | Less_or_equal -> order instances operand J.LessThanOrEqual
  | Greater -> order instances operand J.GreaterThan
  | Greater_or_equal -> order instances operand J.GreaterThanOrEqual
  | Conjunction -> J.binary J.And
  | Disjunction -> J.binary J.Or

let reads_operands_twice instances (operand : O.Type.t)
    (operator : Data.Operator.t) =
  let expands () =
    (not (compared_in_place instances operand))
    && Option.is_some (parts_within_budget ~budget:expansion_budget operand)
  in
  match operator with
  | Equal | Not_equal | Less | Less_or_equal | Greater | Greater_or_equal ->
      expands ()
  | Add | Subtract | Multiply | Divide | Integer_divide | Power | Append
  | Conjunction | Disjunction ->
      false
