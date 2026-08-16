module J = Ast
module O = Optimized
module DT = After_typed.Exhaustive.Decision_tree
module Occ = After_typed.Exhaustive.Occurrence

let js_reserved =
  [
    "abstract"; "arguments"; "await"; "boolean"; "break"; "byte"; "case";
    "catch"; "char"; "class"; "const"; "continue"; "debugger"; "default";
    "delete"; "do"; "double"; "else"; "enum"; "eval"; "export"; "extends";
    "false"; "final"; "finally"; "float"; "for"; "function"; "goto"; "if";
    "implements"; "import"; "in"; "instanceof"; "int"; "interface"; "let";
    "long"; "native"; "new"; "null"; "package"; "private"; "protected";
    "public"; "return"; "short"; "static"; "super"; "switch"; "synchronized";
    "this"; "throw"; "throws"; "transient"; "true"; "try"; "typeof"; "var";
    "void"; "volatile"; "while"; "with"; "yield"; "Array"; "Object"; "String";
    "Number"; "Boolean"; "Math"; "JSON"; "Date"; "RegExp"; "Map"; "Set";
    "Promise"; "Symbol"; "Error"; "console"; "globalThis"; "undefined"; "NaN";
    "Infinity"; "parseInt"; "parseFloat"; "isNaN"; "isFinite";
  ]

let reserved =
  let h = Hashtbl.create 128 in
  List.iter (fun w -> Hashtbl.replace h w ()) js_reserved;
  h

let is_reserved name = Hashtbl.mem reserved name

let starts_an_identifier = function
  | 'A' .. 'Z' | 'a' .. 'z' | '_' | '$' -> true
  | _ -> false

let continues_an_identifier = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '$' -> true
  | _ -> false

let is_valid_js_ident s =
  String.length s > 0
  && starts_an_identifier s.[0]
  && String.for_all continues_an_identifier s

let op_char_token = function
  | '+' -> "$plus" | '-' -> "$minus" | '*' -> "$star" | '/' -> "$slash"
  | '%' -> "$percent" | '=' -> "$eq" | '<' -> "$lt" | '>' -> "$gt"
  | '&' -> "$amp" | '|' -> "$pipe" | '!' -> "$bang" | '^' -> "$caret"
  | ':' -> "$colon" | '.' -> "$dot" | '~' -> "$tilde" | '?' -> "$question"
  | '@' -> "$at" | '#' -> "$hash"
  | c -> Printf.sprintf "$u%d" (Char.code c)

let sanitize (name : string) : string =
  if is_reserved name then "$$" ^ name
  else if is_valid_js_ident name then name
  else
    "$"
    ^ String.concat ""
        (List.map op_char_token
           (List.init (String.length name) (String.get name)))

let runtime_module_name = Runtime.module_name

let module_ident module_name =
  sanitize (String.concat "$" (String.split_on_char '.' module_name))

let js_of_name (name : Data.Name.t) =
  match name with
  | Data.Name.Local local -> sanitize local
  | Data.Name.Global { module_name; exported_name } ->
      module_ident module_name ^ "." ^ sanitize exported_name

let runtime_reference helper =
  J.Member
    {
      object_ = J.Identifier runtime_module_name;
      property = J.Identifier helper;
      computed = false;
    }

let curry_reference = runtime_reference Runtime.curry
let append_reference = runtime_reference Runtime.append
let equal_reference = runtime_reference Runtime.equal
let compare_reference = runtime_reference Runtime.compare

let expression_of_name (name : Data.Name.t) : J.expr =
  match name with
  | Data.Name.Local local -> J.Identifier (sanitize local)
  | Data.Name.Global { module_name; exported_name } ->
      J.Member
        {
          object_ = J.Identifier (module_ident module_name);
          property = J.Identifier (sanitize exported_name);
          computed = false;
        }

let jid name = expression_of_name name
let sname loc = sanitize (Data.Located.unwrap loc)

let temp_counter = ref 0

module SMap = Map.Make (Data.Name)

let name_counts : (string, int) Hashtbl.t = Hashtbl.create 64

let ctor_siblings : (Data.Name.t, (Data.Name.t * int) list) Hashtbl.t =
  Hashtbl.create 64

let ctor_siblings_of name = Hashtbl.find_opt ctor_siblings name
let js_arity : (Data.Name.t, int) Hashtbl.t = Hashtbl.create 64

let reset_names () =
  Hashtbl.clear name_counts;
  Hashtbl.clear ctor_siblings;
  Hashtbl.clear js_arity;
  temp_counter := 0

let reserve_name base =
  if not (Hashtbl.mem name_counts base) then Hashtbl.replace name_counts base 1

let fresh_js base =
  match Hashtbl.find_opt name_counts base with
  | None ->
      Hashtbl.replace name_counts base 1;
      base
  | Some n ->
      Hashtbl.replace name_counts base (n + 1);
      base ^ "$" ^ string_of_int n

let bind_one env src =
  let js = fresh_js (js_of_name src) in
  (SMap.add src js env, js)

let ref_name env src =
  match SMap.find_opt src env with Some js -> js | None -> js_of_name src

let jid_env env src =
  match SMap.find_opt src env with
  | Some js -> J.Identifier js
  | None -> expression_of_name src

let binary op left right = J.Binary { left; op; right }

let is_bool_constructor name =
  Data.Name.base name = "True" || Data.Name.base name = "False"

let is_unit_constructor name =
  Data.Name.base name = "Unit" || Data.Name.base name = "()"

let bool_literal name = J.Literal (J.Bool (Data.Name.base name = "True"))

let is_inline_constructor name =
  is_bool_constructor name || is_unit_constructor name

let is_tag_omitted name =
  match ctor_siblings_of name with
  | Some siblings -> (
      match List.filter (fun (_, arity) -> arity >= 1) siblings with
      | [ (only, _) ] -> Data.Name.equal only name
      | _ -> false)
  | None -> false

let payload_fields js_arguments =
  List.mapi (fun i a -> J.Field ("_" ^ string_of_int i, a)) js_arguments

let constructor_to_object name js_arguments =
  if is_bool_constructor name then bool_literal name
  else if is_unit_constructor name then J.Literal J.Null
  else if js_arguments = [] then J.Literal (J.String (Data.Name.base name))
  else if is_tag_omitted name then J.Object (payload_fields js_arguments)
  else
    J.Object
      (J.Field ("TAG", J.Literal (J.String (Data.Name.base name)))
      :: payload_fields js_arguments)

let rec is_record_construction (expr_node : O.Expr.t) =
  match expr_node.expr with
  | O.Expr.Expr_record_extend _ -> true
  | O.Expr.Expr_apply { fn; _ } -> is_record_construction fn
  | _ -> false

let rec extract_record_fields (expr_node : O.Expr.t) : (string * O.Expr.t) list
    =
  match expr_node.expr with
  | O.Expr.Expr_apply { fn; arg } -> (
      match fn.expr with
      | O.Expr.Expr_record_extend field -> [ (field, arg) ]
      | O.Expr.Expr_apply { fn = inner_fn; arg = inner_arg } -> (
          match inner_fn.expr with
          | O.Expr.Expr_record_extend field ->
              (field, inner_arg) :: extract_record_fields arg
          | _ -> [])
      | _ -> [])
  | O.Expr.Expr_record_empty -> []
  | _ -> []

let rec collect_args acc (fn : O.Expr.t) =
  match fn.expr with
  | O.Expr.Expr_apply { fn = inner_fn; arg = inner_arg } ->
      collect_args (inner_arg :: acc) inner_fn
  | _ -> (fn, acc)

let rec list_to_cons_cells = function
  | [] -> J.Literal (J.Int 0)
  | hd :: tl ->
      J.Object [ J.Field ("hd", hd); J.Field ("tl", list_to_cons_cells tl) ]

let needs_temp_var = function
  | J.Identifier _ | J.Literal _ -> false
  | _ -> true

let fresh_temp () =
  incr temp_counter;
  "$s" ^ string_of_int !temp_counter

let member object_ property =
  J.Member { object_; property = J.Identifier property; computed = false }

let integer_division left right =
  binary J.BitOr (binary J.Divide left right) (J.Literal (J.Int 0))

let indexed_member object_ index =
  J.Member { object_; property = J.Literal (J.Int index); computed = true }

let assign_stmt r e =
  J.ExprStmt (J.Assignment { left = J.Identifier r; right = e })

let shared_operand expression =
  if needs_temp_var expression then
    let name = fresh_temp () in
    ([ J.ConstDecl { name; init = expression } ], J.Identifier name)
  else ([], expression)

let strictly = function
  | J.LessThanOrEqual -> J.LessThan
  | J.GreaterThanOrEqual -> J.GreaterThan
  | operator -> operator

let operator_admits_equality = function
  | J.LessThanOrEqual | J.GreaterThanOrEqual -> true
  | _ -> false

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

let is_list_type name = String.equal (Data.Name.base name) "List"

let type_ident (name : Data.Name.t) =
  match name with
  | Data.Name.Local base -> base
  | Data.Name.Global { module_name; exported_name } ->
      module_ident module_name ^ "$" ^ exported_name

let visible_types : (Data.Name.t, O.Typedecl.t) Hashtbl.t = Hashtbl.create 16
let claimed_instances : (string, unit) Hashtbl.t = Hashtbl.create 16
let instance_definitions : (string * J.stmt) list ref = ref []

let reset_instances () =
  Hashtbl.reset claimed_instances;
  instance_definitions := []

let variant_of (name : Data.Name.t) arguments =
  Option.bind (Hashtbl.find_opt visible_types name) (fun decl ->
      O.Typedecl.constructors decl ~arguments)

let carries_payload (ctor : O.Typedecl.ctor) = ctor.payload <> []

let all_ctors_are_nullary (name : Data.Name.t) arguments =
  match variant_of name arguments with
  | None -> false
  | Some ctors -> not (List.exists carries_payload ctors)

let instance_declarations () =
  List.rev_map (fun (_, definition) -> definition) !instance_definitions

let instance name define =
  if not (Hashtbl.mem claimed_instances name) then begin
    Hashtbl.replace claimed_instances name ();
    let definition = define () in
    instance_definitions := (name, definition) :: !instance_definitions
  end;
  J.Identifier name

let every_key keys =
  List.fold_right
    (fun key collected ->
      Option.bind collected (fun rest ->
          Option.map (fun key -> key :: rest) key))
    keys (Some [])

let is_numeric_variable variable =
  match Data.Constraint.of_variable variable with
  | Some Data.Constraint.Number -> true
  | Some
      ( Data.Constraint.Comparable | Data.Constraint.Appendable
      | Data.Constraint.Comp_appendable )
  | None ->
      false

let rec instance_key (ty : O.Type.t) : string option =
  let applied_to head arguments =
    Option.map
      (fun keys -> String.concat "$" (head :: keys))
      (every_key (List.map instance_key arguments))
  in
  match ty with
  | O.Type.TInt -> Some "Int"
  | O.Type.TFloat -> Some "Float"
  | O.Type.TChar -> Some "Char"
  | O.Type.TStr -> Some "String"
  | O.Type.TBool -> Some "Bool"
  | O.Type.TUnit -> Some "Unit"
  | O.Type.TTup components -> applied_to "Tuple" components
  | O.Type.TCustom (name, arguments) -> applied_to (type_ident name) arguments
  | O.Type.TRecord row ->
      Option.bind (sorted_fields row) (fun fields ->
          let labelled (label, part) =
            Option.map (fun key -> label ^ "$" ^ key) (instance_key part)
          in
          Option.map
            (fun keys -> String.concat "$" ("Record" :: keys))
            (every_key (List.map labelled fields)))
  | O.Type.TVar variable ->
      if is_numeric_variable variable then
        Some (Data.Constraint.name Data.Constraint.Number)
      else None
  | O.Type.TFun _ | O.Type.TRowExtend _ | O.Type.TRowEmpty -> None

let expansion_budget = 8

let compared_in_place (operand : O.Type.t) =
  match operand with
  | O.Type.TInt | O.Type.TFloat | O.Type.TChar | O.Type.TStr | O.Type.TBool
  | O.Type.TUnit ->
      true
  | O.Type.TVar variable -> is_numeric_variable variable
  | O.Type.TCustom (name, arguments) ->
      (not (is_list_type name)) && all_ctors_are_nullary name arguments
  | O.Type.TFun _ | O.Type.TTup _ | O.Type.TRecord _ | O.Type.TRowExtend _
  | O.Type.TRowEmpty ->
      false

let product_parts (ty : O.Type.t) =
  let at index subject = indexed_member subject index in
  let field label subject = member subject label in
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
  | [] -> J.Literal (J.Bool true)
  | first :: rest -> List.fold_left (binary J.And) first rest

let call callee arguments = J.Call { callee; args = arguments }

let rec equality_of ~budget (operand : O.Type.t) left right =
  if compared_in_place operand then binary J.StrictEqual left right
  else
    match parts_within_budget ~budget operand with
    | Some parts ->
        let budget = budget - List.length parts in
        let compared (read, part) =
          equality_of ~budget part (read left) (read right)
        in
        conjunction (List.map compared parts)
    | None ->
        call
          (Option.value (equality_instance operand) ~default:equal_reference)
          [ left; right ]

and three_way_of (operand : O.Type.t) left right =
  if compared_in_place operand then
    J.Conditional
      {
        test = binary J.StrictEqual left right;
        consequent = J.Literal (J.Int 0);
        alternate =
          J.Conditional
            {
              test = binary J.LessThan left right;
              consequent =
                J.Unary { op = J.Negative; arg = J.Literal (J.Int 1) };
              alternate = J.Literal (J.Int 1);
            };
      }
  else
    call
      (Option.value (ordering_instance operand) ~default:compare_reference)
      [ left; right ]

and ordering_of ~budget ~operator (operand : O.Type.t) left right =
  if compared_in_place operand then binary operator left right
  else
    match parts_within_budget ~budget operand with
    | Some parts ->
        let budget = budget - List.length parts in
        let rec lexicographic = function
          | [] -> J.Literal (J.Bool (operator_admits_equality operator))
          | [ (read, last) ] ->
              ordering_of ~budget ~operator last (read left) (read right)
          | (read, part) :: rest ->
              J.Conditional
                {
                  test = equality_of ~budget part (read left) (read right);
                  consequent = lexicographic rest;
                  alternate =
                    ordering_of ~budget ~operator:(strictly operator) part
                      (read left) (read right);
                }
        in
        lexicographic parts
    | None ->
        binary operator
          (call
             (Option.value (ordering_instance operand)
                ~default:compare_reference)
             [ left; right ])
          (J.Literal (J.Int 0))

and equality_instance (operand : O.Type.t) =
  match operand with
  | O.Type.TCustom (name, [ element ]) when is_list_type name ->
      Option.map
        (fun key -> defined ("$eq$List$" ^ key) (list_equality element))
        (instance_key element)
  | O.Type.TCustom (name, arguments) ->
      Option.bind (variant_of name arguments) (fun ctors ->
          Option.map
            (fun key -> defined ("$eq$" ^ key) (variant_equality ctors))
            (instance_key operand))
  | O.Type.TTup _ | O.Type.TRecord _ ->
      Option.bind (product_parts operand) (fun parts ->
          Option.map
            (fun key -> defined ("$eq$" ^ key) (product_equality parts))
            (instance_key operand))
  | O.Type.TVar _ | O.Type.TInt | O.Type.TFloat | O.Type.TChar | O.Type.TStr
  | O.Type.TBool | O.Type.TUnit | O.Type.TFun _ | O.Type.TRowExtend _
  | O.Type.TRowEmpty ->
      None

and append_instance (operand : O.Type.t) =
  match operand with
  | O.Type.TCustom (name, [ _ ]) when is_list_type name ->
      Some (defined "$append$List" list_append)
  | O.Type.TVar _ | O.Type.TInt | O.Type.TFloat | O.Type.TChar | O.Type.TStr
  | O.Type.TBool | O.Type.TUnit | O.Type.TFun _ | O.Type.TTup _
  | O.Type.TCustom _ | O.Type.TRecord _ | O.Type.TRowExtend _
  | O.Type.TRowEmpty ->
      None

and list_append () =
  let cell head tail =
    J.Object [ J.Field ("hd", head); J.Field ("tl", tail) ]
  in
  let assign target value = J.ExprStmt (J.Assignment { left = target; right = value }) in
  ( [ "xs"; "ys" ],
    [
      J.If
        {
          test =
            binary J.StrictEqual (J.Identifier "xs") (J.Literal (J.Int 0));
          consequent = [ J.Return (Some (J.Identifier "ys")) ];
          alternate = None;
        };
      J.ConstDecl
        { name = "root"; init = cell (head_of "xs") (J.Identifier "ys") };
      J.VarDecl { name = "last"; init = Some (J.Identifier "root") };
      J.VarDecl
        { name = "rest"; init = Some (member (J.Identifier "xs") "tl") };
      J.While
        {
          test =
            binary J.StrictNotEqual (J.Identifier "rest") (J.Literal (J.Int 0));
          body =
            [
              J.ConstDecl
                {
                  name = "copied";
                  init = cell (head_of "rest") (J.Identifier "ys");
                };
              assign
                (member (J.Identifier "last") "tl")
                (J.Identifier "copied");
              assign_stmt "last" (J.Identifier "copied");
              assign_stmt "rest" (member (J.Identifier "rest") "tl");
            ];
        };
      J.Return (Some (J.Identifier "root"));
    ] )

and ordering_instance (operand : O.Type.t) =
  match operand with
  | O.Type.TCustom (name, [ element ]) when is_list_type name ->
      Option.map
        (fun key -> defined ("$cmp$List$" ^ key) (list_ordering element))
        (instance_key element)
  | O.Type.TTup _ ->
      Option.bind (product_parts operand) (fun parts ->
          Option.map
            (fun key -> defined ("$cmp$" ^ key) (product_ordering parts))
            (instance_key operand))
  | O.Type.TVar _ | O.Type.TInt | O.Type.TFloat | O.Type.TChar | O.Type.TStr
  | O.Type.TBool | O.Type.TUnit | O.Type.TFun _ | O.Type.TCustom _
  | O.Type.TRecord _ | O.Type.TRowExtend _ | O.Type.TRowEmpty ->
      None

and defined name body = instance name (fun () -> arrow_declaration name (body ()))

and arrow_declaration name (parameters, body) =
  J.ConstDecl
    { name; init = J.Arrow { params = parameters; body = J.ArrowBlock body } }

and walking_both_lists step =
  let is_cons subject =
    binary J.StrictNotEqual (J.Identifier subject) (J.Literal (J.Int 0))
  in
  [
    J.VarDecl { name = "left"; init = Some (J.Identifier "xs") };
    J.VarDecl { name = "right"; init = Some (J.Identifier "ys") };
    J.While
      {
        test = binary J.And (is_cons "left") (is_cons "right");
        body =
          step
          @ [
              assign_stmt "left" (member (J.Identifier "left") "tl");
              assign_stmt "right" (member (J.Identifier "right") "tl");
            ];
      };
  ]

and head_of subject = member (J.Identifier subject) "hd"

and list_equality element () =
  ( [ "xs"; "ys" ],
    walking_both_lists
      [
        J.If
          {
            test =
              J.Unary
                {
                  op = J.Not;
                  arg =
                    equality_of ~budget:expansion_budget element
                      (head_of "left") (head_of "right");
                };
            consequent = [ J.Return (Some (J.Literal (J.Bool false))) ];
            alternate = None;
          };
      ]
    @ [
        J.Return
          (Some
             (binary J.StrictEqual (J.Identifier "left") (J.Identifier "right")));
      ] )

and returning_first_difference name ordering =
  [
    J.ConstDecl { name; init = ordering };
    J.If
      {
        test =
          binary J.StrictNotEqual (J.Identifier name) (J.Literal (J.Int 0));
        consequent = [ J.Return (Some (J.Identifier name)) ];
        alternate = None;
      };
  ]

and list_ordering element () =
  let exhausted subject other =
    J.Conditional
      {
        test =
          binary J.StrictNotEqual (J.Identifier subject) (J.Literal (J.Int 0));
        consequent = J.Literal (J.Int 1);
        alternate =
          J.Conditional
            {
              test =
                binary J.StrictNotEqual (J.Identifier other)
                  (J.Literal (J.Int 0));
              consequent =
                J.Unary { op = J.Negative; arg = J.Literal (J.Int 1) };
              alternate = J.Literal (J.Int 0);
            };
      }
  in
  ( [ "xs"; "ys" ],
    walking_both_lists
      (returning_first_difference "ordering"
         (three_way_of element (head_of "left") (head_of "right")))
    @ [ J.Return (Some (exhausted "left" "right")) ] )

and product_equality parts () =
  let compared (read, part) =
    equality_of ~budget:max_int part
      (read (J.Identifier "a"))
      (read (J.Identifier "b"))
  in
  ([ "a"; "b" ], [ J.Return (Some (conjunction (List.map compared parts))) ])

and product_ordering parts () =
  let compared index (read, part) =
    returning_first_difference
      (Printf.sprintf "ordering%d" index)
      (three_way_of part (read (J.Identifier "a")) (read (J.Identifier "b")))
  in
  ( [ "a"; "b" ],
    List.concat (List.mapi compared parts)
    @ [ J.Return (Some (J.Literal (J.Int 0))) ] )

and payload_equality (ctor : O.Typedecl.ctor) =
  let compared index part =
    let payload subject = member (J.Identifier subject) ("_" ^ string_of_int index) in
    equality_of ~budget:expansion_budget part (payload "a") (payload "b")
  in
  conjunction (List.mapi compared ctor.payload)

and variant_equality ctors () =
  let is_object subject =
    binary J.StrictEqual
      (J.Unary { op = J.Typeof; arg = J.Identifier subject })
      (J.Literal (J.String "object"))
  in
  let identical =
    binary J.StrictEqual (J.Identifier "a") (J.Identifier "b")
  in
  let returning_unless test result =
    J.If
      {
        test = J.Unary { op = J.Not; arg = test };
        consequent = [ J.Return (Some result) ];
        alternate = None;
      }
  in
  let guards =
    [
      returning_unless (is_object "a") identical;
      returning_unless (is_object "b") (J.Literal (J.Bool false));
    ]
  in
  let tag subject = member (J.Identifier subject) "TAG" in
  let case (ctor : O.Typedecl.ctor) =
    {
      J.test = Some (J.Literal (J.String (Data.Name.base ctor.id)));
      consequent = [ J.Return (Some (payload_equality ctor)) ];
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
    | [ only ] -> guards @ [ J.Return (Some (payload_equality only)) ]
    | carrying ->
        guards
        @ [
            J.If
              {
                test = binary J.StrictNotEqual (tag "a") (tag "b");
                consequent = [ J.Return (Some (J.Literal (J.Bool false))) ];
                alternate = None;
              };
            J.Switch { discriminant = tag "a"; cases = cases carrying };
          ]
  in
  ([ "a"; "b" ], body)

let appending (operand : O.Type.t) left right =
  match operand with
  | O.Type.TStr -> binary J.Plus left right
  | O.Type.TVar _ | O.Type.TInt | O.Type.TFloat | O.Type.TChar | O.Type.TBool
  | O.Type.TUnit | O.Type.TFun _ | O.Type.TTup _ | O.Type.TCustom _
  | O.Type.TRecord _ | O.Type.TRowExtend _ | O.Type.TRowEmpty ->
      call
        (Option.value (append_instance operand) ~default:append_reference)
        [ left; right ]

let equality operand left right =
  equality_of ~budget:expansion_budget operand left right

let inequality operand left right =
  if compared_in_place operand then binary J.StrictNotEqual left right
  else J.Unary { op = J.Not; arg = equality operand left right }

let ordering operand operator left right =
  ordering_of ~budget:expansion_budget ~operator operand left right

let lowering ~(operand : O.Type.t) :
    Data.Operator.t -> J.expr -> J.expr -> J.expr = function
  | Add -> binary J.Plus
  | Subtract -> binary J.Minus
  | Multiply -> binary J.Multiply
  | Divide -> binary J.Divide
  | Integer_divide -> integer_division
  | Power -> binary J.Exponent
  | Append -> appending operand
  | Equal -> equality operand
  | Not_equal -> inequality operand
  | Less -> ordering operand J.LessThan
  | Less_or_equal -> ordering operand J.LessThanOrEqual
  | Greater -> ordering operand J.GreaterThan
  | Greater_or_equal -> ordering operand J.GreaterThanOrEqual
  | Conjunction -> binary J.And
  | Disjunction -> binary J.Or

let reads_operands_twice (operand : O.Type.t) (operator : Data.Operator.t) =
  let expanded () =
    (not (compared_in_place operand))
    && Option.is_some (parts_within_budget ~budget:expansion_budget operand)
  in
  match operator with
  | Equal | Not_equal | Less | Less_or_equal | Greater | Greater_or_equal ->
      expanded ()
  | Add | Subtract | Multiply | Divide | Integer_divide | Power | Append
  | Conjunction | Disjunction ->
      false

let shared_operands ~when_duplicated left right =
  if when_duplicated then
    let left_bindings, left = shared_operand left in
    let right_bindings, right = shared_operand right in
    (left_bindings @ right_bindings, left, right)
  else ([], left, right)

let binary_lowering ~(operand : O.Type.t) (operator : Data.Operator.t) left
    right : J.stmt list * J.expr =
  let bindings, left, right =
    shared_operands
      ~when_duplicated:(reads_operands_twice operand operator)
      left right
  in
  (bindings, lowering ~operand operator left right)

let method_lowering (method_ : Data.Method.t) ~operand left right =
  let bindings, left, right = shared_operands ~when_duplicated:true left right in
  let picking test = J.Conditional { test; consequent = left; alternate = right } in
  match method_ with
  | Minimum ->
      ( bindings,
        picking
          (ordering_of ~budget:expansion_budget ~operator:J.LessThan operand
             left right) )
  | Maximum ->
      ( bindings,
        picking
          (ordering_of ~budget:expansion_budget ~operator:J.GreaterThan operand
             left right) )
  | Compare ->
      let name = fresh_temp () in
      let ordering = J.Identifier name in
      let result = Data.Method.ordering_result in
      ( bindings
        @ [ J.ConstDecl { name; init = three_way_of operand left right } ],
        J.Conditional
          {
            test = binary J.LessThan ordering (J.Literal (J.Int 0));
            consequent = constructor_to_object result.less [];
            alternate =
              J.Conditional
                {
                  test = binary J.StrictEqual ordering (J.Literal (J.Int 0));
                  consequent = constructor_to_object result.equal [];
                  alternate = constructor_to_object result.greater [];
                };
          } )

let saturated_lowering env name ~operand =
  if SMap.mem name env then None
  else
    match Data.Operator.referred_to_by name with
    | Some operator -> Some (binary_lowering ~operand operator)
    | None ->
        Option.map
          (fun method_ -> method_lowering method_ ~operand)
          (Data.Method.referred_to_by name)

let js_eq left lit = J.Binary { left; op = J.StrictEqual; right = J.Literal lit }

let js_ne_zero occ =
  J.Binary { left = occ; op = J.StrictNotEqual; right = J.Literal (J.Int 0) }

let js_is_object occ =
  J.Binary
    {
      left = J.Unary { op = J.Typeof; arg = occ };
      op = J.StrictEqual;
      right = J.Literal (J.String "object");
    }

let occ_expr root (o : Occ.t) : J.expr =
  List.fold_left
    (fun e step ->
      match step with
      | Occ.Payload i -> member e ("_" ^ string_of_int i)
      | Occ.Index i -> indexed_member e i
      | Occ.Field f -> member e f
      | Occ.Hd -> member e "hd"
      | Occ.Tl -> member e "tl")
    root o

let ctor_literal name =
  if is_bool_constructor name then J.Bool (Data.Name.base name = "True")
  else J.String (Data.Name.base name)

let test_expr occ_e (test : DT.test) : J.expr =
  match test with
  | DT.Test_ctor name -> js_eq occ_e (ctor_literal name)
  | DT.Test_tag name ->
      if is_tag_omitted name then js_is_object occ_e
      else js_eq (member occ_e "TAG") (J.String (Data.Name.base name))
  | DT.Test_int n -> js_eq occ_e (J.Int n)
  | DT.Test_str s -> js_eq occ_e (J.String s)
  | DT.Test_chr c -> js_eq occ_e (J.String c)
  | DT.Test_nil -> js_eq occ_e (J.Int 0)
  | DT.Test_cons -> js_ne_zero occ_e

type discriminant = By_tag | By_value

let switch_key (test : DT.test) : (discriminant * J.literal) option =
  match test with
  | DT.Test_tag n when not (is_tag_omitted n) ->
      Some (By_tag, J.String (Data.Name.base n))
  | DT.Test_tag _ -> None
  | DT.Test_ctor n when not (is_bool_constructor n) ->
      Some (By_value, J.String (Data.Name.base n))
  | DT.Test_int n -> Some (By_value, J.Int n)
  | DT.Test_str s -> Some (By_value, J.String s)
  | DT.Test_chr c -> Some (By_value, J.String c)
  | DT.Test_ctor _ | DT.Test_nil | DT.Test_cons -> None

let switch_plan occ_e (branches : (DT.test * DT.t) list) :
    (J.expr * (J.literal * DT.t) list) option =
  let keyed =
    List.fold_right
      (fun (test, subtree) collected ->
        match (collected, switch_key test) with
        | Some cases, Some (kind, literal) ->
            Some ((kind, literal, subtree) :: cases)
        | _ -> None)
      branches (Some [])
  in
  match keyed with
  | None | Some [] -> None
  | Some ((kind, _, _) :: _ as cases) ->
      if List.for_all (fun (other, _, _) -> other = kind) cases then
        let discriminant =
          match kind with By_tag -> member occ_e "TAG" | By_value -> occ_e
        in
        Some
          ( discriminant,
            List.map (fun (_, literal, subtree) -> (literal, subtree)) cases )
      else None

let match_failure =
  [
    J.Throw
      (J.New
         {
           callee = J.Identifier "Error";
           args = [ J.Literal (J.String "Pattern match failed") ];
         });
  ]


let curry_call f args =
  J.Call { callee = curry_reference; args = [ f; J.Array args ] }

let split_at n lst =
  let rec go i acc = function
    | rest when i = 0 -> (List.rev acc, rest)
    | x :: rest -> go (i - 1) (x :: acc) rest
    | [] -> (List.rev acc, [])
  in
  go n [] lst

let method_lowering (method_ : Data.Method.t) ~operand left right =
  let left_bindings, left = shared_operand left in
  let right_bindings, right = shared_operand right in
  let bindings = left_bindings @ right_bindings in
  match method_ with
  | Minimum ->
      ( bindings,
        J.Conditional
          {
            test = ordering_of ~budget:expansion_budget ~operator:J.LessThan
                     operand left right;
            consequent = left;
            alternate = right;
          } )
  | Maximum ->
      ( bindings,
        J.Conditional
          {
            test = ordering_of ~budget:expansion_budget ~operator:J.GreaterThan
                     operand left right;
            consequent = left;
            alternate = right;
          } )
  | Compare ->
      let name = fresh_temp () in
      let ordering = J.Identifier name in
      let result = Data.Method.ordering_result in
      ( bindings
        @ [ J.ConstDecl { name; init = three_way_of operand left right } ],
        J.Conditional
          {
            test = binary J.LessThan ordering (J.Literal (J.Int 0));
            consequent = constructor_to_object result.less [];
            alternate =
              J.Conditional
                {
                  test = binary J.StrictEqual ordering (J.Literal (J.Int 0));
                  consequent = constructor_to_object result.equal [];
                  alternate = constructor_to_object result.greater [];
                };
          } )

let saturated_lowering env name ~operand =
  if SMap.mem name env then None
  else
    match Data.Operator.referred_to_by name with
    | Some operator -> Some (binary_lowering ~operand operator)
    | None ->
        Option.map
          (fun method_ -> method_lowering method_ ~operand)
          (Data.Method.referred_to_by name)

let declared_arity env name =
  if SMap.mem name env then None else Hashtbl.find_opt js_arity name

type arity = After_typed.Arity.t = Exactly of int | At_least of int

let arity_of_type = After_typed.Arity.of_type

let closure_partial callee args missing =
  let rparams = List.init missing (fun _ -> fresh_temp ()) in
  let rargs = List.map (fun p -> J.Identifier p) rparams in
  J.Arrow
    {
      params = rparams;
      body = J.ArrowExpr (J.Call { callee; args = args @ rargs });
    }

let fold_emit (f : 'a -> J.stmt list * 'b) (items : 'a list) :
    J.stmt list * 'b list =
  let stmts, vals =
    List.fold_left
      (fun (sacc, vacc) item ->
        let s, v = f item in
        (List.rev_append s sacc, v :: vacc))
      ([], []) items
  in
  (List.rev stmts, List.rev vals)

type tctx = {
  fn : Data.Name.t;
  params : string list;
  mutable triggered : bool;
}

let bind_binds env binds =
  let env, rev =
    List.fold_left
      (fun (env, acc) (src, occ) ->
        let env, js = bind_one env src in
        (env, J.ConstDecl { name = js; init = occ } :: acc))
      (env, []) binds
  in
  (env, List.rev rev)

let bind_params env names =
  let env, rev =
    List.fold_left
      (fun (env, acc) src ->
        let env, js = bind_one env src in
        (env, js :: acc))
      (env, []) names
  in
  (env, List.rev rev)

let accessor_arrow field =
  J.Arrow
    {
      params = [ "r" ];
      body =
        J.ArrowExpr (member (J.Identifier "r") (Data.Located.unwrap field));
    }

let arrow_of_body params stmts =
  let body =
    match stmts with
    | [ J.Return (Some e) ] -> J.ArrowExpr e
    | _ -> J.ArrowBlock stmts
  in
  J.Arrow { params; body }

module DS = After_typed.Decision_share

let thunk_names plan =
  List.map
    (fun (id, _) -> (id, fresh_js ("$dt" ^ string_of_int id)))
    (DS.shared plan)

let rec lower env root ~terminating ~leaf ~fail ~sink ~plan ~tnames
    (tree : DT.t) : J.stmt list =
  match DS.id_of plan tree with
  | Some id ->
      sink
        (J.Call
           { callee = J.Identifier (List.assoc id tnames); args = [] })
  | None -> lower_node env root ~terminating ~leaf ~fail ~sink ~plan ~tnames tree

and lower_node env root ~terminating ~leaf ~fail ~sink ~plan ~tnames
    (tree : DT.t) : J.stmt list =
  match tree with
  | DT.Fail -> fail
  | DT.Leaf { action; bindings } ->
      let jbinds =
        List.map (fun (v, o) -> (Data.Name.local v, occ_expr root o)) bindings
      in
      let env', bstmts = bind_binds env jbinds in
      bstmts @ leaf env' action
  | DT.Switch { occurrence; branches; default } -> (
      let occ_e = occ_expr root occurrence in
      let go tr =
        lower env root ~terminating ~leaf ~fail ~sink ~plan ~tnames tr
      in
      match (if terminating then switch_plan occ_e branches else None) with
      | Some (disc, cases) when List.length cases >= 2 ->
          let default_case =
            match default with
            | Some t -> [ { J.test = None; consequent = go t } ]
            | None -> []
          in
          let js_cases =
            List.map
              (fun (lit, tr) ->
                { J.test = Some (J.Literal lit); consequent = go tr })
              cases
          in
          [ J.Switch { discriminant = disc; cases = js_cases @ default_case } ]
      | _ ->
          let rec build = function
            | [] -> begin
                match default with Some t -> go t | None -> fail
              end
            | [ (_, tr) ] when default = None -> go tr
            | (test, tr) :: rest ->
                [
                  J.If
                    {
                      test = test_expr occ_e test;
                      consequent = go tr;
                      alternate = Some (build rest);
                    };
                ]
          in
          build branches)

let shared_thunks env root ~plan ~tnames clause_expr =
  let sink e = [ J.Return (Some e) ] in
  let leaf env action =
    let sa, ea = clause_expr env action in
    sa @ [ J.Return (Some ea) ]
  in
  List.map
    (fun (id, sub) ->
      let body =
        lower_node env root ~terminating:true ~leaf
          ~fail:match_failure ~sink ~plan ~tnames sub
      in
      J.ConstDecl { name = List.assoc id tnames; init = arrow_of_body [] body })
    (DS.shared plan)

let rec emit_value env (e : O.Expr.t) : J.stmt list * J.expr =
  let statements, expression = emit_uncoerced env e in
  ( statements,
    coerced expression ~expected:(arity_of_type e.O.Expr.typ)
      ~actual:(emitted_arity env e) )

and coerced expression ~expected ~actual =
  match (expected, actual) with
  | Exactly wanted, Exactly given
    when wanted <> given && wanted >= 1 && given >= 1 ->
      let params = List.init wanted (fun _ -> fresh_temp ()) in
      let arguments = List.map (fun p -> J.Identifier p) params in
      let call_in_two_steps () =
        let saturating, extra = split_at given arguments in
        J.Call
          {
            callee = J.Call { callee = expression; args = saturating };
            args = extra;
          }
      in
      let body =
        if given < wanted then call_in_two_steps ()
        else closure_partial expression arguments (given - wanted)
      in
      J.Arrow { params; body = J.ArrowExpr body }
  | (Exactly _ | At_least _), (Exactly _ | At_least _) -> expression

and emitted_arity env (e : O.Expr.t) : arity =
  match e.expr with
  | O.Expr.Expr_lambda { params; _ } -> Exactly (List.length params)
  | O.Expr.Expr_kernel (Kernel_value kernel) ->
      Exactly (Data.Kernel.arity kernel)
  | O.Expr.Expr_constr { name; arguments } ->
      let supplied = List.length arguments in
      begin
        match declared_arity env name with
        | Some n when n > supplied -> Exactly (n - supplied)
        | Some _ | None -> arity_of_type e.typ
      end
  | O.Expr.Expr_ident _ -> callee_arity env e
  | O.Expr.Expr_apply { fn; _ } when is_record_construction fn -> Exactly 0
  | O.Expr.Expr_apply { fn; arg } ->
      let callee, args = collect_args [ arg ] fn in
      let supplied = List.length args in
      begin
        match callee_arity env callee with
        | Exactly n when n > supplied -> Exactly (n - supplied)
        | Exactly n when n >= 1 -> arity_of_type e.typ
        | Exactly _ | At_least _ -> At_least 0
      end
  | O.Expr.Expr_binop _ | O.Expr.Expr_let _ | O.Expr.Expr_if_then_else _
  | O.Expr.Expr_record _ | O.Expr.Expr_record_update _
  | O.Expr.Expr_pattern _ | O.Expr.Expr_accessor _
  | O.Expr.Expr_access _ | O.Expr.Expr_record_extend _
  | O.Expr.Expr_record_select _ | O.Expr.Expr_record_empty | O.Expr.Expr_unit
  | O.Expr.Expr_kernel _ | O.Expr.Expr_char _ | O.Expr.Expr_string _
  | O.Expr.Expr_int _ | O.Expr.Expr_float _ | O.Expr.Expr_list _
  | O.Expr.Expr_cons _ | O.Expr.Expr_tuple _ ->
      arity_of_type e.typ

and callee_arity env (callee : O.Expr.t) : arity =
  match callee.expr with
  | O.Expr.Expr_ident name -> begin
      match declared_arity env name with
      | Some n -> Exactly n
      | None -> arity_of_type callee.typ
    end
  | _ -> arity_of_type callee.typ

and emit_uncoerced env (e : O.Expr.t) : J.stmt list * J.expr =
  match e.expr with
  | O.Expr.Expr_int n -> ([], J.Literal (J.Int n))
  | O.Expr.Expr_float f -> ([], J.Literal (J.Float f))
  | O.Expr.Expr_string s -> ([], J.Literal (J.String s))
  | O.Expr.Expr_char c -> ([], J.Literal (J.String c))
  | O.Expr.Expr_ident name when is_inline_constructor name ->
      ([], constructor_to_object name [])
  | O.Expr.Expr_ident name -> ([], jid_env env name)
  | O.Expr.Expr_record_empty -> ([], J.Object [])
  | O.Expr.Expr_unit -> ([], J.Literal J.Null)
  | O.Expr.Expr_kernel (Kernel_value kernel) ->
      ([], Of_kernel.value kernel)
  | O.Expr.Expr_kernel (Kernel_unary { kernel; argument }) ->
      let statements, subject = emit_value env argument in
      (statements, Of_kernel.unary_operation kernel subject)
  | O.Expr.Expr_kernel (Kernel_binary { kernel; left; right }) ->
      let left_statements, left = emit_value env left in
      let right_statements, right = emit_value env right in
      ( left_statements @ right_statements,
        Of_kernel.binary_operation kernel left right )
  | O.Expr.Expr_record_extend name -> ([], jid_env env (Data.Name.local name))
  | O.Expr.Expr_record_select name -> ([], jid_env env (Data.Name.local name))
  | O.Expr.Expr_accessor field -> ([], accessor_arrow field)
  | O.Expr.Expr_access { expr; field } ->
      let s, o = emit_value env expr in
      ( s,
        J.Member
          {
            object_ = o;
            property = J.Identifier (Data.Located.unwrap field);
            computed = false;
          } )
  | O.Expr.Expr_binop { name; operands = a, b } ->
      let sa, ea = emit_value env a in
      let sb, eb = emit_value env b in
      let bindings, lowered =
        binary_lowering ~operand:a.O.Expr.typ name ea eb
      in
      (sa @ sb @ bindings, lowered)
  | O.Expr.Expr_constr { name; arguments } ->
      let ss, es = emit_values env arguments in
      (ss, constructor_to_object name es)
  | O.Expr.Expr_record rows ->
      let ss, members =
        fold_emit
          (fun { O.Expr.name; value } ->
            let s, v = emit_value env value in
            (s, J.Field (name, v)))
          rows
      in
      (ss, J.Object members)
  | O.Expr.Expr_list es ->
      let ss, vs = emit_values env es in
      (ss, list_to_cons_cells vs)
  | O.Expr.Expr_cons { head; tail } ->
      let sh, eh = emit_value env head in
      let st, et = emit_value env tail in
      (sh @ st, J.Object [ J.Field ("hd", eh); J.Field ("tl", et) ])
  | O.Expr.Expr_tuple items ->
      let ss, vs = emit_values env items in
      (ss, J.Array vs)
  | O.Expr.Expr_record_update { record; fields } ->
      let sr, er = emit_value env record in
      let ss, members =
        fold_emit
          (fun { O.Expr.name; value } ->
            let s, v = emit_value env value in
            (s, J.Field (name, v)))
          fields
      in
      (sr @ ss, J.Object (J.Spread er :: members))
  | O.Expr.Expr_apply { fn; arg } -> emit_apply env fn arg
  | O.Expr.Expr_lambda { params; body } -> ([], emit_lambda env params body)
  | O.Expr.Expr_if_then_else { if_exp; then_exp; else_exp } ->
      let sc, ec = emit_value env if_exp in
      let st, et = emit_value env then_exp in
      let se, ee = emit_value env else_exp in
      if st = [] && se = [] then
        (sc, J.Conditional { test = ec; consequent = et; alternate = ee })
      else
        let r = fresh_temp () in
        ( sc
          @ [
              J.VarDecl { name = r; init = None };
              J.If
                {
                  test = ec;
                  consequent = st @ [ assign_stmt r et ];
                  alternate = Some (se @ [ assign_stmt r ee ]);
                };
            ],
          J.Identifier r )
  | O.Expr.Expr_let { binding; body } ->

      let env', name =
        bind_one env
          (Data.Name.local (Data.Located.unwrap binding.bind_body.name))
      in
      let sv, ev = emit_value env' binding.bind_body.body in
      let sb, eb = emit_value env' body in
      (sv @ [ J.ConstDecl { name; init = ev } ] @ sb, eb)
  | O.Expr.Expr_pattern { expr; pattern_data_items } ->
      let ss, occ, sbind = emit_scrutinee env expr in
      let r = fresh_temp () in
      let chain = emit_match_assign env r occ pattern_data_items in
      ( ss @ sbind @ [ J.VarDecl { name = r; init = None } ] @ chain,
        J.Identifier r )

and emit_values env (es : O.Expr.t list) : J.stmt list * J.expr list =
  fold_emit (emit_value env) es

and emit_scrutinee env (expr : O.Expr.t) : J.stmt list * J.expr * J.stmt list =
  let s, e = emit_value env expr in
  if needs_temp_var e then
    let t = fresh_temp () in
    (s, J.Identifier t, [ J.ConstDecl { name = t; init = e } ])
  else (s, e, [])

and emit_apply env fn arg =
  if is_record_construction fn then emit_record_apply env fn arg
  else
    let callee, args = collect_args [ arg ] fn in
    let saturated_operator =
      match (callee.expr, args) with
      | O.Expr.Expr_ident op, [ left; right ] ->
          Option.map
            (fun lower -> (lower, left, right))
            (saturated_lowering env op ~operand:left.O.Expr.typ)
      | _ -> None
    in
    match saturated_operator with
    | Some (lower, left, right) ->
        let sa, ea = emit_value env left in
        let sb, eb = emit_value env right in
        let bindings, lowered = lower ea eb in
        (sa @ sb @ bindings, lowered)
    | None -> begin
        match (callee.expr, args) with
        | O.Expr.Expr_kernel (Kernel_value kernel), _ ->
            let arity = Data.Kernel.arity kernel in
            emit_known_call env (Of_kernel.value kernel) ~arity
              ~result_type:
                (O.Type.result_after ~applied:arity callee.O.Expr.typ)
              args
        | O.Expr.Expr_ident name, _ -> begin
            match declared_arity env name with
            | Some n when n >= 1 ->
                emit_known_call env (jid_env env name) ~arity:n
                  ~result_type:
                    (O.Type.result_after ~applied:n callee.O.Expr.typ)
                  args
            | Some _ | None -> emit_generic env callee args
          end
        | _ -> emit_generic env callee args
      end

and emit_known_call env callee ~arity ~result_type args =
  let statements, arguments = emit_values env args in
  applied callee ~arity ~result_type ~statements ~arguments

and applied callee ~arity ~result_type ~statements ~arguments =
  let supplied = List.length arguments in
  if supplied = arity then (statements, J.Call { callee; args = arguments })
  else if supplied < arity then
    (statements, closure_partial callee arguments (arity - supplied))
  else
    let saturating, extra = split_at arity arguments in
    let saturated = J.Call { callee; args = saturating } in
    match arity_of_type result_type with
    | Exactly n when n >= 1 ->
        applied saturated ~arity:n
          ~result_type:(O.Type.result_after ~applied:n result_type)
          ~statements ~arguments:extra
    | Exactly _ | At_least _ -> (statements, curry_call saturated extra)

and emit_generic env callee args =
  let sc, ec = emit_value env callee in
  let ss, es = emit_values env args in
  match arity_of_type callee.O.Expr.typ with
  | Exactly arity when arity >= 1 ->
      let bound, target =
        if List.length es < arity && needs_temp_var ec then
          let t = fresh_temp () in
          ([ J.ConstDecl { name = t; init = ec } ], J.Identifier t)
        else ([], ec)
      in
      let statements, expression =
        applied target ~arity
          ~result_type:(O.Type.result_after ~applied:arity callee.O.Expr.typ)
          ~statements:ss ~arguments:es
      in
      (sc @ bound @ statements, expression)
  | Exactly _ | At_least _ -> (sc @ ss, curry_call ec es)

and emit_record_apply env fn arg =
  let apply_expr =
    { O.Expr.typ = fn.O.Expr.typ; expr = O.Expr.Expr_apply { fn; arg } }
  in
  match extract_record_fields apply_expr with
  | [] ->
      let sf, ef = emit_value env fn in
      let sa, ea = emit_value env arg in
      (sf @ sa, J.Call { callee = ef; args = [ ea ] })
  | fields ->
      let ss, members =
        fold_emit
          (fun (n, v) ->
            let s, e = emit_value env v in
            (s, J.Field (n, e)))
          (List.rev fields)
      in
      (ss, J.Object members)

and emit_lambda env params body =
  let names =
    List.map
      (fun (p : O.Expr.expr_lambda_param) ->
        Data.Name.local (Data.Located.unwrap p.name))
      params
  in
  let env, param_names = bind_params env names in

  arrow_of_body param_names (emit_return env None body)

and self_tail_args env tc (e : O.Expr.t) : O.Expr.t list option =
  match e.expr with
  | O.Expr.Expr_apply { fn; arg } -> (
      let callee, args = collect_args [ arg ] fn in
      match callee.expr with
      | O.Expr.Expr_ident name
        when Data.Name.equal name tc.fn
             && (not (SMap.mem name env))
             && List.length args = List.length tc.params ->
          Some args
      | _ -> None)
  | _ -> None

and loop_step env tc args =
  let ss, es = emit_values env args in
  let temps = List.map (fun _ -> fresh_temp ()) es in
  let bind = List.map2 (fun t v -> J.ConstDecl { name = t; init = v }) temps es in
  let step =
    List.map2 (fun p t -> assign_stmt p (J.Identifier t)) tc.params temps
  in
  ss @ bind @ step @ [ J.Continue ]

and tail_self_call env tc (e : O.Expr.t) : J.stmt list option =
  match tc with
  | Some tc -> (
      match self_tail_args env tc e with
      | Some args ->
          tc.triggered <- true;
          Some (loop_step env tc args)
      | None -> None)
  | None -> None

and emit_return env tc (e : O.Expr.t) : J.stmt list =
  match tail_self_call env tc e with
  | Some stmts -> stmts
  | None -> (
      match e.expr with
      | O.Expr.Expr_let { binding; body } ->
          let env', name =
            bind_one env
              (Data.Name.local (Data.Located.unwrap binding.bind_body.name))
          in
          let sv, ev = emit_value env' binding.bind_body.body in
          sv @ [ J.ConstDecl { name; init = ev } ] @ emit_return env' tc body
      | O.Expr.Expr_if_then_else { if_exp; then_exp; else_exp } ->
          let sc, ec = emit_value env if_exp in
          sc
          @ [
              J.If
                {
                  test = ec;
                  consequent = emit_return env tc then_exp;
                  alternate = Some (emit_return env tc else_exp);
                };
            ]
      | O.Expr.Expr_pattern { expr; pattern_data_items } ->
          let ss, occ, sbind = emit_scrutinee env expr in
          ss @ sbind @ emit_match_return env tc occ pattern_data_items
      | _ ->
          let s, ev = emit_value env e in
          s @ [ J.Return (Some ev) ])

and match_tree clauses =
  let patterns =
    List.map (fun (c : O.Expr.expr_pattern_case) -> c.O.Expr.pattern) clauses
  in
  (After_typed.Exhaustive.build ctor_siblings_of patterns, Array.of_list clauses)

and trivial_action (e : O.Expr.t) =
  match e.O.Expr.expr with
  | O.Expr.Expr_int _ | O.Expr.Expr_float _ | O.Expr.Expr_string _
  | O.Expr.Expr_char _ | O.Expr.Expr_ident _ ->
      true
  | O.Expr.Expr_constr { arguments = []; _ } -> true
  | _ -> false

and shareable (clause_arr : O.Expr.expr_pattern_case array) (tree : DT.t) =
  match tree with
  | DT.Switch _ -> true
  | DT.Leaf { action; _ } -> not (trivial_action clause_arr.(action).expr)
  | DT.Fail -> false

and emit_match_return env tc (occ : J.expr)
    (clauses : O.Expr.expr_pattern_case list) : J.stmt list =
  let tree, clause_arr = match_tree clauses in
  let plan = DS.analyze ~shareable:(shareable clause_arr) tree in
  let clause_expr env action =
    emit_value env clause_arr.(action).O.Expr.expr
  in
  let leaf env action = emit_return env tc clause_arr.(action).O.Expr.expr in
  let sink e = [ J.Return (Some e) ] in
  let tnames = thunk_names plan in
  shared_thunks env occ ~plan ~tnames clause_expr
  @ lower env occ ~terminating:true ~leaf
      ~fail:match_failure
      ~sink ~plan ~tnames tree

and emit_match_assign env (r : string) (occ : J.expr)
    (clauses : O.Expr.expr_pattern_case list) : J.stmt list =
  let tree, clause_arr = match_tree clauses in
  let plan = DS.analyze ~shareable:(shareable clause_arr) tree in
  let clause_expr env action =
    emit_value env clause_arr.(action).O.Expr.expr
  in
  let leaf env action =
    let sa, ea = emit_value env clause_arr.(action).O.Expr.expr in
    sa @ [ assign_stmt r ea ]
  in
  let sink e = [ assign_stmt r e ] in
  let tnames = thunk_names plan in
  shared_thunks env occ ~plan ~tnames clause_expr
  @ lower env occ ~terminating:false ~leaf
      ~fail:match_failure
      ~sink ~plan ~tnames tree

let decl_stmts (decl : O.Declaration.t) : J.stmt list =
  let name = sname decl.name in
  let decl = After_typed.Eta_expand.body_lambda_merged decl in
  match decl.params with
  | [] ->
      let s, e = emit_value SMap.empty decl.body in
      s @ [ J.ConstDecl { name; init = e } ]
  | params ->
      let names =
        List.map
          (fun (p : O.Declaration.param) ->
            Data.Name.local (Data.Located.unwrap p.name))
          params
      in
      let env, param_names = bind_params SMap.empty names in
      let tc =
        {
          fn = Data.Name.local (Data.Located.unwrap decl.name);
          params = param_names;
          triggered = false;
        }
      in
      let body = emit_return env (Some tc) decl.body in

      let body =
        if tc.triggered then
          [ J.While { test = J.Literal (J.Bool true); body } ]
        else body
      in
      [ J.ConstDecl { name; init = arrow_of_body param_names body } ]

let program_of_declarations (decls : O.Declaration.t list) : J.program =
  List.concat_map decl_stmts decls

let is_defined_here (name : Data.Name.t) =
  match name with Data.Name.Local _ -> true | Data.Name.Global _ -> false

let constructor_decls (constructors : (Data.Name.t * int) list) : J.stmt list =
  constructors
  |> List.filter (fun (name, _) ->
         is_defined_here name && not (is_inline_constructor name))
  |> List.map (fun (name, arity) ->
         if arity = 0 then
           J.ConstDecl
             { name = js_of_name name; init = constructor_to_object name [] }
         else
           let params = List.init arity (fun i -> "_" ^ string_of_int i) in
           let args = List.map (fun p -> J.Identifier p) params in
           J.ConstDecl
             {
               name = js_of_name name;
               init =
                 J.Arrow
                   {
                     params;
                     body = J.ArrowExpr (constructor_to_object name args);
                   };
             })

let program_with_helpers ~arities ~constructors ~siblings ~typedecls ~exports
    (decls : O.Declaration.t list) : J.program =
  reset_names ();
  reset_instances ();
  Hashtbl.reset visible_types;
  List.iter
    (fun (decl : O.Typedecl.t) -> Hashtbl.replace visible_types decl.name decl)
    typedecls;
  List.iter (fun (name, arity) -> Hashtbl.replace js_arity name arity) arities;
  List.iter
    (fun (name, arity) ->
      if is_defined_here name then reserve_name (js_of_name name);
      Hashtbl.replace js_arity name arity)
    constructors;
  List.iter
    (fun (decl : O.Declaration.t) ->
      let src = Data.Name.local (Data.Located.unwrap decl.name) in
      reserve_name (js_of_name src);
      Hashtbl.replace js_arity src
        (After_typed.Eta_expand.declaration_arity decl))
    decls;
  List.iter
    (fun (name, sibs) -> Hashtbl.replace ctor_siblings name sibs)
    siblings;
  let exported =
    match exports with
    | [] -> []
    | names -> [ J.Export (List.map js_of_name names) ]
  in
  let body = program_of_declarations decls in
  constructor_decls constructors @ instance_declarations () @ body @ exported

let runtime_module_source () = Runtime.source

let extension = "mjs"

let import_lines imports =
  match imports with
  | [] -> ""
  | modules ->
      To_string.program_to_string
        (List.map
           (fun module_name ->
             J.Import_namespace
               {
                 local = module_ident module_name;
                 from = "./" ^ module_name ^ "." ^ extension;
               })
           modules)

let emit_module ~arities ~constructors ~siblings ~typedecls ~imports ~exports
    (decls : O.Declaration.t list) : string =
  let program =
    program_with_helpers ~arities ~constructors ~siblings ~typedecls ~exports
      decls
  in
  let runtime_import =
    if J.references runtime_module_name program then [ runtime_module_name ]
    else []
  in
  import_lines (runtime_import @ imports)
  ^ To_string.program_to_string program
