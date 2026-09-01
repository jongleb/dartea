module Expr = Optimized.Expr

type way =
  | Set_attribute [@rename "attribute"]
  | Set_property [@rename "property"]
  | Set_style [@rename "style"]
[@@deriving equal, to_string]

type hole_kind =
  | Text
  | Attribute
  | Slot of { key : string; way : way }
  | Event of { event : string; plain : bool }
  | Children of { keyed : bool }
  | Rows of { keyed : bool }
  | Subtree

let equal_hole_kind one other =
  match (one, other) with
  | Text, Text | Attribute, Attribute | Subtree, Subtree -> true
  | Slot { key; way }, Slot { key = other_key; way = other_way } ->
      String.equal key other_key && equal_way way other_way
  | Event { event; plain }, Event { event = other_event; plain = other_plain } ->
      String.equal event other_event && Bool.equal plain other_plain
  | Children { keyed }, Children { keyed = other_keyed }
  | Rows { keyed }, Rows { keyed = other_keyed } ->
      Bool.equal keyed other_keyed
  | (Text | Attribute | Slot _ | Event _ | Subtree | Children _ | Rows _), _ -> false

type hole = { path : int list; kind : hole_kind; value : Expr.t }
type static = { key : string; value : string; way : way }
type child = Element of t | Static_text of string | Hole of hole_kind

and t = {
  tag : string;
  namespace : string option;
  attributes : static list;
  children : child list;
}

let equal_way one other =
  match (one, other) with
  | Set_attribute, Set_attribute | Set_property, Set_property | Set_style, Set_style -> true
  | Set_attribute, (Set_property | Set_style)
  | Set_property, (Set_attribute | Set_style)
  | Set_style, (Set_attribute | Set_property) ->
      false

let equal_static one other =
  String.equal one.key other.key
  && String.equal one.value other.value
  && equal_way one.way other.way

let rec equal one other =
  String.equal one.tag other.tag
  && Option.equal String.equal one.namespace other.namespace
  && List.equal equal_static one.attributes other.attributes
  && List.equal equal_child one.children other.children

and equal_child one other =
  match (one, other) with
  | Element inner, Element other_inner -> equal inner other_inner
  | Static_text text, Static_text other_text -> String.equal text other_text
  | Hole kind, Hole other_kind -> equal_hole_kind kind other_kind
  | Element _, (Static_text _ | Hole _)
  | Static_text _, (Element _ | Hole _)
  | Hole _, (Element _ | Static_text _) ->
      false

let guarded_key key =
  let lower = String.lowercase_ascii key in
  String.starts_with ~prefix:"on" lower || String.equal lower "formaction"

let static_of name key value =
  match name with
  | Html_calls.Known.Attribute when not (guarded_key key || Html_calls.dangerous_uri value) ->
      Some { key; value; way = Set_attribute }
  | Html_calls.Known.Style -> Some { key; value; way = Set_style }
  | Html_calls.Known.Property when Html_calls.Reflected.holds key && not (Html_calls.dangerous_uri value) ->
      Some { key; value; way = Set_property }
  | Html_calls.Known.Attribute | Html_calls.Known.Property | Html_calls.Known.Node | Html_calls.Known.Keyed_node | Html_calls.Known.Node_ns
  | Html_calls.Known.Keyed_node_ns | Html_calls.Known.Text | Html_calls.Known.On | Html_calls.Known.Normal | Html_calls.Known.Decode_succeed
  | Html_calls.Known.Encode_string | Html_calls.Known.Json_identity | Html_calls.Known.List_map ->
      None

type attribute = Static of static | Dynamic of hole_kind * Expr.t

let unwrapped (e : Expr.t) =
  match e.expr with
  | Expr_apply { fn; arg } when Html_calls.is_string_encoder fn -> arg
  | Expr_apply _ | Expr_ident _ | Expr_kernel _ | Expr_constr _ | Expr_binop _ | Expr_let _
  | Expr_if_then_else _ | Expr_record _ | Expr_record_update _ | Expr_pattern _
  | Expr_accessor _ | Expr_access _ | Expr_record_extend _ | Expr_record_select _
  | Expr_record_empty | Expr_unit | Expr_lambda _ | Expr_char _ | Expr_string _
  | Expr_int _ | Expr_float _ | Expr_list _ | Expr_cons _ | Expr_tuple _ ->
      e

let slot_of name key =
  match name with
  | Html_calls.Known.Attribute when not (guarded_key key) -> Some Set_attribute
  | Html_calls.Known.Style -> Some Set_style
  | Html_calls.Known.Property when Html_calls.Reflected.holds key || Html_calls.Live.holds key -> Some Set_property
  | Html_calls.Known.Attribute | Html_calls.Known.Property | Html_calls.Known.Node | Html_calls.Known.Keyed_node | Html_calls.Known.Node_ns
  | Html_calls.Known.Keyed_node_ns | Html_calls.Known.Text | Html_calls.Known.On | Html_calls.Known.Normal | Html_calls.Known.Decode_succeed
  | Html_calls.Known.Encode_string | Html_calls.Known.Json_identity | Html_calls.Known.List_map ->
      None

let attribute (e : Expr.t) =
  match Html_calls.call e with
  | Some (Html_calls.Known.On, [ { expr = Expr_string event; _ }; given ]) ->
      let plain, value = Html_calls.handler given in
      Dynamic (Event { event; plain }, value)
  | Some (name, [ key; value ]) -> begin
      match (Html_calls.literal_string key, Html_calls.literal_string value) with
      | Some key, Some value ->
          Option.fold ~none:(Dynamic (Attribute, e))
            ~some:(fun static -> Static static)
            (static_of name key value)
      | Some key, None -> begin
          match slot_of name key with
          | Some way -> Dynamic (Slot { key; way }, unwrapped value)
          | None -> Dynamic (Attribute, e)
        end
      | None, _ -> Dynamic (Attribute, e)
    end
  | Some _ | None -> Dynamic (Attribute, e)

let attributes (e : Expr.t) =
  Html_calls.literal_list e |> Option.map (List.map attribute)

let map_call (e : Expr.t) =
  match Html_calls.call e with
  | Some (Html_calls.Known.List_map, [ row; items ]) -> Some (row, items)
  | Some _ | None -> None

let front arguments =
  List.filteri (fun index _ -> index < List.length arguments - 1) arguments

let last arguments = List.nth arguments (List.length arguments - 1)

let row_call ~arity (row : Expr.t) =
  let lift callee arguments =
    match Expr.ident_of callee with
    | Some fn when Option.equal Int.equal (arity fn) (Some (List.length arguments + 1)) ->
        Some (callee, arguments)
    | Some _ | None -> None
  in
  let through_lambda =
    match row.expr with
    | Expr_lambda { params = [ param ]; body } -> begin
        let name = Data.Name.local (Data.Located.unwrap param.name) in
        let callee, arguments = Expr.spine body in
        let captures (argument : Expr.t) =
          Expr.Names.mem name (Expr.free_variables ~bound:Expr.Names.empty argument)
        in
        match arguments with
        | [] -> None
        | _ :: _ -> begin
            match (last arguments).expr with
            | Expr_ident held
              when Data.Name.equal held name && not (List.exists captures (front arguments)) ->
                lift callee (front arguments)
            | _ -> None
          end
      end
    | _ ->
        let callee, arguments = Expr.spine row in
        lift callee arguments
  in
  Option.value through_lambda ~default:(row, [])

let rec element ~path (e : Expr.t) : (t * hole list) option =
  match Html_calls.call e with
  | Some (Html_calls.Known.Node, [ { expr = Expr_string tag; _ }; attribute_list; child_list ]) ->
      shape ~path ~keyed:false ~namespace:None ~tag attribute_list child_list
  | Some (Html_calls.Known.Keyed_node, [ { expr = Expr_string tag; _ }; attribute_list; child_list ]) ->
      shape ~path ~keyed:true ~namespace:None ~tag attribute_list child_list
  | Some
      ( Html_calls.Known.Node_ns,
        [
          { expr = Expr_string namespace; _ };
          { expr = Expr_string tag; _ };
          attribute_list;
          child_list;
        ] ) ->
      shape ~path ~keyed:false ~namespace:(Some namespace) ~tag attribute_list child_list
  | Some
      ( Html_calls.Known.Keyed_node_ns,
        [
          { expr = Expr_string namespace; _ };
          { expr = Expr_string tag; _ };
          attribute_list;
          child_list;
        ] ) ->
      shape ~path ~keyed:true ~namespace:(Some namespace) ~tag attribute_list child_list
  | Some _ | None -> None

and shape ~path ~keyed ~namespace ~tag attribute_list child_list =
  let form_of found =
    let attribute_holes =
      List.filter_map
        (fun found ->
          match found with
          | Dynamic (kind, value) -> Some { path; kind; value }
          | Static _ -> None)
        found
    in
    let static =
      List.filter_map
        (fun found -> match found with Static static -> Some static | Dynamic _ -> None)
        found
    in
    let children, child_holes = children ~path ~keyed child_list in
    ({ tag; namespace; attributes = static; children }, attribute_holes @ child_holes)
  in
  if Html_calls.Guarded_tag.holds tag then None else Option.map form_of (attributes attribute_list)

and children ~path ~keyed (e : Expr.t) : child list * hole list =
  let literal = if keyed then None else Html_calls.literal_list e in
  match literal with
  | None ->
      let kind =
        if Option.is_some (map_call e) then Rows { keyed } else Children { keyed }
      in
      ([], [ { path; kind; value = e } ])
  | Some items ->
      let entries =
        List.mapi (fun index item -> child ~path:(path @ [ index ]) item) items
      in
      (List.map fst entries, List.concat_map snd entries)

and child ~path (e : Expr.t) : child * hole list =
  match Html_calls.call e with
  | Some (Html_calls.Known.Text, [ { expr = Expr_string text; _ } ]) -> (Static_text text, [])
  | Some (Html_calls.Known.Text, [ value ]) -> (Hole Text, [ { path; kind = Text; value } ])
  | Some _ | None -> begin
      match element ~path e with
      | Some (form, holes) -> (Element form, holes)
      | None -> (Hole Subtree, [ { path; kind = Subtree; value = e } ])
    end

let of_expression (e : Expr.t) = element ~path:[] e

module Guard = struct
  let rec chain ~local (e : Expr.t) =
    match e.expr with
    | Expr_ident (Local name) when local (Data.Name.local name) -> Some (name, [])
    | Expr_access { expr; field } ->
        Option.map
          (fun (root, fields) -> (root, Data.Located.unwrap field :: fields))
          (chain ~local expr)
    | Expr_ident _ | Expr_kernel _ | Expr_constr _ | Expr_binop _ | Expr_let _
    | Expr_if_then_else _ | Expr_record _ | Expr_record_update _ | Expr_apply _
    | Expr_pattern _ | Expr_accessor _ | Expr_record_extend _ | Expr_record_select _
    | Expr_record_empty | Expr_unit | Expr_lambda _ | Expr_char _ | Expr_string _
    | Expr_int _ | Expr_float _ | Expr_list _ | Expr_cons _ | Expr_tuple _ ->
        None

  let rec binds (e : Expr.t) =
    match e.expr with
    | Expr_let _ | Expr_lambda _ | Expr_pattern _ -> true
    | Expr_ident _ | Expr_kernel _ | Expr_constr _ | Expr_binop _ | Expr_if_then_else _
    | Expr_record _ | Expr_record_update _ | Expr_apply _ | Expr_accessor _ | Expr_access _
    | Expr_record_extend _ | Expr_record_select _ | Expr_record_empty | Expr_unit
    | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _ | Expr_list _ | Expr_cons _
    | Expr_tuple _ ->
        List.exists binds (Expr.children e)

  let equal_chain (root, fields) (other_root, other_fields) =
    String.equal root other_root && List.equal String.equal fields other_fields

  let inputs ~local (e : Expr.t) =
    let rec collect found (e : Expr.t) =
      match chain ~local e with
      | Some seen ->
          if List.exists (fun (_, known) -> equal_chain known seen) found then found
          else (e, seen) :: found
      | None -> List.fold_left collect found (Expr.children e)
    in
    List.rev_map fst (collect [] e)

  let atomic (e : Expr.t) =
    match e.expr with
    | Expr_ident _ | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _ | Expr_unit
    | Expr_record_empty ->
        true
    | Expr_kernel _ | Expr_constr _ | Expr_binop _ | Expr_let _ | Expr_if_then_else _
    | Expr_record _ | Expr_record_update _ | Expr_apply _ | Expr_pattern _ | Expr_accessor _
    | Expr_access _ | Expr_record_extend _ | Expr_record_select _ | Expr_lambda _
    | Expr_list _ | Expr_cons _ | Expr_tuple _ ->
        false

  type t = Always | Once | On of Expr.t list

  let limit = 4

  let of_expression ~local (e : Expr.t) =
    if binds e || atomic e || Option.is_some (chain ~local e) then Always
    else
      match inputs ~local e with
      | [] -> Once
      | seen when List.length seen <= limit -> On seen
      | _ -> Always

  let slots ~local (holes : hole list) =
    List.fold_left
      (fun total (hole : hole) ->
        match of_expression ~local hole.value with
        | Always -> total
        | Once -> total + 1
        | On seen -> total + List.length seen)
      0 holes
end

module Selector = struct
  type t = { outer : Data.Name.t; item : Data.Name.t; field : string option }

  let equality (e : Expr.t) =
    match e.expr with
    | Expr.Expr_binop { name = Data.Operator.Equal; operands = one, other } -> Some (one, other)
    | _ -> begin
        match Expr.spine e with
        | { expr = Expr.Expr_ident name; _ }, [ one; other ]
          when String.equal (Data.Name.base name) (Data.Operator.lexeme Data.Operator.Equal) ->
            Some (one, other)
        | _, _ -> None
      end

  let named (side : Expr.t) =
    match side.expr with Expr.Expr_ident name -> Some name | _ -> None

  let read (side : Expr.t) =
    match side.expr with
    | Expr.Expr_access { expr = { expr = Expr.Expr_ident name; _ }; field } ->
        Some (name, Some (Data.Located.unwrap field))
    | Expr.Expr_ident name -> Some (name, None)
    | _ -> None

  let of_test (e : Expr.t) =
    match equality e with
    | None -> None
    | Some (one, other) -> begin
        match (named one, read other, read one, named other) with
        | Some outer, Some (item, field), _, _ -> Some { outer; item; field }
        | _, _, Some (item, field), Some outer -> Some { outer; item; field }
        | _, _, _, _ -> None
      end

  let rec inside (e : Expr.t) =
    match of_test e with
    | Some found -> Some found
    | None ->
        List.fold_left
          (fun found part -> match found with Some _ -> found | None -> inside part)
          None (Expr.children e)

  let of_hole (hole : hole) =
    match hole.kind with
    | Text | Attribute | Slot _ -> inside hole.value
    | Event _ | Children _ | Rows _ | Subtree -> None
end

let rec size (form : t) =
  List.fold_left
    (fun total child ->
      match child with
      | Element inner -> total + size inner
      | Static_text _ | Hole _ -> total)
    1 form.children

let rec forms (e : Expr.t) : (t * hole list) list =
  match of_expression e with
  | Some found -> [ found ]
  | None -> List.concat_map forms (Expr.children e)
