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

module Known = struct
  type t =
    | Node [@rename "node"]
    | Keyed_node [@rename "keyedNode"]
    | Node_ns [@rename "nodeNS"]
    | Keyed_node_ns [@rename "keyedNodeNS"]
    | Text [@rename "text"]
    | Attribute [@rename "attribute"]
    | Property [@rename "property"]
    | Style [@rename "style"]
    | On [@rename "on"]
    | Normal [@rename "Normal"]
    | Decode_succeed [@rename "succeed"]
    | Encode_string [@rename "string"]
    | Json_identity [@rename "identity"]
    | List_map [@rename "map"]
  [@@deriving enumerate, to_string]

  let home = function
    | Node | Keyed_node | Node_ns | Keyed_node_ns | Text | Attribute | Property
    | Style | On | Normal ->
        Prelude.name Prelude.VirtualDom
    | Decode_succeed -> Prelude.name Prelude.Json_decode
    | Encode_string -> Prelude.name Prelude.Json_encode
    | List_map -> Prelude.name Prelude.List
    | Json_identity -> Platform_kernel.Kernel_module.to_string Json

  let of_name ~module_name ~exported_name =
    List.find_opt
      (fun known ->
        String.equal (home known) module_name
        && String.equal (to_string known) exported_name)
      all
end

module Reflected = struct
  type t =
    | Class_name [@rename "className"]
    | Html_for [@rename "htmlFor"]
    | Id [@rename "id"]
    | Title [@rename "title"]
    | Lang [@rename "lang"]
    | Dir [@rename "dir"]
    | Type [@rename "type"]
    | Name [@rename "name"]
    | Placeholder [@rename "placeholder"]
    | Href [@rename "href"]
    | Src [@rename "src"]
    | Alt [@rename "alt"]
    | Target [@rename "target"]
    | Rel [@rename "rel"]
    | Accept [@rename "accept"]
    | Action [@rename "action"]
    | Method [@rename "method"]
    | Enctype [@rename "enctype"]
    | Pattern [@rename "pattern"]
    | Min [@rename "min"]
    | Max [@rename "max"]
    | Step [@rename "step"]
    | Cols [@rename "cols"]
    | Rows [@rename "rows"]
    | Wrap [@rename "wrap"]
    | Autocomplete [@rename "autocomplete"]
    | Download [@rename "download"]
    | Media [@rename "media"]
    | Kind [@rename "kind"]
    | Srclang [@rename "srclang"]
    | Poster [@rename "poster"]
    | Preload [@rename "preload"]
    | Label [@rename "label"]
    | Scope [@rename "scope"]
    | Headers [@rename "headers"]
    | Cite [@rename "cite"]
    | Hreflang [@rename "hreflang"]
  [@@deriving enumerate, to_string]

  let holds key = List.exists (fun reflected -> String.equal (to_string reflected) key) all
end

module Live = struct
  type t = Value [@rename "value"] | Checked [@rename "checked"] | Selected [@rename "selected"]
  [@@deriving enumerate, to_string]

  let holds key = List.exists (fun live -> String.equal (to_string live) key) all
end

module Guarded_tag = struct
  type t = Script [@rename "script"] [@@deriving enumerate, to_string]

  let holds tag = List.exists (fun guarded -> String.equal (to_string guarded) tag) all
end

let global_name (e : Expr.t) =
  match e.expr with
  | Expr_kernel (Kernel_value (Platform { name = Global { module_name; exported_name }; _ }))
  | Expr_ident (Global { module_name; exported_name }) ->
      Some (module_name, exported_name)
  | Expr_kernel _ | Expr_ident _ | Expr_constr _ | Expr_binop _ | Expr_let _
  | Expr_if_then_else _ | Expr_record _ | Expr_record_update _ | Expr_apply _
  | Expr_pattern _ | Expr_accessor _ | Expr_access _ | Expr_record_extend _
  | Expr_record_select _ | Expr_record_empty | Expr_unit | Expr_lambda _
  | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _ | Expr_list _
  | Expr_cons _ | Expr_tuple _ ->
      None

let known (e : Expr.t) =
  Option.bind (global_name e) (fun (module_name, exported_name) ->
      Known.of_name ~module_name ~exported_name)

let call (e : Expr.t) =
  let head, arguments = Expr.spine e in
  Option.map (fun found -> (found, arguments)) (known head)

let message_of (e : Expr.t) =
  match call e with
  | Some (Known.Decode_succeed, [ message ]) -> Some message
  | Some _ | None -> None

let handler (e : Expr.t) =
  match call e with
  | Some (Known.Normal, [ decoder ]) ->
      Option.fold ~none:(false, e) ~some:(fun message -> (true, message)) (message_of decoder)
  | Some _ | None -> (false, e)

let is_string_encoder (e : Expr.t) =
  match known e with
  | Some (Known.Json_identity | Known.Encode_string) -> true
  | Some _ | None -> false

let literal_string (e : Expr.t) =
  match e.expr with
  | Expr_string text -> Some text
  | Expr_apply { fn; arg = { expr = Expr_string text; _ } } when is_string_encoder fn ->
      Some text
  | Expr_apply _ -> None
  | Expr_ident _ | Expr_kernel _ | Expr_constr _ | Expr_binop _ | Expr_let _
  | Expr_if_then_else _ | Expr_record _ | Expr_record_update _ | Expr_pattern _
  | Expr_accessor _ | Expr_access _ | Expr_record_extend _ | Expr_record_select _
  | Expr_record_empty | Expr_unit | Expr_lambda _ | Expr_char _ | Expr_int _
  | Expr_float _ | Expr_list _ | Expr_cons _ | Expr_tuple _ ->
      None

let rec literal_list (e : Expr.t) =
  match e.expr with
  | Expr_list items -> Some items
  | Expr_cons { head; tail } ->
      Option.map (fun rest -> head :: rest) (literal_list tail)
  | Expr_ident _ | Expr_kernel _ | Expr_constr _ | Expr_binop _ | Expr_let _
  | Expr_if_then_else _ | Expr_record _ | Expr_record_update _ | Expr_apply _
  | Expr_pattern _ | Expr_accessor _ | Expr_access _ | Expr_record_extend _
  | Expr_record_select _ | Expr_record_empty | Expr_unit | Expr_lambda _
  | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _ | Expr_tuple _ ->
      None


let js_whitespace code =
  code = 0x20
  || (code >= 0x09 && code <= 0x0D)
  || code = 0xA0 || code = 0x1680
  || (code >= 0x2000 && code <= 0x200A)
  || code = 0x2028 || code = 0x2029 || code = 0x202F || code = 0x205F
  || code = 0x3000 || code = 0xFEFF

let scalars text =
  let rec from at found =
    if at >= String.length text then List.rev found
    else
      let scalar = String.get_utf_8_uchar text at in
      from (at + Uchar.utf_decode_length scalar) (Uchar.utf_decode_uchar scalar :: found)
  in
  from 0 []

let lowercase scalars =
  let buffer = Buffer.create 16 in
  List.iter (Buffer.add_utf_8_uchar buffer) scalars;
  String.lowercase_ascii (Buffer.contents buffer)

let dangerous_uri value =
  let blank scalar = js_whitespace (Uchar.to_int scalar) in
  let all = scalars value in
  let compact = lowercase (List.filter (fun scalar -> not (blank scalar)) all) in
  let rec trim = function
    | scalar :: rest when blank scalar -> trim rest
    | rest -> rest
  in
  String.starts_with ~prefix:"javascript:" compact
  || String.starts_with ~prefix:"data:text/html" (lowercase (trim all))

let guarded_key key =
  let lower = String.lowercase_ascii key in
  String.starts_with ~prefix:"on" lower || String.equal lower "formaction"

let static_of name key value =
  match name with
  | Known.Attribute when not (guarded_key key || dangerous_uri value) ->
      Some { key; value; way = Set_attribute }
  | Known.Style -> Some { key; value; way = Set_style }
  | Known.Property when Reflected.holds key && not (dangerous_uri value) ->
      Some { key; value; way = Set_property }
  | Known.Attribute | Known.Property | Known.Node | Known.Keyed_node | Known.Node_ns
  | Known.Keyed_node_ns | Known.Text | Known.On | Known.Normal | Known.Decode_succeed
  | Known.Encode_string | Known.Json_identity | Known.List_map ->
      None

type attribute = Static of static | Dynamic of hole_kind * Expr.t

let unwrapped (e : Expr.t) =
  match e.expr with
  | Expr_apply { fn; arg } when is_string_encoder fn -> arg
  | Expr_apply _ | Expr_ident _ | Expr_kernel _ | Expr_constr _ | Expr_binop _ | Expr_let _
  | Expr_if_then_else _ | Expr_record _ | Expr_record_update _ | Expr_pattern _
  | Expr_accessor _ | Expr_access _ | Expr_record_extend _ | Expr_record_select _
  | Expr_record_empty | Expr_unit | Expr_lambda _ | Expr_char _ | Expr_string _
  | Expr_int _ | Expr_float _ | Expr_list _ | Expr_cons _ | Expr_tuple _ ->
      e

let slot_of name key =
  match name with
  | Known.Attribute when not (guarded_key key) -> Some Set_attribute
  | Known.Style -> Some Set_style
  | Known.Property when Reflected.holds key || Live.holds key -> Some Set_property
  | Known.Attribute | Known.Property | Known.Node | Known.Keyed_node | Known.Node_ns
  | Known.Keyed_node_ns | Known.Text | Known.On | Known.Normal | Known.Decode_succeed
  | Known.Encode_string | Known.Json_identity | Known.List_map ->
      None

let attribute (e : Expr.t) =
  match call e with
  | Some (Known.On, [ { expr = Expr_string event; _ }; given ]) ->
      let plain, value = handler given in
      Dynamic (Event { event; plain }, value)
  | Some (name, [ key; value ]) -> begin
      match (literal_string key, literal_string value) with
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
  literal_list e |> Option.map (List.map attribute)

let map_call (e : Expr.t) =
  match call e with
  | Some (Known.List_map, [ row; items ]) -> Some (row, items)
  | Some _ | None -> None

let rec element ~path (e : Expr.t) : (t * hole list) option =
  match call e with
  | Some (Known.Node, [ { expr = Expr_string tag; _ }; attribute_list; child_list ]) ->
      shape ~path ~keyed:false ~namespace:None ~tag attribute_list child_list
  | Some (Known.Keyed_node, [ { expr = Expr_string tag; _ }; attribute_list; child_list ]) ->
      shape ~path ~keyed:true ~namespace:None ~tag attribute_list child_list
  | Some
      ( Known.Node_ns,
        [
          { expr = Expr_string namespace; _ };
          { expr = Expr_string tag; _ };
          attribute_list;
          child_list;
        ] ) ->
      shape ~path ~keyed:false ~namespace:(Some namespace) ~tag attribute_list child_list
  | Some
      ( Known.Keyed_node_ns,
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
  if Guarded_tag.holds tag then None else Option.map form_of (attributes attribute_list)

and children ~path ~keyed (e : Expr.t) : child list * hole list =
  let literal = if keyed then None else literal_list e in
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
  match call e with
  | Some (Known.Text, [ { expr = Expr_string text; _ } ]) -> (Static_text text, [])
  | Some (Known.Text, [ value ]) -> (Hole Text, [ { path; kind = Text; value } ])
  | Some _ | None -> begin
      match element ~path e with
      | Some (form, holes) -> (Element form, holes)
      | None -> (Hole Subtree, [ { path; kind = Subtree; value = e } ])
    end

let of_expression (e : Expr.t) = element ~path:[] e

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

type guard = Always | Once | On of Expr.t

let guard ~local (e : Expr.t) =
  if binds e || atomic e || Option.is_some (chain ~local e) then Always
  else
    match inputs ~local e with
    | [] -> Once
    | [ one ] -> On one
    | _ :: _ :: _ -> Always

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
