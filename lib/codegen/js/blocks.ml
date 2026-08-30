module Expr = Optimized.Expr

type hole_kind = Text | Attribute | Event | Children | Subtree
[@@deriving equal]

type hole = { path : int list; kind : hole_kind; value : Expr.t }
type way = Set_attribute | Set_property | Set_style
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

let virtual_dom = "VirtualDom"

let kernel_name (e : Expr.t) =
  match e.expr with
  | Expr_kernel (Kernel_value (Platform { name = Global { module_name; exported_name }; _ }))
  | Expr_ident (Global { module_name; exported_name })
    when String.equal module_name virtual_dom ->
      Some exported_name
  | Expr_kernel _ | Expr_ident _ | Expr_constr _ | Expr_binop _ | Expr_let _
  | Expr_if_then_else _ | Expr_record _ | Expr_record_update _ | Expr_apply _
  | Expr_pattern _ | Expr_accessor _ | Expr_access _ | Expr_record_extend _
  | Expr_record_select _ | Expr_record_empty | Expr_unit | Expr_lambda _
  | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _ | Expr_list _
  | Expr_cons _ | Expr_tuple _ ->
      None

let call (e : Expr.t) =
  let head, arguments = Expr.spine e in
  Option.map (fun name -> (name, arguments)) (kernel_name head)

let json_encode = "Json.Encode"

let is_string_encoder (e : Expr.t) =
  match e.expr with
  | Expr_kernel (Kernel_value (Platform { name = Global { module_name = "Json"; exported_name = "identity" }; _ })) ->
      true
  | Expr_ident (Global { module_name; exported_name = "string" }) ->
      String.equal module_name json_encode
  | Expr_kernel _ | Expr_ident _ | Expr_constr _ | Expr_binop _ | Expr_let _
  | Expr_if_then_else _ | Expr_record _ | Expr_record_update _ | Expr_apply _
  | Expr_pattern _ | Expr_accessor _ | Expr_access _ | Expr_record_extend _
  | Expr_record_select _ | Expr_record_empty | Expr_unit | Expr_lambda _
  | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _ | Expr_list _
  | Expr_cons _ | Expr_tuple _ ->
      false

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

let reflects key =
  match key with
  | "className" | "htmlFor" | "id" | "title" | "lang" | "dir" | "type" | "name"
  | "placeholder" | "href" | "src" | "alt" | "target" | "rel" | "accept"
  | "action" | "method" | "enctype" | "pattern" | "min" | "max" | "step"
  | "cols" | "rows" | "wrap" | "autocomplete" | "download" | "media" | "kind"
  | "srclang" | "poster" | "preload" | "label" | "scope" | "headers" | "cite"
  | "hreflang" ->
      true
  | _ -> false

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

let guarded_tag tag = String.equal tag "script"

let static_of name key value =
  match name with
  | "attribute" when not (guarded_key key || dangerous_uri value) ->
      Some { key; value; way = Set_attribute }
  | "style" -> Some { key; value; way = Set_style }
  | "property" when reflects key && not (dangerous_uri value) ->
      Some { key; value; way = Set_property }
  | _ -> None

type attribute = Static of static | Dynamic of hole_kind

let attribute (e : Expr.t) =
  match call e with
  | Some ("on", [ { expr = Expr_string _; _ }; _ ]) -> Dynamic Event
  | Some (name, [ key; value ]) -> begin
      match (literal_string key, literal_string value) with
      | Some key, Some value ->
          Option.fold ~none:(Dynamic Attribute)
            ~some:(fun static -> Static static)
            (static_of name key value)
      | Some _, None | None, _ -> Dynamic Attribute
    end
  | Some _ | None -> Dynamic Attribute

let attributes (e : Expr.t) =
  literal_list e |> Option.map (List.map attribute)

let rec element ~path (e : Expr.t) : (t * hole list) option =
  match call e with
  | Some ("node", [ { expr = Expr_string tag; _ }; attribute_list; child_list ]) ->
      shape ~path ~namespace:None ~tag attribute_list child_list
  | Some
      ( "nodeNS",
        [
          { expr = Expr_string namespace; _ };
          { expr = Expr_string tag; _ };
          attribute_list;
          child_list;
        ] ) ->
      shape ~path ~namespace:(Some namespace) ~tag attribute_list child_list
  | Some _ | None -> None

and shape ~path ~namespace ~tag attribute_list child_list =
  let form_of found =
    let attribute_holes =
      List.filter_map
        (fun (found, (argument : Expr.t)) ->
          match found with
          | Dynamic kind -> Some { path; kind; value = argument }
          | Static _ -> None)
        (List.combine found (Option.get (literal_list attribute_list)))
    in
    let static =
      List.filter_map
        (fun found -> match found with Static static -> Some static | Dynamic _ -> None)
        found
    in
    let children, child_holes = children ~path child_list in
    ({ tag; namespace; attributes = static; children }, attribute_holes @ child_holes)
  in
  if guarded_tag tag then None else Option.map form_of (attributes attribute_list)

and children ~path (e : Expr.t) : child list * hole list =
  match literal_list e with
  | None -> ([], [ { path; kind = Children; value = e } ])
  | Some items ->
      let entries =
        List.mapi (fun index item -> child ~path:(path @ [ index ]) item) items
      in
      (List.map fst entries, List.concat_map snd entries)

and child ~path (e : Expr.t) : child * hole list =
  match call e with
  | Some ("text", [ { expr = Expr_string text; _ } ]) -> (Static_text text, [])
  | Some ("text", [ value ]) -> (Hole Text, [ { path; kind = Text; value } ])
  | Some _ | None -> begin
      match element ~path e with
      | Some (form, holes) -> (Element form, holes)
      | None -> (Hole Subtree, [ { path; kind = Subtree; value = e } ])
    end

let of_expression (e : Expr.t) = element ~path:[] e

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
