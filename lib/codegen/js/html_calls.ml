module Expr = Optimized.Expr

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
