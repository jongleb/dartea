module J = Ast

module Key = struct
  type t =
    | Tag [@rename "tag"]
    | Namespace [@rename "namespace"]
    | Attributes [@rename "attributes"]
    | Children [@rename "children"]
    | Holes [@rename "holes"]
    | Text [@rename "text"]
    | Hole [@rename "hole"]
    | Path [@rename "path"]
    | Kind [@rename "kind"]
    | Keyed [@rename "keyed"]
    | Event [@rename "event"]
    | Plain [@rename "plain"]
    | Key [@rename "key"]
    | Value [@rename "value"]
    | Way [@rename "way"]
    | Form [@rename "form"]
    | Refresh [@rename "refresh"]
    | Find [@rename "find"]
    | At [@rename "at"]
    | Guards [@rename "guards"]
    | Get [@rename "get"]
  [@@deriving to_string]
end

module Kind = struct
  type t =
    | Text [@rename "text"]
    | Attribute [@rename "attribute"]
    | Slot [@rename "slot"]
    | Event [@rename "event"]
    | Children [@rename "children"]
    | Rows [@rename "rows"]
    | Subtree [@rename "subtree"]
  [@@deriving to_string]

  let of_hole (kind : Blocks.hole_kind) =
    match kind with
    | Text -> Text
    | Attribute -> Attribute
    | Slot _ -> Slot
    | Event _ -> Event
    | Children _ -> Children
    | Rows _ -> Rows
    | Subtree -> Subtree
end

let field key value = J.Field (Key.to_string key, value)

type shape = {
  form : Blocks.t;
  holes : (int list * Blocks.hole_kind) list;
  guards : int;
}

type aim = { of_row : Data.Name.t; name : string; entries : J.expr }

type t = {
  mutable known : (string * shape) list;
  mutable refreshers : (string * J.expr) list;
  mutable aims : aim list;
}

let create () = { known = []; refreshers = []; aims = [] }

let aim table ~fn entries =
  let name = Runtime.aim (List.length table.aims) in
  table.aims <- { of_row = fn; name; entries } :: table.aims

let aim_for table fn =
  List.find_opt (fun aim -> Data.Name.equal aim.of_row fn) table.aims
  |> Option.map (fun aim -> aim.name)

let refresher table arrow =
  let name = Runtime.refresher (List.length table.refreshers) in
  table.refreshers <- (name, arrow) :: table.refreshers;
  name

let equal_hole (path, kind) (other_path, other_kind) =
  List.equal Int.equal path other_path && Blocks.equal_hole_kind kind other_kind

let equal_shape one other =
  Blocks.equal one.form other.form && List.equal equal_hole one.holes other.holes

let of_form ~guards (form : Blocks.t) (holes : Blocks.hole list) =
  {
    form;
    holes = List.map (fun (hole : Blocks.hole) -> (hole.path, hole.kind)) holes;
    guards;
  }

let kind_fields (kind : Blocks.hole_kind) =
  match kind with
  | Children { keyed } | Rows { keyed } -> [ field Keyed (J.bool keyed) ]
  | Event { event; plain } -> [ field Event (J.string event); field Plain (J.bool plain) ]
  | Slot { key; way } -> [ field Key (J.string key); field Way (J.string (Blocks.string_of_way way)) ]
  | Text | Attribute | Subtree -> []

let attribute ({ key; value; way } : Blocks.static) =
  J.Object
    [
      field Key (J.string key);
      field Value (J.string value);
      field Way (J.string (Blocks.string_of_way way));
    ]

let hole_index shape path =
  let rec search index = function
    | (held, _) :: rest ->
        if List.equal Int.equal held path then index else search (index + 1) rest
    | [] -> index
  in
  search 0 shape.holes

let rec node shape ~path (form : Blocks.t) =
  let namespace =
    Option.fold ~none:[]
      ~some:(fun namespace -> [ field Namespace (J.string namespace) ])
      form.namespace
  in
  J.Object
    ((field Tag (J.string form.tag) :: namespace)
    @ [
        field Attributes (J.Array (List.map attribute form.attributes));
        field Children
          (J.Array
             (List.mapi (fun index -> child shape ~path:(path @ [ index ])) form.children));
      ])

and child shape ~path (child : Blocks.child) =
  match child with
  | Element inner -> node shape ~path inner
  | Static_text text -> J.Object [ field Text (J.string text) ]
  | Hole _ -> J.Object [ field Hole (J.int (hole_index shape path)) ]

let chain path =
  let root = "$$e" in
  let step held index =
    let rec siblings held left =
      if left = 0 then held else siblings (Dom.member held Dom.Node.Next_sibling) (left - 1)
    in
    siblings (Dom.member held Dom.Node.First_child) index
  in
  J.Arrow
    {
      params = [ root ];
      body = J.ArrowExpr (List.fold_left step (J.Identifier root) path);
    }

let expression shape =
  let hole (path, kind) =
    J.Object
      ([
         field Path (J.Array (List.map J.int path));
         field Find (chain path);
         field Kind (J.string (Kind.to_string (Kind.of_hole kind)));
       ]
      @ kind_fields kind)
  in
  match node shape ~path:[] shape.form with
  | J.Object fields ->
      J.Object
        (fields
        @ [
            field Holes (J.Array (List.map hole shape.holes));
            field Guards (J.int shape.guards);
          ])
  | other -> other

let name table shape =
  match List.find_opt (fun (_, known) -> equal_shape known shape) table.known with
  | Some (name, _) -> name
  | None ->
      let name = Runtime.form (List.length table.known) in
      table.known <- (name, shape) :: table.known;
      name

let declarations table =
  List.rev_map (fun (name, shape) -> J.ConstDecl { name; init = expression shape }) table.known
  @ List.rev_map (fun (name, arrow) -> J.ConstDecl { name; init = arrow }) table.refreshers
  @ List.rev_map (fun aim -> J.ConstDecl { name = aim.name; init = aim.entries }) table.aims
