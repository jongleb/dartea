module J = Ast

type shape = { form : Blocks.t; holes : (int list * Blocks.hole_kind) list }
type t = { mutable known : (string * shape) list }

let create () = { known = [] }

let equal_hole (path, kind) (other_path, other_kind) =
  List.equal Int.equal path other_path && Blocks.equal_hole_kind kind other_kind

let equal_shape one other =
  Blocks.equal one.form other.form && List.equal equal_hole one.holes other.holes

let of_form (form : Blocks.t) (holes : Blocks.hole list) =
  { form; holes = List.map (fun (hole : Blocks.hole) -> (hole.path, hole.kind)) holes }

let kind_name (kind : Blocks.hole_kind) =
  match kind with
  | Text -> "text"
  | Attribute -> "attribute"
  | Event -> "event"
  | Children -> "children"
  | Subtree -> "subtree"

let way_name (way : Blocks.way) =
  match way with
  | Set_attribute -> "attribute"
  | Set_property -> "property"
  | Set_style -> "style"

let attribute ({ key; value; way } : Blocks.static) =
  J.Object
    [
      J.Field ("key", J.string key);
      J.Field ("value", J.string value);
      J.Field ("way", J.string (way_name way));
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
      ~some:(fun namespace -> [ J.Field ("namespace", J.string namespace) ])
      form.namespace
  in
  J.Object
    ((J.Field ("tag", J.string form.tag) :: namespace)
    @ [
        J.Field ("attributes", J.Array (List.map attribute form.attributes));
        J.Field
          ( "children",
            J.Array
              (List.mapi (fun index -> child shape ~path:(path @ [ index ])) form.children)
          );
      ])

and child shape ~path (child : Blocks.child) =
  match child with
  | Element inner -> node shape ~path inner
  | Static_text text -> J.Object [ J.Field ("text", J.string text) ]
  | Hole _ -> J.Object [ J.Field ("hole", J.int (hole_index shape path)) ]

let expression shape =
  let hole (path, kind) =
    J.Object
      [
        J.Field ("path", J.Array (List.map J.int path));
        J.Field ("kind", J.string (kind_name kind));
      ]
  in
  match node shape ~path:[] shape.form with
  | J.Object fields ->
      J.Object (fields @ [ J.Field ("holes", J.Array (List.map hole shape.holes)) ])
  | other -> other

let name table shape =
  match List.find_opt (fun (_, known) -> equal_shape known shape) table.known with
  | Some (name, _) -> name
  | None ->
      let name = "$$form" ^ string_of_int (List.length table.known) in
      table.known <- (name, shape) :: table.known;
      name

let declarations table =
  List.rev_map
    (fun (name, shape) -> J.ConstDecl { name; init = expression shape })
    table.known
