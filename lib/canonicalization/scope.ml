module Names = Data.Name.Set
module Binders = Set.Make (String)

type binders = { names : Binders.t; repeated : Binders.t }

let nothing_bound = { names = Binders.empty; repeated = Binders.empty }
let bound_by_name name =
  { names = Binders.singleton name; repeated = Binders.empty }

let alongside left right =
  {
    names = Binders.union left.names right.names;
    repeated =
      Binders.union
        (Binders.inter left.names right.names)
        (Binders.union left.repeated right.repeated);
  }

let union_map f items =
  List.fold_left (fun known item -> alongside known (f item)) nothing_bound items

let rec bound_by_pattern (p : Canonical.Pattern.t) : binders =
  match p with
  | P_var name -> bound_by_name name
  | P_record fields -> union_map bound_by_name fields
  | P_tuple items | P_list items -> union_map bound_by_pattern items
  | P_cons (head, tail) ->
      alongside (bound_by_pattern head) (bound_by_pattern tail)
  | P_alias (inner, name) ->
      alongside (bound_by_pattern inner) (bound_by_name name)
  | P_ctor (_, arguments) -> union_map bound_by_pattern arguments
  | P_anything | P_unit | P_chr _ | P_str _ | P_int _ -> nothing_bound

let bound_by_parameters (params : string Data.Located.t list) : binders =
  union_map (fun param -> bound_by_name (Data.Located.unwrap param)) params

module type VISITOR = sig
  type 'a t
  type scope

  val return : 'a -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val binding : scope -> binders -> (scope -> 'a t) -> 'a t
  val reference : scope -> Data.Name.t -> Canonical.Expr.t t
  val constructor : scope -> Data.Name.t -> Data.Name.t t
  val type_reference : scope -> Data.Name.t -> Data.Name.t t
end

module Traversal (V : VISITOR) = struct
  let ( let+ ) value f = V.map value ~f
  let ( and+ ) = V.both

  let each items ~f =
    List.fold_right
      (fun item rest ->
        let+ item = f item and+ rest = rest in
        item :: rest)
      items (V.return [])

  let rec pattern scope (p : Canonical.Pattern.t) : Canonical.Pattern.t V.t =
    let open Canonical.Pattern in
    match p with
    | P_ctor (name, arguments) ->
        let+ ctor = V.constructor scope name
        and+ arguments = each arguments ~f:(pattern scope) in
        P_ctor (ctor, arguments)
    | P_tuple items ->
        let+ items = each items ~f:(pattern scope) in
        P_tuple items
    | P_list items ->
        let+ items = each items ~f:(pattern scope) in
        P_list items
    | P_cons (head, tail) ->
        let+ head = pattern scope head and+ tail = pattern scope tail in
        P_cons (head, tail)
    | P_alias (inner, name) ->
        let+ inner = pattern scope inner in
        P_alias (inner, name)
    | (P_anything | P_var _ | P_record _ | P_unit | P_chr _ | P_str _ | P_int _)
      as leaf ->
        V.return leaf

  let rec type_expression scope (t : Canonical.Typedef.Impl.t) :
      Canonical.Typedef.Impl.t V.t =
    let open Canonical.Typedef in
    let+ parameters = each t.parameters ~f:(type_expression scope)
    and+ body =
      match t.body with
      | Kind.Tkind_concrete written ->
          let+ name = V.type_reference scope (Data.Located.unwrap written) in
          Kind.Tkind_concrete { written with Data.Located.thing = name }
      | Kind.Tkind_record { values; row_type } ->
          let row (row : Type_record_row.t) =
            let+ body = type_expression scope row.body in
            { row with Type_record_row.body }
          in
          let+ values = each values ~f:row in
          Kind.Tkind_record { values; row_type }
      | Kind.Tkind_tuple items ->
          let+ items = each items ~f:(type_expression scope) in
          Kind.Tkind_tuple items
      | Kind.Tkind_function { arguments } ->
          let+ arguments = each arguments ~f:(type_expression scope) in
          Kind.Tkind_function { arguments }
      | (Kind.Tkind_var _ | Kind.Tkind_unit) as leaf -> V.return leaf
    in
    { Impl.parameters; body }

  let rec expression scope (e : Canonical.Expr.t) : Canonical.Expr.t V.t =
    let open Canonical.Expr in
    match e with
    | Expr_ident name -> V.reference scope name
    | Expr_apply { fn; arg } ->
        let+ fn = expression scope fn and+ arg = expression scope arg in
        Expr_apply { fn; arg }
    | Expr_let { binding; body } ->
        let { bind_type; bind_body = { name; body = bound_value } } = binding in
        V.binding scope (bound_by_name (Data.Located.unwrap name)) (fun inner ->
            let+ bound_value = expression scope bound_value
            and+ body = expression inner body
            and+ bind_type =
              match bind_type with
              | None -> V.return None
              | Some annotation ->
                  let+ content = type_expression scope annotation.content in
                  Some { annotation with content }
            in
            Expr_let
              {
                binding = { bind_type; bind_body = { name; body = bound_value } };
                body;
              })
    | Expr_if_then_else { if_exp; then_exp; else_exp } ->
        let+ if_exp = expression scope if_exp
        and+ then_exp = expression scope then_exp
        and+ else_exp = expression scope else_exp in
        Expr_if_then_else { if_exp; then_exp; else_exp }
    | Expr_pattern { expr; pattern_data_items } ->
        let case (item : expr_pattern_case) =
          V.binding scope (bound_by_pattern item.pattern) (fun inner ->
              let+ pattern = pattern scope item.pattern
              and+ expr = expression inner item.expr in
              { pattern; expr })
        in
        let+ expr = expression scope expr
        and+ pattern_data_items = each pattern_data_items ~f:case in
        Expr_pattern { expr; pattern_data_items }
    | Expr_lambda { params; body } ->
        V.binding scope (bound_by_parameters params) (fun inner ->
            let+ body = expression inner body in
            Expr_lambda { params; body })
    | Expr_access { expr; field } ->
        let+ expr = expression scope expr in
        Expr_access { expr; field }
    | Expr_list items ->
        let+ items = each items ~f:(expression scope) in
        Expr_list items
    | Expr_cons { head; tail } ->
        let+ head = expression scope head and+ tail = expression scope tail in
        Expr_cons { head; tail }
    | Expr_tuple items ->
        let+ items = each items ~f:(expression scope) in
        Expr_tuple items
    | Expr_record_update { record; fields } ->
        let row (row : expr_record_row) =
          let+ value = expression scope row.value in
          { row with value }
        in
        let+ record = expression scope record
        and+ fields = each fields ~f:row in
        Expr_record_update { record; fields }
    | ( Expr_accessor _ | Expr_record_extend _ | Expr_record_select _
      | Expr_record_empty | Expr_unit | Expr_kernel _ | Expr_char _
      | Expr_string _ | Expr_int _ | Expr_float _ ) as leaf ->
        V.return leaf

  let declaration scope (d : Canonical.Declaration.t) :
      Canonical.Declaration.t V.t =
    V.binding scope (bound_by_parameters d.body_part.params) (fun inner ->
        let+ type_part_data =
          match d.type_part_data with
          | None -> V.return None
          | Some (tp : Canonical.Declaration.type_part) ->
              let+ type_alias = type_expression scope tp.type_alias in
              Some { tp with type_alias }
        and+ expr =
          let+ expr = expression inner (Data.Located.unwrap d.body_part.expr) in
          { d.body_part.expr with Data.Located.thing = expr }
        in
        {
          Canonical.Declaration.type_part_data;
          body_part = { d.body_part with expr };
        })
end

module Mentions = struct
  type 'a t = Names.t
  type scope = Names.t

  let return _ = Names.empty
  let map mentioned ~f:_ = mentioned
  let both = Names.union

  let binding scope binders inner =
    inner
      (Binders.fold
         (fun name known -> Names.add (Data.Name.local name) known)
         binders.names scope)

  let mentioned scope name =
    if Names.mem name scope then Names.empty else Names.singleton name

  let reference = mentioned
  let constructor = mentioned
  let type_reference _ _ = Names.empty
end

module Mentioned = Traversal (Mentions)

let free_in_declaration (d : Canonical.Declaration.t) : Names.t =
  Mentioned.declaration Names.empty d
