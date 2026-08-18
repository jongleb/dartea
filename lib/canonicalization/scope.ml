module Names = Data.Name.Set
module Binders = Map.Make (String)

type binders = {
  names : Data.Region.t Binders.t;
  repeated : Data.Region.t Binders.t;
}

let nothing_bound = { names = Binders.empty; repeated = Binders.empty }

let bound_by_name ~region name =
  { names = Binders.singleton name region; repeated = Binders.empty }

let both_hold left right =
  Binders.merge
    (fun _ one other ->
      match (one, other) with Some _, Some region -> Some region | _ -> None)
    left right

let either_holds left right = Binders.union (fun _ one _ -> Some one) left right

let alongside left right =
  {
    names = either_holds left.names right.names;
    repeated =
      either_holds
        (both_hold left.names right.names)
        (either_holds left.repeated right.repeated);
  }

let union_map f items =
  List.fold_left (fun known item -> alongside known (f item)) nothing_bound items

let rec bound_by_pattern (p : Canonical.Pattern.t) : binders =
  let region = p.region in
  match p.thing with
  | P_var name -> bound_by_name ~region name
  | P_record fields -> union_map (bound_by_name ~region) fields
  | P_tuple items | P_list items -> union_map bound_by_pattern items
  | P_cons (head, tail) ->
      alongside (bound_by_pattern head) (bound_by_pattern tail)
  | P_alias (inner, name) ->
      alongside (bound_by_pattern inner) (bound_by_name ~region name)
  | P_ctor (_, arguments) -> union_map bound_by_pattern arguments
  | P_anything | P_unit | P_chr _ | P_str _ | P_int _ -> nothing_bound

let bound_by_parameters (params : string Data.Located.t list) : binders =
  union_map
    (fun (param : string Data.Located.t) ->
      bound_by_name ~region:param.region param.thing)
    params

module type VISITOR = sig
  type 'a t
  type scope

  val return : 'a -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val binding : scope -> binders -> (scope -> 'a t) -> 'a t
  val reference : scope -> Data.Name.t Data.Located.t -> Canonical.Expr.t t
  val constructor : scope -> Data.Name.t Data.Located.t -> Data.Name.t t
  val type_reference : scope -> Data.Name.t Data.Located.t -> Data.Name.t t
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
    let same kind = Data.Located.at p.region kind in
    match p.thing with
    | P_ctor (name, arguments) ->
        let+ ctor = V.constructor scope (Data.Located.at p.region name)
        and+ arguments = each arguments ~f:(pattern scope) in
        same (P_ctor (ctor, arguments))
    | P_tuple items ->
        let+ items = each items ~f:(pattern scope) in
        same (P_tuple items)
    | P_list items ->
        let+ items = each items ~f:(pattern scope) in
        same (P_list items)
    | P_cons (head, tail) ->
        let+ head = pattern scope head and+ tail = pattern scope tail in
        same (P_cons (head, tail))
    | P_alias (inner, name) ->
        let+ inner = pattern scope inner in
        same (P_alias (inner, name))
    | (P_anything | P_var _ | P_record _ | P_unit | P_chr _ | P_str _ | P_int _)
      as leaf ->
        V.return (same leaf)

  let rec type_expression scope (t : Canonical.Typedef.Impl.t) :
      Canonical.Typedef.Impl.t V.t =
    let open Canonical.Typedef in
    let+ parameters = each t.parameters ~f:(type_expression scope)
    and+ body =
      match t.body with
      | Kind.Tkind_concrete written ->
          let+ name = V.type_reference scope written in
          Kind.Tkind_concrete (Data.Located.at written.region name)
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
      | Kind.Tkind_function { arguments; result } ->
          let+ arguments = each arguments ~f:(type_expression scope)
          and+ result = type_expression scope result in
          Kind.Tkind_function { arguments; result }
      | (Kind.Tkind_var _ | Kind.Tkind_unit) as leaf -> V.return leaf
    in
    { Impl.parameters; body }

  let rec expression scope (e : Canonical.Expr.t) : Canonical.Expr.t V.t =
    let open Canonical.Expr in
    let same expr = Data.Located.at e.region expr in
    match e.thing with
    | Expr_ident name -> V.reference scope (Data.Located.at e.region name)
    | Expr_apply { fn; arg } ->
        let+ fn = expression scope fn and+ arg = expression scope arg in
        same (Expr_apply { fn; arg })
    | Expr_let { binding; body } ->
        let { bind_type; bind_body = { name; body = bound_value } } = binding in
        V.binding scope (bound_by_name ~region:name.region name.thing) (fun inner ->
            let+ bound_value = expression inner bound_value
            and+ body = expression inner body
            and+ bind_type =
              match bind_type with
              | None -> V.return None
              | Some annotation ->
                  let+ content = type_expression scope annotation.content in
                  Some { annotation with content }
            in
            same
              (Expr_let
                 {
                   binding =
                     { bind_type; bind_body = { name; body = bound_value } };
                   body;
                 }))
    | Expr_if_then_else { if_exp; then_exp; else_exp } ->
        let+ if_exp = expression scope if_exp
        and+ then_exp = expression scope then_exp
        and+ else_exp = expression scope else_exp in
        same (Expr_if_then_else { if_exp; then_exp; else_exp })
    | Expr_pattern { expr; pattern_data_items } ->
        let case (item : expr_pattern_case) =
          V.binding scope (bound_by_pattern item.pattern) (fun inner ->
              let+ pattern = pattern scope item.pattern
              and+ expr = expression inner item.expr in
              { pattern; expr })
        in
        let+ expr = expression scope expr
        and+ pattern_data_items = each pattern_data_items ~f:case in
        same (Expr_pattern { expr; pattern_data_items })
    | Expr_lambda { params; body } ->
        V.binding scope (bound_by_parameters params) (fun inner ->
            let+ body = expression inner body in
            same (Expr_lambda { params; body }))
    | Expr_access { expr; field } ->
        let+ expr = expression scope expr in
        same (Expr_access { expr; field })
    | Expr_list items ->
        let+ items = each items ~f:(expression scope) in
        same (Expr_list items)
    | Expr_cons { head; tail } ->
        let+ head = expression scope head and+ tail = expression scope tail in
        same (Expr_cons { head; tail })
    | Expr_tuple items ->
        let+ items = each items ~f:(expression scope) in
        same (Expr_tuple items)
    | Expr_record_update { record; fields } ->
        let row (row : expr_record_row) =
          let+ value = expression scope row.value in
          { row with value }
        in
        let+ record = expression scope record
        and+ fields = each fields ~f:row in
        same (Expr_record_update { record; fields })
    | ( Expr_accessor _ | Expr_record_extend _ | Expr_record_select _
      | Expr_record_empty | Expr_unit | Expr_kernel _ | Expr_char _
      | Expr_string _ | Expr_int _ | Expr_float _ ) as leaf ->
        V.return (same leaf)

  let declaration scope (d : Canonical.Declaration.t) :
      Canonical.Declaration.t V.t =
    V.binding scope (bound_by_parameters d.body_part.params) (fun inner ->
        let+ type_part_data =
          match d.type_part_data with
          | None -> V.return None
          | Some (tp : Canonical.Declaration.type_part) ->
              let+ type_alias = type_expression scope tp.type_alias in
              Some { tp with type_alias }
        and+ expr = expression inner d.body_part.expr in
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
         (fun name _ known -> Names.add (Data.Name.local name) known)
         binders.names scope)

  let mentioned scope (name : Data.Name.t Data.Located.t) =
    if Names.mem name.thing scope then Names.empty
    else Names.singleton name.thing

  let reference = mentioned
  let constructor = mentioned
  let type_reference _ _ = Names.empty
end

module Mentioned = Traversal (Mentions)

let free_in_declaration (d : Canonical.Declaration.t) : Names.t =
  Mentioned.declaration Names.empty d
