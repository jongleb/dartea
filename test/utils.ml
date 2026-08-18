open Ast.Kind.Frontend
open Data

let make_name_no_loc ~name = Located.dummy name
let make_tkind ~name = Typedef.Kind.Tkind_concrete (make_name_no_loc ~name)

let make_typedef ?(parameters = []) ~body () =
  Typedef.Impl.Fields.create ~parameters ~body

let named ~f (thing : 'a Data.Located.t) =
  Data.Located.at (f thing.region) thing.thing

module Typedef_util = struct
  open Typedef

  let rec map_regions ~f (typedef : Impl.t) : Impl.t =
    {
      Impl.parameters = List.map (map_regions ~f) typedef.parameters;
      body = kind ~f typedef.body;
    }

  and kind ~f (written : Kind.t) : Kind.t =
    match written with
    | Kind.Tkind_concrete name -> Kind.Tkind_concrete (named ~f name)
    | Kind.Tkind_var name -> Kind.Tkind_var (named ~f name)
    | Kind.Tkind_unit -> Kind.Tkind_unit
    | Kind.Tkind_tuple items -> Kind.Tkind_tuple (List.map (map_regions ~f) items)
    | Kind.Tkind_function { arguments; result } ->
        Kind.Tkind_function
          {
            arguments = List.map (map_regions ~f) arguments;
            result = map_regions ~f result;
          }
    | Kind.Tkind_record { values; row_type } ->
        Kind.Tkind_record
          {
            values =
              List.map
                (fun (row : Type_record_row.t) ->
                  {
                    Type_record_row.name = named ~f row.name;
                    body = map_regions ~f row.body;
                  })
                values;
            row_type = Option.map (named ~f) row_type;
          }
end

module Typealias_util = struct
  open Typealias

  let map_regions ~f { typedef; params; name } =
    {
      typedef = Typedef_util.map_regions ~f typedef;
      params = List.map (named ~f) params;
      name = named ~f name;
    }
end

module Exposing_util = struct
  open Exposing

  let privacy ~f = function
    | Public region -> Public (f region)
    | Private -> Private

  let upper ~f { name; privacy = written } =
    { name = named ~f name; privacy = privacy ~f written }

  let exposed ~f = function
    | Lower name -> Lower (named ~f name)
    | Upper written -> Upper (upper ~f written)

  let map_regions ~f = function
    | Open -> Open
    | Explicit items -> Explicit (List.map (exposed ~f) items)
end

module Import_thing_util = struct
  open Import_thing

  let map_regions ~f { name; alias; exposing } =
    {
      name = named ~f name;
      alias;
      exposing = Exposing_util.map_regions ~f exposing;
    }
end

module Pattern_util = struct
  open Pattern

  let rec map_regions ~f (pattern : Pattern.t) : Pattern.t =
    let inner = map_regions ~f in
    Data.Located.at (f pattern.region)
      (match pattern.thing with
      | P_anything -> P_anything
      | P_var name -> P_var name
      | P_record fields -> P_record fields
      | P_alias (aliased, name) -> P_alias (inner aliased, name)
      | P_unit -> P_unit
      | P_tuple items -> P_tuple (List.map inner items)
      | P_list items -> P_list (List.map inner items)
      | P_cons (head, tail) -> P_cons (inner head, inner tail)
      | P_chr letter -> P_chr letter
      | P_str text -> P_str text
      | P_int value -> P_int value
      | P_ctor (name, arguments) -> P_ctor (name, List.map inner arguments))
end

module Expr_util = struct
  open Expr

  let rec map_regions ~f (expr : Expr.t) : Expr.t =
    let inner = map_regions ~f in
    let row (written : expr_record_row) =
      { written with value = inner written.value }
    in
    Data.Located.at (f expr.region)
      (match expr.thing with
      | Expr_char letter -> Expr_char letter
      | Expr_string text -> Expr_string text
      | Expr_int value -> Expr_int value
      | Expr_float value -> Expr_float value
      | Expr_unit -> Expr_unit
      | Expr_list items -> Expr_list (List.map inner items)
      | Expr_cons { head; tail } ->
          Expr_cons { head = inner head; tail = inner tail }
      | Expr_tuple items -> Expr_tuple (List.map inner items)
      | Expr_binop { name; operands = left, right } ->
          Expr_binop { name; operands = (inner left, inner right) }
      | Expr_let { binding = { bind_type; bind_body }; body } ->
          Expr_let
            {
              binding =
                {
                  bind_type =
                    Option.map
                      (fun (annotation : expr_let_binding_type) ->
                        {
                          annotation with
                          content =
                            Typedef_util.map_regions ~f annotation.content;
                        })
                      bind_type;
                  bind_body =
                    {
                      name = named ~f bind_body.name;
                      body = inner bind_body.body;
                    };
                };
              body = inner body;
            }
      | Expr_if_then_else { if_exp; then_exp; else_exp } ->
          Expr_if_then_else
            {
              if_exp = inner if_exp;
              then_exp = inner then_exp;
              else_exp = inner else_exp;
            }
      | Expr_record rows -> Expr_record (List.map row rows)
      | Expr_record_update { record; fields } ->
          Expr_record_update
            { record = inner record; fields = List.map row fields }
      | Expr_apply { fn; arg } -> Expr_apply { fn = inner fn; arg = inner arg }
      | Expr_constr_fixed name -> Expr_constr_fixed name
      | Expr_ident name -> Expr_ident name
      | Expr_qualified qualified -> Expr_qualified qualified
      | Expr_pattern { expr = scrutinee; pattern_data_items } ->
          Expr_pattern
            {
              expr = inner scrutinee;
              pattern_data_items =
                List.map
                  (fun (case : expr_pattern_case) ->
                    {
                      pattern = Pattern_util.map_regions ~f case.pattern;
                      expr = inner case.expr;
                    })
                  pattern_data_items;
            }
      | Expr_accessor field -> Expr_accessor (named ~f field)
      | Expr_access { expr = record; field } ->
          Expr_access { expr = inner record; field = named ~f field }
      | Expr_unop { name; operand } ->
          Expr_unop { name = named ~f name; operand = inner operand }
      | Expr_lambda { params; body } ->
          Expr_lambda { params = List.map (named ~f) params; body = inner body })
end

module Top_declaration_util = struct
  open Declaration

  let body_part ~f { name; expr; params } =
    {
      name = named ~f name;
      expr = Expr_util.map_regions ~f expr;
      params = List.map (named ~f) params;
    }

  let type_part ~f { name; type_alias } =
    { name = named ~f name; type_alias = Typedef_util.map_regions ~f type_alias }

  let map_regions ~f { type_part_data; body_part = body } =
    {
      type_part_data = Option.map (type_part ~f) type_part_data;
      body_part = body_part ~f body;
    }
end

module Typedecl_util = struct
  open Typedecl

  let constructor ~f { id; data } =
    { id = named ~f id; data = List.map (Typedef_util.map_regions ~f) data }

  let map_regions ~f { name; params; ctors } =
    { name = named ~f name; params; ctors = List.map (constructor ~f) ctors }
end

let map_regions ~f = function
  | Impl.Type_alias alias -> Impl.Type_alias (Typealias_util.map_regions ~f alias)
  | Impl.Import thing -> Impl.Import (Import_thing_util.map_regions ~f thing)
  | Impl.Type_dec declared -> Impl.Type_dec (Typedecl_util.map_regions ~f declared)
  | Impl.Top_declaration declaration ->
      Impl.Top_declaration (Top_declaration_util.map_regions ~f declaration)
  | Impl.ModuleName name -> Impl.ModuleName (named ~f name)
  | Impl.Export exposed -> Impl.Export (Exposing_util.map_regions ~f exposed)

let dummify_all_locs = map_regions ~f:(fun _ -> Data.Region.nowhere)

let regions_of impl_list =
  let seen = ref [] in
  List.iter
    (fun impl ->
      ignore
        (map_regions
           ~f:(fun region ->
             seen := region :: !seen;
             region)
           impl))
    impl_list;
  List.rev !seen

module Canonical_pattern_util = struct
  open Canonical.Pattern

  let rec dummify (pattern : Canonical.Pattern.t) : Canonical.Pattern.t =
    Data.Located.dummy
      (match pattern.thing with
      | P_anything -> P_anything
      | P_var name -> P_var name
      | P_record fields -> P_record fields
      | P_alias (aliased, name) -> P_alias (dummify aliased, name)
      | P_unit -> P_unit
      | P_tuple items -> P_tuple (List.map dummify items)
      | P_list items -> P_list (List.map dummify items)
      | P_cons (head, tail) -> P_cons (dummify head, dummify tail)
      | P_chr letter -> P_chr letter
      | P_str text -> P_str text
      | P_int value -> P_int value
      | P_ctor (name, arguments) -> P_ctor (name, List.map dummify arguments))
end

module Canonical_expr_util = struct
  open Canonical.Expr

  let anywhere (thing : 'a Data.Located.t) = Data.Located.dummy thing.thing

  let rec dummify (expr : Canonical.Expr.t) : Canonical.Expr.t =
    let row (written : expr_record_row) =
      { written with value = dummify written.value }
    in
    Data.Located.dummy
      (match expr.thing with
      | Expr_char letter -> Expr_char letter
      | Expr_string text -> Expr_string text
      | Expr_int value -> Expr_int value
      | Expr_float value -> Expr_float value
      | Expr_unit -> Expr_unit
      | Expr_kernel primitive -> Expr_kernel primitive
      | Expr_record_empty -> Expr_record_empty
      | Expr_record_extend label -> Expr_record_extend label
      | Expr_record_select label -> Expr_record_select label
      | Expr_ident name -> Expr_ident name
      | Expr_accessor field -> Expr_accessor (anywhere field)
      | Expr_list items -> Expr_list (List.map dummify items)
      | Expr_tuple items -> Expr_tuple (List.map dummify items)
      | Expr_cons { head; tail } ->
          Expr_cons { head = dummify head; tail = dummify tail }
      | Expr_apply { fn; arg } ->
          Expr_apply { fn = dummify fn; arg = dummify arg }
      | Expr_access { expr = record; field } ->
          Expr_access { expr = dummify record; field = anywhere field }
      | Expr_if_then_else { if_exp; then_exp; else_exp } ->
          Expr_if_then_else
            {
              if_exp = dummify if_exp;
              then_exp = dummify then_exp;
              else_exp = dummify else_exp;
            }
      | Expr_record_update { record; fields } ->
          Expr_record_update
            { record = dummify record; fields = List.map row fields }
      | Expr_lambda { params; body } ->
          Expr_lambda
            { params = List.map anywhere params; body = dummify body }
      | Expr_let { binding = { bind_type; bind_body }; body } ->
          Expr_let
            {
              binding =
                {
                  bind_type;
                  bind_body =
                    {
                      name = anywhere bind_body.name;
                      body = dummify bind_body.body;
                    };
                };
              body = dummify body;
            }
      | Expr_pattern { expr = scrutinee; pattern_data_items } ->
          Expr_pattern
            {
              expr = dummify scrutinee;
              pattern_data_items =
                List.map
                  (fun (case : expr_pattern_case) ->
                    {
                      pattern = Canonical_pattern_util.dummify case.pattern;
                      expr = dummify case.expr;
                    })
                  pattern_data_items;
            })
end

module Canonical_typedef_util = struct
  open Canonical.Typedef

  let anywhere (thing : 'a Data.Located.t) = Data.Located.dummy thing.thing

  let rec dummify (typedef : Impl.t) : Impl.t =
    {
      Impl.parameters = List.map dummify typedef.parameters;
      body = dummify_kind typedef.body;
    }

  and dummify_kind (written : Kind.t) : Kind.t =
    match written with
    | Kind.Tkind_concrete name -> Kind.Tkind_concrete (anywhere name)
    | Kind.Tkind_var name -> Kind.Tkind_var (anywhere name)
    | Kind.Tkind_unit -> Kind.Tkind_unit
    | Kind.Tkind_tuple items -> Kind.Tkind_tuple (List.map dummify items)
    | Kind.Tkind_function { arguments; result } ->
        Kind.Tkind_function
          { arguments = List.map dummify arguments; result = dummify result }
    | Kind.Tkind_record { values; row_type } ->
        Kind.Tkind_record
          {
            values =
              List.map
                (fun (row : Type_record_row.t) ->
                  {
                    Type_record_row.name = anywhere row.name;
                    body = dummify row.body;
                  })
                values;
            row_type = Option.map anywhere row_type;
          }
end
