module Typedef = struct
  open Data

  module rec Impl : sig
    type t = { parameters : t list; body : Kind.t } [@@deriving show, fields]
  end = struct
    type t = { parameters : t list; body : Kind.t } [@@deriving show, fields]
  end

  and Kind : sig
    type t =
      | Tkind_concrete of string Located.t
      | Tkind_var of string Data.Located.t
      | Tkind_record of Type_record.t
      | Tkind_tuple of Impl.t list
      | Tkind_function of Type_function.t
      | Tkind_unit
    [@@deriving show]
  end = struct
    type t =
      | Tkind_concrete of string Located.t
      | Tkind_var of string Data.Located.t
      | Tkind_record of Type_record.t
      | Tkind_tuple of Impl.t list
      | Tkind_function of Type_function.t
      | Tkind_unit
    [@@deriving show]
  end

  and Type_record_row : sig
    type t = { name : string Data.Located.t; body : Impl.t }
    [@@deriving show, fields]
  end = struct
    type t = { name : string Data.Located.t; body : Impl.t }
    [@@deriving show, fields]
  end

  and Type_record : sig
    type t = {
      values : Type_record_row.t list;
      row_type : string Data.Located.t option;
    }
    [@@deriving show, fields]
  end = struct
    type t = {
      values : Type_record_row.t list;
      row_type : string Data.Located.t option;
    }
    [@@deriving show, fields]
  end

  and Type_function : sig
    type t = { arguments : Impl.t list; result : Impl.t } [@@deriving show, fields]
  end = struct
    type t = { arguments : Impl.t list; result : Impl.t } [@@deriving show, fields]
  end
end

module Exposing = struct
  type privacy = Public of Data.Region.t | Private [@@deriving show]

  type upper = { name : string Data.Located.t; privacy : privacy }
  [@@deriving show]

  type exposed = Lower of string Data.Located.t | Upper of upper
  [@@deriving show]

  type t = Open | Explicit of exposed list [@@deriving show]
end

module Pattern = struct
  type t = kind Data.Located.t [@@deriving show]

  and kind =
    | P_anything
    | P_var of string
    | P_record of string list
    | P_alias of (t * string)
    | P_unit
    | P_tuple of t list
    | P_list of t list
    | P_cons of (t * t)
    | P_chr of string
    | P_str of string
    | P_int of int
    | P_ctor of (string * t list)
  [@@deriving show]
end

module Port_thing = struct
  type t = { name : string Data.Located.t; typedef : Typedef.Impl.t }
  [@@deriving show]
end

module Typealias = struct
  type t = {
    typedef : Typedef.Impl.t;
    params : string Data.Located.t list;
    name : string Data.Located.t;
  }
  [@@deriving show, fields]
end

module Typedecl = struct
  type type_ctor = { id : string Data.Located.t; data : Typedef.Impl.t list }
  [@@deriving show]

  type t = {
    name : string Data.Located.t;
    ctors : type_ctor list;
    params : string list;
  }
  [@@deriving show]
end

module Import_thing = struct
  type t = {
    name : string Data.Located.t;
    alias : string option;
    exposing : Exposing.t;
  }
  [@@deriving show]
end

module Expr = struct
  type t = expr Data.Located.t [@@deriving show]

  and expr =
    | Expr_char of string
    | Expr_string of string
    | Expr_int of int
    | Expr_float of float
    | Expr_unit
    | Expr_list of t list
    | Expr_cons of expr_cons
    | Expr_tuple of t list
    | Expr_binop of expr_binop
    | Expr_let of expr_let
    | Expr_if_then_else of expr_if_then_else
    | Expr_record of expr_record_row list
    | Expr_record_update of expr_record_update
    | Expr_apply of expr_apply
    | Expr_constr_fixed of string
    | Expr_ident of string
    | Expr_qualified of expr_qualified
    | Expr_pattern of expr_pattern
    | Expr_accessor of string Data.Located.t
    | Expr_access of expr_access
    | Expr_unop of expr_unop
    | Expr_lambda of expr_lambda
  [@@deriving show]

  and expr_lambda = { params : string Data.Located.t list; body : t }
  [@@deriving show]

  and expr_cons = { head : t; tail : t } [@@deriving show]

  and expr_unop = { name : string Data.Located.t; operand : t } [@@deriving show]

  and expr_qualified = { qualifier : string; name : string } [@@deriving show]
  and expr_binop = { name : string; operands : t * t } [@@deriving show]

  and expr_let_binding_type = { name : string; content : Typedef.Impl.t }
  [@@deriving show]

  and expr_let_binding_body = { name : string Data.Located.t; body : t }
  [@@deriving show]

  and expr_let_binding = {
    bind_type : expr_let_binding_type option;
    bind_body : expr_let_binding_body;
  }
  [@@deriving show]

  and expr_let = { binding : expr_let_binding; body : t } [@@deriving show]

  and expr_if_then_else = { if_exp : t; then_exp : t; else_exp : t }
  [@@deriving show]

  and expr_record_row = { name : string; value : t } [@@deriving show]

  and expr_record_update = { record : t; fields : expr_record_row list }
  [@@deriving show]

  and expr_apply = { fn : t; arg : t } [@@deriving show]
  and expr_pattern_case = { pattern : Pattern.t; expr : t } [@@deriving show]

  and expr_pattern = { expr : t; pattern_data_items : expr_pattern_case list }
  [@@deriving show]

  and expr_access = { expr : t; field : string Data.Located.t } [@@deriving show]

  let at region expr = Data.Located.at region expr
  let spanning first last = Data.Region.merge first.Data.Located.region last.Data.Located.region

  let make_qualified ~region lexeme =
    match String.rindex_opt lexeme '.' with
    | Some i ->
        at region
          (Expr_qualified
             {
               qualifier = String.sub lexeme 0 i;
               name = String.sub lexeme (i + 1) (String.length lexeme - i - 1);
             })
    | None -> at region (Expr_ident lexeme)

  exception Too_many_tuple_parts of { given : int; region : Data.Region.t }

  let make_expr_tuple ~region parts =
    match parts with
    | [ _; _ ] | [ _; _; _ ] -> at region (Expr_tuple parts)
    | parts -> raise (Too_many_tuple_parts { given = List.length parts; region })

  let make_operator_value ~region name =
    let parameter side = Data.Located.at region side in
    let operand side = at region (Expr_ident side) in
    at region
      (Expr_lambda
         {
           params = [ parameter "$left"; parameter "$right" ];
           body =
             at region
               (Expr_binop
                  { name; operands = (operand "$left", operand "$right") });
         })

  type let_binding =
    | Bind_value of expr_let_binding
    | Bind_pattern of { pattern : Pattern.t; value : t }

  let make_expr_let ~bindings body =
    List.fold_right
      (fun binding body ->
        match binding with
        | Bind_value binding ->
            at
              (Data.Region.merge binding.bind_body.name.Data.Located.region
                 body.Data.Located.region)
              (Expr_let { body; binding })
        | Bind_pattern { pattern; value } ->
            at
              (Data.Region.merge pattern.Data.Located.region
                 body.Data.Located.region)
              (Expr_pattern
                 { expr = value; pattern_data_items = [ { pattern; expr = body } ] }))
      bindings body

  let make_expr_lambda ~region ~params body =
    match params with
    | [] -> body
    | _ -> at region (Expr_lambda { params; body })

  let unwritable_parameter index = "$p" ^ string_of_int index

  let make_parameters ~params body =
    let bind (index, names, wrap) parameter =
      let region = parameter.Data.Located.region in
      let renamed name = Data.Located.at region name in
      let taken names wrap = (index + 1, names, wrap) in
      match parameter.Data.Located.thing with
      | Pattern.P_var written -> taken (renamed written :: names) wrap
      | Pattern.P_anything ->
          taken (renamed (unwritable_parameter index) :: names) wrap
      | destructured ->
          let subject = unwritable_parameter index in
          let wrap body =
            wrap
              (at
                 (Data.Region.merge region body.Data.Located.region)
                 (Expr_pattern
                    {
                      expr = at region (Expr_ident subject);
                      pattern_data_items =
                        [
                          {
                            pattern = Data.Located.at region destructured;
                            expr = body;
                          };
                        ];
                    }))
          in
          taken (renamed subject :: names) wrap
    in
    let start = (0, [], fun body -> body) in
    let _, reversed, wrap = List.fold_left bind start params in
    (List.rev reversed, wrap body)

  let make_expr_apply ~args fn =
    Non_empty_list.reduce
      ~f:(fun fn arg -> at (spanning fn arg) (Expr_apply { fn; arg }))
      Non_empty_list.(fn :: args)
end

module Declaration = struct
  type t = { type_part_data : type_part option; body_part : body_part }
  [@@deriving show]

  and type_part = { name : string Data.Located.t; type_alias : Typedef.Impl.t }
  [@@deriving show]

  and body_part = {
    name : string Data.Located.t;
    expr : Expr.t;
    params : string Data.Located.t list;
  }
  [@@deriving show]
end

module Impl = struct
  type t =
    | Type_alias of Typealias.t
    | Type_dec of Typedecl.t
    | Top_declaration of Declaration.t
    | Import of Import_thing.t
    | ModuleName of string Data.Located.t
    | Export of Exposing.t
    | Port of Port_thing.t
  [@@deriving show]
end

module Module = struct
  module String_map = Map.Make (String)

  type t = {
    imports : Import_thing.t list;
    type_aliases : Typealias.t String_map.t;
    type_declarations : Typedecl.t String_map.t;
    top_declarations : Declaration.t list;
    exports : Exposing.t;
    name : string Data.Located.t option;
  }

  let wiring (typedef : Typedef.Impl.t) =
    let named direction =
      Expr.Expr_qualified
        {
          qualifier = Data.Kernel.Port.home;
          name = Data.Kernel.Port.string_of_direction direction;
        }
    in
    match typedef.body with
    | Typedef.Kind.Tkind_function { arguments = taken :: _; _ } -> (
        match taken.body with
        | Typedef.Kind.Tkind_function _ -> named Data.Kernel.Port.Incoming
        | Typedef.Kind.Tkind_concrete _ | Typedef.Kind.Tkind_var _
        | Typedef.Kind.Tkind_record _ | Typedef.Kind.Tkind_tuple _
        | Typedef.Kind.Tkind_unit ->
            named Data.Kernel.Port.Outgoing)
    | Typedef.Kind.Tkind_function { arguments = []; _ }
    | Typedef.Kind.Tkind_concrete _ | Typedef.Kind.Tkind_var _
    | Typedef.Kind.Tkind_record _ | Typedef.Kind.Tkind_tuple _
    | Typedef.Kind.Tkind_unit ->
        named Data.Kernel.Port.Outgoing

  let wired (port : Port_thing.t) =
    let region = port.name.region in
    let at thing = { Data.Located.thing; region } in
    let given = "given" in
    let call fn arg = at (Expr.Expr_apply { fn; arg }) in
    {
      Declaration.type_part_data =
        Some { Declaration.name = port.name; type_alias = port.typedef };
      body_part =
        {
          Declaration.name = port.name;
          params = [ at given ];
          expr =
            call
              (call
                 (at (wiring port.typedef))
                 (at (Expr.Expr_string (Data.Located.unwrap port.name))))
              (at (Expr.Expr_ident given));
        };
    }

  let of_impl impl_list =
    let collected =
      List.fold_left
        (fun acc next ->
          match next with
          | Impl.Import thing -> { acc with imports = thing :: acc.imports }
          | Impl.Type_alias ta ->
              {
                acc with
                type_aliases = String_map.add ta.name.thing ta acc.type_aliases;
              }
          | Impl.Type_dec td ->
              {
                acc with
                type_declarations =
                  String_map.add td.name.thing td acc.type_declarations;
              }
          | Impl.Top_declaration td ->
              {
                acc with
                top_declarations = td :: acc.top_declarations;
              }
          | Impl.Export e -> { acc with exports = e }
          | Impl.ModuleName name -> { acc with name = Some name }
          | Impl.Port port ->
              {
                acc with
                top_declarations = wired port :: acc.top_declarations;
              })
        {
          imports = [];
          type_aliases = String_map.empty;
          type_declarations = String_map.empty;
          top_declarations = [];
          exports = Exposing.Open;
          name = None;
        }
        impl_list
    in
    {
      collected with
      imports = List.rev collected.imports;
      top_declarations = List.rev collected.top_declarations;
    }
end
