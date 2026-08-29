module Typedef = struct
  open Data

  module rec Impl : sig
    type t = { parameters : t list; body : Kind.t } [@@deriving show, fields]

    val of_frontend : Frontend.Typedef.Impl.t -> t
  end = struct
    type t = { parameters : t list; body : Kind.t } [@@deriving show, fields]

    let rec of_frontend (typ : Frontend.Typedef.Impl.t) =
      {
        parameters = List.map of_frontend typ.Frontend.Typedef.Impl.parameters;
        body = Kind.of_frontend typ.body;
      }
  end

  and Kind : sig
    type t =
      | Tkind_concrete of Name.t Located.t
      | Tkind_var of string Data.Located.t
      | Tkind_record of Type_record.t
      | Tkind_tuple of Impl.t list
      | Tkind_function of Type_function.t
      | Tkind_unit
    [@@deriving show]

    val of_frontend : Frontend.Typedef.Kind.t -> t
  end = struct
    type t =
      | Tkind_concrete of Name.t Located.t
      | Tkind_var of string Data.Located.t
      | Tkind_record of Type_record.t
      | Tkind_tuple of Impl.t list
      | Tkind_function of Type_function.t
      | Tkind_unit
    [@@deriving show]

    let of_frontend = function
      | Frontend.Typedef.Kind.Tkind_concrete written ->
          Tkind_concrete (Located.map Name.of_dotted written)
      | Tkind_var name -> Tkind_var name
      | Tkind_record record -> Tkind_record (Type_record.of_frontend record)
      | Tkind_tuple types -> Tkind_tuple (List.map Impl.of_frontend types)
      | Tkind_function func -> Tkind_function (Type_function.of_frontend func)
      | Tkind_unit -> Tkind_unit
  end

  and Type_record_row : sig
    type t = { name : string Data.Located.t; body : Impl.t }
    [@@deriving show, fields]

    val of_frontend : Frontend.Typedef.Type_record_row.t -> t
  end = struct
    type t = { name : string Data.Located.t; body : Impl.t }
    [@@deriving show, fields]

    let of_frontend (row : Frontend.Typedef.Type_record_row.t) =
      {
        name = row.Frontend.Typedef.Type_record_row.name;
        body = Impl.of_frontend row.body;
      }
  end

  and Type_record : sig
    type t = {
      values : Type_record_row.t list;
      row_type : string Data.Located.t option;
    }
    [@@deriving show, fields]

    val of_frontend : Frontend.Typedef.Type_record.t -> t
  end = struct
    type t = {
      values : Type_record_row.t list;
      row_type : string Data.Located.t option;
    }
    [@@deriving show, fields]

    let of_frontend (record : Frontend.Typedef.Type_record.t) =
      {
        values =
          List.map Type_record_row.of_frontend
            record.Frontend.Typedef.Type_record.values;
        row_type = record.row_type;
      }
  end

  and Type_function : sig
    type t = { arguments : Impl.t list; result : Impl.t } [@@deriving show, fields]

    val of_frontend : Frontend.Typedef.Type_function.t -> t
  end = struct
    type t = { arguments : Impl.t list; result : Impl.t } [@@deriving show, fields]

    let of_frontend (func : Frontend.Typedef.Type_function.t) =
      {
        arguments =
          List.map Impl.of_frontend func.Frontend.Typedef.Type_function.arguments;
        result = Impl.of_frontend func.Frontend.Typedef.Type_function.result;
      }
  end
end

module Exposed = struct
  type item = Value of string | Type of { name : string; ctors_exposed : bool }
  [@@deriving show]

  type t = All | Only of item list [@@deriving show]

  let of_frontend (exposing : Frontend.Exposing.t) =
    match exposing with
    | Frontend.Exposing.Open -> All
    | Frontend.Exposing.Explicit items ->
        Only
          (List.map
             (function
               | Frontend.Exposing.Lower name -> Value (Data.Located.unwrap name)
               | Frontend.Exposing.Upper { name; privacy } ->
                   let ctors_exposed =
                     match privacy with
                     | Frontend.Exposing.Public _ -> true
                     | Frontend.Exposing.Private -> false
                   in
                   Type { name = Data.Located.unwrap name; ctors_exposed })
             items)
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
    | P_ctor of (Data.Name.t * t list)
  [@@deriving show]

  let rec of_frontend (pattern : Frontend.Pattern.t) : t =
    let same kind = Data.Located.at pattern.region kind in
    match pattern.thing with
    | Frontend.Pattern.P_anything -> same P_anything
    | P_tuple items -> same (P_tuple (List.map of_frontend items))
    | P_list items -> same (P_list (List.map of_frontend items))
    | P_cons (head, tail) -> same (P_cons (of_frontend head, of_frontend tail))
    | P_chr letter -> same (P_chr letter)
    | P_str text -> same (P_str text)
    | P_int value -> same (P_int value)
    | P_ctor (ctor, arguments) ->
        same (P_ctor (Data.Name.of_dotted ctor, List.map of_frontend arguments))
    | P_alias (inner, name) -> same (P_alias (of_frontend inner, name))
    | P_unit -> same P_unit
    | P_var name -> same (P_var name)
    | P_record fields -> same (P_record fields)
end

module Typealias = struct
  type t = {
    params : string list;
    typedef : Typedef.Impl.t;
    name : Data.Name.t;
    region : Data.Region.t;
  }
  [@@deriving show]

  let of_frontend (frontend : Frontend.Typealias.t) : t =
    {
      name = Data.Name.local frontend.name.thing;
      region = frontend.name.region;
      params = List.map (fun p -> p.Data.Located.thing) frontend.params;
      typedef = Typedef.Impl.of_frontend frontend.typedef;
    }
end

module Typedecl = struct
  type type_ctor = {
    id : Data.Name.t;
    data : Typedef.Impl.t list;
    region : Data.Region.t;
  }
  [@@deriving show]

  type t = {
    name : Data.Name.t;
    ctors : type_ctor list;
    params : string list;
    region : Data.Region.t;
  }
  [@@deriving show]

  let of_frontend (td : Frontend.Typedecl.t) : t =
    let ctors =
      List.map
        (fun (ctor : Frontend.Typedecl.type_ctor) ->
          {
            id = Data.Name.local ctor.id.thing;
            data = List.map Typedef.Impl.of_frontend ctor.data;
            region = ctor.id.region;
          })
        td.ctors
    in
    { name = Data.Name.local td.name.thing; ctors; params = td.params;
      region = td.name.region }

  let arities (declared : t) =
    List.map (fun ctor -> (ctor.id, List.length ctor.data)) declared.ctors

  let siblings (declarations : t list) =
    List.concat_map
      (fun declared ->
        let family = arities declared in
        List.map (fun (ctor, _) -> (ctor, family)) family)
      declarations
    |> List.to_seq |> Data.Name.Map.of_seq
end

module Import = struct
  type t = {
    module_name : string;
    alias : string option;
    exposed : Exposed.t;
    region : Data.Region.t;
  }
  [@@deriving show]

  let of_frontend (import : Frontend.Import_thing.t) =
    {
      module_name = Data.Located.unwrap import.Frontend.Import_thing.name;
      alias = import.alias;
      exposed = Exposed.of_frontend import.exposing;
      region = import.Frontend.Import_thing.name.region;
    }
end

module Expr = struct
  type t = expr Data.Located.t [@@deriving show]

  and expr =
    | Expr_char of string
    | Expr_string of string
    | Expr_int of int
    | Expr_float of float
    | Expr_list of t list
    | Expr_cons of expr_cons
    | Expr_tuple of t list
    | Expr_let of expr_let
    | Expr_if_then_else of expr_if_then_else
    | Expr_record_update of expr_record_update
    | Expr_apply of expr_apply
    | Expr_ident of Data.Name.t
    | Expr_pattern of expr_pattern
    | Expr_accessor of string Data.Located.t
    | Expr_access of expr_access
    | Expr_record_extend of string
    | Expr_record_select of string
    | Expr_record_empty
    | Expr_unit
    | Expr_kernel of Data.Kernel.t
    | Expr_lambda of expr_lambda
  [@@deriving show]

  and expr_lambda = { params : string Data.Located.t list; body : t }
  [@@deriving show]

  and expr_cons = { head : t; tail : t } [@@deriving show]

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

  let unary_function = function "-" -> "negate" | operator -> operator

  let of_frontend (expr : Frontend.Expr.t) : t =
    let rec go (written : Frontend.Expr.t) =
      let same expr = Data.Located.at written.region expr in
      match written.thing with
      | Frontend.Expr.Expr_float value -> same (Expr_float value)
      | Expr_string text -> same (Expr_string text)
      | Expr_int value -> same (Expr_int value)
      | Expr_char letter -> same (Expr_char letter)
      | Expr_unit -> same Expr_unit
      | Expr_accessor field -> same (Expr_accessor field)
      | Expr_access { expr; field } -> same (Expr_access { expr = go expr; field })
      | Expr_list items -> same (Expr_list (List.map go items))
      | Expr_cons { head; tail } ->
          same (Expr_cons { head = go head; tail = go tail })
      | Expr_tuple items -> same (Expr_tuple (List.map go items))
      | Expr_record_update { record; fields } ->
          same
            (Expr_record_update
               {
                 record = go record;
                 fields =
                   List.map
                     (fun (row : Frontend.Expr.expr_record_row) ->
                       { name = row.name; value = go row.value })
                     fields;
               })
      | Expr_binop { name; operands = left, right } ->
          let operator = Data.Located.at written.region (Expr_ident (Data.Name.local name)) in
          same
            (Expr_apply
               {
                 fn =
                   Data.Located.at written.region
                     (Expr_apply { fn = operator; arg = go left });
                 arg = go right;
               })
      | Expr_let
          {
            binding = { bind_body = { name; body = bind_body }; bind_type };
            body;
          } ->
          same
            (Expr_let
               {
                 binding =
                   {
                     bind_type =
                       Option.map
                         (fun (annotation : Frontend.Expr.expr_let_binding_type) ->
                           {
                             name = annotation.name;
                             content = Typedef.Impl.of_frontend annotation.content;
                           })
                         bind_type;
                     bind_body = { name; body = go bind_body };
                   };
                 body = go body;
               })
      | Expr_apply { fn; arg } -> same (Expr_apply { fn = go fn; arg = go arg })
      | Expr_ident name -> same (Expr_ident (Data.Name.local name))
      | Expr_if_then_else { if_exp; then_exp; else_exp } ->
          same
            (Expr_if_then_else
               { if_exp = go if_exp; then_exp = go then_exp; else_exp = go else_exp })
      | Expr_pattern { expr; pattern_data_items } ->
          same
            (Expr_pattern
               {
                 expr = go expr;
                 pattern_data_items =
                   List.map
                     (fun (case : Frontend.Expr.expr_pattern_case) ->
                       {
                         pattern = Pattern.of_frontend case.pattern;
                         expr = go case.expr;
                       })
                     pattern_data_items;
               })
      | Expr_record rows ->
          List.fold_left
            (fun built (row : Frontend.Expr.expr_record_row) ->
              let extend =
                Data.Located.at row.value.region (Expr_record_extend row.name)
              in
              same
                (Expr_apply
                   {
                     fn =
                       Data.Located.at written.region
                         (Expr_apply { fn = extend; arg = go row.value });
                     arg = built;
                   }))
            (same Expr_record_empty) rows
      | Expr_lambda { params; body } ->
          same (Expr_lambda { params; body = go body })
      | Expr_constr_fixed name -> same (Expr_ident (Data.Name.local name))
      | Expr_qualified { qualifier; name } ->
          same
            (Expr_ident (Data.Name.global ~module_name:qualifier ~exported_name:name))
      | Expr_unop { name; operand } ->
          let negate =
            Data.Located.at name.region
              (Expr_ident (Data.Name.local (unary_function name.thing)))
          in
          same (Expr_apply { fn = negate; arg = go operand })
    in
    go expr
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

  let of_frontend_type_part (tp : Frontend.Declaration.type_part) : type_part =
    {
      name = tp.Frontend.Declaration.name;
      type_alias = Typedef.Impl.of_frontend tp.type_alias;
    }

  let of_frontend_body_part (bp : Frontend.Declaration.body_part) : body_part =
    {
      name = bp.Frontend.Declaration.name;
      expr = Expr.of_frontend bp.expr;
      params = bp.params;
    }

  let of_frontend (decl : Frontend.Declaration.t) : t =
    {
      type_part_data =
        Option.map of_frontend_type_part decl.Frontend.Declaration.type_part_data;
      body_part = of_frontend_body_part decl.body_part;
    }
end

module Module = struct
  module String_map = Map.Make (String)

  type t = {
    name : string;
    imports : Import.t list;
    exports : Exposed.t;
    type_aliases : Typealias.t String_map.t;
    type_declarations : Typedecl.t String_map.t;
    top_declarations : Declaration.t list;
  }

  let record_constructor (alias : Frontend.Typealias.t) :
      Frontend.Declaration.t option =
    let open Frontend in
    match alias.typedef with
    | { Typedef.Impl.parameters = []; body = Typedef.Kind.Tkind_record record }
      when Option.is_none record.Typedef.Type_record.row_type ->
        let at value = { alias.name with Data.Located.thing = value } in
        let rows = record.Typedef.Type_record.values in
        let params =
          List.mapi (fun index _ -> at ("$a" ^ string_of_int index)) rows
        in
        let value =
          at
            (Expr.Expr_record
               (List.map2
                  (fun (row : Typedef.Type_record_row.t) param ->
                    {
                      Expr.name = Data.Located.unwrap row.name;
                      value = at (Expr.Expr_ident (Data.Located.unwrap param));
                    })
                  rows params))
        in
        let aliased =
          {
            Typedef.Impl.parameters =
              List.map
                (fun param ->
                  {
                    Typedef.Impl.parameters = [];
                    body = Typedef.Kind.Tkind_var param;
                  })
                alias.params;
            body = Typedef.Kind.Tkind_concrete alias.name;
          }
        in
        let signature =
          match rows with
          | [] -> aliased
          | rows ->
              {
                Typedef.Impl.parameters = [];
                body =
                  Typedef.Kind.Tkind_function
                    {
                      Typedef.Type_function.arguments =
                        List.map
                          (fun (row : Typedef.Type_record_row.t) -> row.body)
                          rows;
                      result = aliased;
                    };
              }
        in
        Some
          {
            Declaration.type_part_data =
              Some { Declaration.name = alias.name; type_alias = signature };
            body_part =
              { Declaration.name = alias.name; expr = value; params };
          }
    | { Typedef.Impl.body = Typedef.Kind.Tkind_record _; _ } -> None
    | {
     Typedef.Impl.body =
       ( Typedef.Kind.Tkind_concrete _ | Typedef.Kind.Tkind_var _
       | Typedef.Kind.Tkind_unit | Typedef.Kind.Tkind_tuple _
       | Typedef.Kind.Tkind_function _ );
     _;
    } ->
        None

  let of_frontend ~fallback_name (frontend_module : Frontend.Module.t) : t =
    let type_aliases =
      Frontend.Module.String_map.fold
        (fun name ta acc -> String_map.add name (Typealias.of_frontend ta) acc)
        frontend_module.type_aliases String_map.empty
    in
    let type_declarations =
      Frontend.Module.String_map.fold
        (fun name td acc -> String_map.add name (Typedecl.of_frontend td) acc)
        frontend_module.type_declarations String_map.empty
    in
    let record_constructors =
      Frontend.Module.String_map.fold
        (fun _ alias collected ->
          match record_constructor alias with
          | None -> collected
          | Some declaration -> Declaration.of_frontend declaration :: collected)
        frontend_module.type_aliases []
    in
    let top_declarations =
      List.map Declaration.of_frontend frontend_module.top_declarations
      @ List.rev record_constructors
    in
    {
      name =
        Option.map Data.Located.unwrap frontend_module.name
        |> Option.value ~default:fallback_name;
      imports = List.map Import.of_frontend frontend_module.imports;
      exports = Exposed.of_frontend frontend_module.exports;
      type_aliases;
      type_declarations;
      top_declarations;
    }

  module String_set = Set.Make (String)

  type error = Import_cycle of string list [@@deriving show]

  let show_error (Import_cycle modules) =
    "these modules import each other in a cycle: " ^ String.concat ", " modules

  type traversal = {
    settled : String_set.t;
    ordered : t list;
  }

  let in_dependency_order (modules : t list) :
      (t list, error) result =
    let known =
      List.fold_left
        (fun acc (m : t) -> String_map.add m.name m acc)
        String_map.empty modules
    in
    let cycle_through ~importing name =
      let rec until_start = function
        | [] -> []
        | current :: outer ->
            if String.equal current name then [ current ]
            else current :: until_start outer
      in
      List.rev (until_start importing)
    in
    let rec visit ~importing traversal name =
      if String_set.mem name traversal.settled then Ok traversal
      else if List.exists (String.equal name) importing then
        Error (Import_cycle (cycle_through ~importing name))
      else
        match String_map.find_opt name known with
        | None -> Ok traversal
        | Some (m : t) ->
            let importing = name :: importing in
            List.fold_left
              (fun traversal (import : Import.t) ->
                Result.bind traversal (fun traversal ->
                    visit ~importing traversal import.module_name))
              (Ok traversal) m.imports
            |> Result.map (fun traversal ->
                   {
                     settled = String_set.add name traversal.settled;
                     ordered = m :: traversal.ordered;
                   })
    in
    List.fold_left
      (fun traversal (m : t) ->
        Result.bind traversal (fun traversal -> visit ~importing:[] traversal m.name))
      (Ok { settled = String_set.empty; ordered = [] })
      modules
    |> Result.map (fun traversal -> List.rev traversal.ordered)
end

module Exports = struct
  module Names = Set.Make (String)
  module By_name = Map.Make (String)

  type exported_type = Alias | Ctors_hidden | Ctors_exposed of Names.t
  type t = { terms : Names.t; types : exported_type By_name.t }

  let declared_names map =
    Module.String_map.fold (fun name _ acc -> Names.add name acc) map Names.empty

  let declared_values (m : Module.t) =
    List.fold_left
      (fun acc (d : Declaration.t) ->
        Names.add (Data.Located.unwrap d.body_part.name) acc)
      Names.empty m.top_declarations

  let ctors_of_type (m : Module.t) type_name =
    Module.String_map.find_opt type_name m.type_declarations
    |> Option.map (fun (td : Typedecl.t) ->
           Names.of_list
             (List.map
                (fun (ctor : Typedecl.type_ctor) -> Data.Name.base ctor.id)
                td.ctors))
    |> Option.value ~default:Names.empty

  let declared_terms (m : Module.t) =
    Module.String_map.fold
      (fun type_name _ acc -> Names.union acc (ctors_of_type m type_name))
      m.type_declarations (declared_values m)

  let declared_types (m : Module.t) =
    Names.union
      (declared_names m.type_declarations)
      (declared_names m.type_aliases)

  let type_names (exports : t) =
    By_name.fold (fun name _ acc -> Names.add name acc) exports.types Names.empty

  let of_module (m : Module.t) : t =
    let exported_type ~ctors_exposed name =
      let ctors = ctors_of_type m name in
      match (Names.is_empty ctors, ctors_exposed) with
      | true, _ -> Alias
      | false, false -> Ctors_hidden
      | false, true -> Ctors_exposed ctors
    in
    match m.exports with
    | Exposed.All ->
        let expose_type ~ctors_exposed name _ types =
          By_name.add name (exported_type ~ctors_exposed name) types
        in
        {
          terms = declared_terms m;
          types =
            Module.String_map.fold
              (expose_type ~ctors_exposed:true)
              m.type_declarations By_name.empty
            |> Module.String_map.fold
                 (expose_type ~ctors_exposed:false)
                 m.type_aliases;
        }
    | Only items ->
        let add acc (item : Exposed.item) =
          match item with
          | Value name -> { acc with terms = Names.add name acc.terms }
          | Type { name; ctors_exposed } ->
              let exported = exported_type ~ctors_exposed name in
              let terms =
                match exported with
                | Ctors_exposed ctors -> Names.union acc.terms ctors
                | Alias when Names.mem name (declared_values m) ->
                    Names.add name acc.terms
                | Alias | Ctors_hidden -> acc.terms
              in
              { terms; types = By_name.add name exported acc.types }
        in
        List.fold_left add { terms = Names.empty; types = By_name.empty } items
end
