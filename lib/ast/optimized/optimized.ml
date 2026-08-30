module Type = Typed.Type

module Pattern = Typed.Pattern

module Typedecl = struct
  type ctor = { id : Data.Name.t; payload : Type.t list } [@@deriving show]

  type t = {
    name : Data.Name.t;
    params : Type.t Typed.Variable.t list;
    ctors : ctor list;
  }
  [@@deriving show]

  let constructors (decl : t) ~arguments =
    if List.length decl.params <> List.length arguments then None
    else
      let bindings =
        List.combine decl.params arguments |> Type.By_variable.of_list
      in
      Some
        (List.map
           (fun ctor ->
             { ctor with payload = List.map (Type.substitute bindings) ctor.payload })
           decl.ctors)
end

module Expr = struct
  type t = { typ : Type.t; expr : expr } [@@deriving show]

  and expr =
    | Expr_constr of expr_constr
    | Expr_binop of expr_binop
    | Expr_let of expr_let
    | Expr_if_then_else of expr_if_then_else
    | Expr_record of expr_record_row list
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
    | Expr_kernel of expr_kernel
    | Expr_lambda of expr_lambda
    | Expr_char of string
    | Expr_string of string
    | Expr_int of int
    | Expr_float of float
    | Expr_list of t list
    | Expr_cons of expr_cons
    | Expr_tuple of t list
  [@@deriving show]

  and expr_kernel =
    | Kernel_value of Data.Kernel.t
    | Kernel_unary of { kernel : Data.Kernel.unary; argument : t }
    | Kernel_binary of { kernel : Data.Kernel.binary; left : t; right : t }
  [@@deriving show]

  and expr_lambda_param = { name : string Data.Located.t; typ : Type.t }
  [@@deriving show]

  and expr_lambda = { params : expr_lambda_param list; body : t }
  [@@deriving show]

  and expr_cons = { head : t; tail : t } [@@deriving show]

  and expr_constr = { name : Data.Name.t; arguments : t list } [@@deriving show]

  and expr_binop = { name : Data.Operator.t; operands : t * t } [@@deriving show]

  and expr_let_binding_type = { name : string } [@@deriving show]

  and expr_let_binding_body = { name : string Data.Located.t; body : t }
  [@@deriving show]

  and expr_let_binding = { bind_body : expr_let_binding_body } [@@deriving show]

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

  let ident_of e =
    match e.expr with
    | Expr_ident name -> Some name
    | Expr_constr _ | Expr_binop _ | Expr_let _ | Expr_if_then_else _
    | Expr_record _ | Expr_record_update _ | Expr_apply _ | Expr_pattern _
    | Expr_accessor _ | Expr_access _ | Expr_record_extend _
    | Expr_record_select _ | Expr_record_empty | Expr_unit | Expr_kernel _
    | Expr_lambda _ | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _
    | Expr_list _ | Expr_cons _ | Expr_tuple _ ->
        None

  let record_extend_of e =
    match e.expr with
    | Expr_record_extend field -> Some field
    | Expr_constr _ | Expr_binop _ | Expr_let _ | Expr_if_then_else _
    | Expr_record _ | Expr_record_update _ | Expr_apply _ | Expr_ident _
    | Expr_pattern _ | Expr_accessor _ | Expr_access _ | Expr_record_select _
    | Expr_record_empty | Expr_unit | Expr_kernel _ | Expr_lambda _ | Expr_char _
    | Expr_string _ | Expr_int _ | Expr_float _ | Expr_list _ | Expr_cons _
    | Expr_tuple _ ->
        None

  let lambda_of e =
    match e.expr with
    | Expr_lambda lambda -> Some lambda
    | Expr_constr _ | Expr_binop _ | Expr_let _ | Expr_if_then_else _
    | Expr_record _ | Expr_record_update _ | Expr_apply _ | Expr_ident _
    | Expr_pattern _ | Expr_accessor _ | Expr_access _ | Expr_record_extend _
    | Expr_record_select _ | Expr_record_empty | Expr_unit | Expr_kernel _
    | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _ | Expr_list _
    | Expr_cons _ | Expr_tuple _ ->
        None

  let spine expression =
    let rec collect arguments e =
      match e.expr with
      | Expr_apply { fn; arg } -> collect (arg :: arguments) fn
      | Expr_constr _ | Expr_binop _ | Expr_let _ | Expr_if_then_else _
      | Expr_record _ | Expr_record_update _ | Expr_ident _ | Expr_pattern _
      | Expr_accessor _ | Expr_access _ | Expr_record_extend _
      | Expr_record_select _ | Expr_record_empty | Expr_unit | Expr_kernel _
      | Expr_lambda _ | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _
      | Expr_list _ | Expr_cons _ | Expr_tuple _ ->
          (e, arguments)
    in
    collect [] expression

  let map_children (e : t) ~(f : t -> t) : t =
    let expr =
      match e.expr with
      | Expr_constr { name; arguments } ->
          Expr_constr { name; arguments = List.map f arguments }
      | Expr_binop { name; operands = left, right } ->
          let left = f left in
          Expr_binop { name; operands = (left, f right) }
      | Expr_let { binding; body } ->
          let bound_value = f binding.bind_body.body in
          Expr_let
            {
              binding = { bind_body = { binding.bind_body with body = bound_value } };
              body = f body;
            }
      | Expr_if_then_else { if_exp; then_exp; else_exp } ->
          let if_exp = f if_exp in
          let then_exp = f then_exp in
          Expr_if_then_else { if_exp; then_exp; else_exp = f else_exp }
      | Expr_record rows ->
          Expr_record (List.map (fun (row : expr_record_row) -> { row with value = f row.value }) rows)
      | Expr_apply { fn; arg } ->
          let fn = f fn in
          Expr_apply { fn; arg = f arg }
      | Expr_pattern { expr; pattern_data_items } ->
          let scrutinee = f expr in
          Expr_pattern
            {
              expr = scrutinee;
              pattern_data_items =
                List.map
                  (fun (case : expr_pattern_case) -> { case with expr = f case.expr })
                  pattern_data_items;
            }
      | Expr_access { expr; field } -> Expr_access { expr = f expr; field }
      | Expr_lambda { params; body } -> Expr_lambda { params; body = f body }
      | Expr_list items -> Expr_list (List.map f items)
      | Expr_cons { head; tail } -> Expr_cons { head = f head; tail = f tail }
      | Expr_tuple items -> Expr_tuple (List.map f items)
      | Expr_record_update { record; fields } ->
          Expr_record_update
            {
              record = f record;
              fields = List.map (fun (row : expr_record_row) -> { row with value = f row.value }) fields;
            }
      | Expr_kernel (Kernel_unary { kernel; argument }) ->
          Expr_kernel (Kernel_unary { kernel; argument = f argument })
      | Expr_kernel (Kernel_binary { kernel; left; right }) ->
          Expr_kernel (Kernel_binary { kernel; left = f left; right = f right })
      | ( Expr_ident _ | Expr_accessor _ | Expr_record_extend _
        | Expr_record_select _ | Expr_record_empty | Expr_unit
        | Expr_kernel (Kernel_value _)
        | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _ ) as leaf ->
          leaf
    in
    { e with expr }

  let children (e : t) : t list =
    let seen = ref [] in
    let collect child =
      seen := child :: !seen;
      child
    in
    let (_ : t) = map_children e ~f:collect in
    List.rev !seen

  module Names = Data.Name.Set

  let union_map = Pattern.union_map

  let bound_by_lambda (params : expr_lambda_param list) : Names.t =
    Names.of_list
      (List.map
         (fun (p : expr_lambda_param) -> Data.Name.local (Data.Located.unwrap p.name))
         params)

  let whole (_ : t) : t list option = None

  let rec free_variables ?(through = whole) ~(bound : Names.t) (e : t) : Names.t =
    let free_variables = free_variables ~through in
    match e.expr with
    | Expr_ident name ->
        if Names.mem name bound then Names.empty else Names.singleton name
    | Expr_let { binding; body } ->
        let bound_in_body =
          Names.add
            (Data.Name.local (Data.Located.unwrap binding.bind_body.name))
            bound
        in
        Names.union
          (free_variables ~bound binding.bind_body.body)
          (free_variables ~bound:bound_in_body body)
    | Expr_lambda { params; body } ->
        free_variables ~bound:(Names.union bound (bound_by_lambda params)) body
    | Expr_pattern { expr; pattern_data_items } ->
        let free_in_case (case : expr_pattern_case) =
          let bound = Names.union bound (Pattern.bound case.pattern) in
          free_variables ~bound case.expr
        in
        Names.union
          (free_variables ~bound expr)
          (union_map free_in_case pattern_data_items)
    | Expr_binop { name; operands = left, right } ->
        let operands =
          Names.union (free_variables ~bound left) (free_variables ~bound right)
        in
        let operator = Data.Name.local (Data.Operator.lexeme name) in
        if Names.mem operator bound then operands else Names.add operator operands
    | Expr_constr _ | Expr_if_then_else _ | Expr_record _ | Expr_record_update _
    | Expr_apply _ | Expr_accessor _ | Expr_access _ | Expr_record_extend _
    | Expr_record_select _ | Expr_record_empty | Expr_unit | Expr_kernel _
    | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _ | Expr_list _
    | Expr_cons _ | Expr_tuple _ ->
        let below = Option.value ~default:(children e) (through e) in
        union_map (free_variables ~bound) below

  let rec references ?(through = whole) (e : t) : Names.t =
    let below = Option.value ~default:(children e) (through e) in
    let inside = union_map (references ~through) below in
    match e.expr with
    | Expr_ident name -> Names.add name inside
    | Expr_constr { name; _ } -> Names.add name inside
    | Expr_binop { name; _ } ->
        Names.add (Data.Name.local (Data.Operator.lexeme name)) inside
    | Expr_pattern { pattern_data_items; _ } ->
        List.fold_left
          (fun found (case : expr_pattern_case) ->
            Names.union found (Pattern.references case.pattern))
          inside pattern_data_items
    | Expr_let _ | Expr_if_then_else _ | Expr_record _ | Expr_record_update _
    | Expr_apply _ | Expr_accessor _ | Expr_access _ | Expr_record_extend _
    | Expr_record_select _ | Expr_record_empty | Expr_unit | Expr_kernel _
    | Expr_lambda _ | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _
    | Expr_list _ | Expr_cons _ | Expr_tuple _ ->
        inside

  let rec of_typed (e : Typed.Expr.t) : t =
    let typ = e.typ in
    let expr =
      match e.expr with
      | Typed.Expr.Expr_constr { name; arguments } ->
          Expr_constr
            { name; arguments = List.map of_typed arguments }
      | Typed.Expr.Expr_binop { name; operands = e1, e2 } ->
          Expr_binop
            { name; operands = (of_typed e1, of_typed e2) }
      | Typed.Expr.Expr_let { binding; body } ->
          let bind_body =
            {
              name = binding.bind_body.name;
              body = of_typed binding.bind_body.body;
            }
          in
          Expr_let
            { binding = { bind_body }; body = of_typed body }
      | Typed.Expr.Expr_if_then_else { if_exp; then_exp; else_exp } ->
          Expr_if_then_else
            {
              if_exp = of_typed if_exp;
              then_exp = of_typed then_exp;
              else_exp = of_typed else_exp;
            }
      | Typed.Expr.Expr_record rows ->
          Expr_record
            (List.map
               (fun { Typed.Expr.name; value } ->
                 { name; value = of_typed value })
               rows)
      | Typed.Expr.Expr_apply { fn; arg } -> begin
          let head, arguments = Typed.Expr.spine e in
          match (head.Typed.Expr.expr, arguments) with
          | Typed.Expr.Expr_kernel (Language (Unary kernel)), [ argument ] ->
              Expr_kernel
                (Kernel_unary { kernel; argument = of_typed argument })
          | Typed.Expr.Expr_kernel (Language (Binary kernel)), [ left; right ] ->
              Expr_kernel
                (Kernel_binary
                   {
                     kernel;
                     left = of_typed left;
                     right = of_typed right;
                   })
          | Typed.Expr.Expr_kernel
              (Language (Nullary _ | Unary _ | Binary _) | Platform _), _
          | ( (Typed.Expr.Expr_constr _ | Typed.Expr.Expr_binop _ | Typed.Expr.Expr_let _
              | Typed.Expr.Expr_if_then_else _ | Typed.Expr.Expr_record _
              | Typed.Expr.Expr_record_update _ | Typed.Expr.Expr_apply _
              | Typed.Expr.Expr_ident _ | Typed.Expr.Expr_pattern _
              | Typed.Expr.Expr_accessor _ | Typed.Expr.Expr_access _
              | Typed.Expr.Expr_record_extend _ | Typed.Expr.Expr_record_select _
              | Typed.Expr.Expr_record_empty | Typed.Expr.Expr_unit | Typed.Expr.Expr_lambda _
              | Typed.Expr.Expr_char _ | Typed.Expr.Expr_string _ | Typed.Expr.Expr_int _
              | Typed.Expr.Expr_float _ | Typed.Expr.Expr_list _ | Typed.Expr.Expr_cons _
              | Typed.Expr.Expr_tuple _),
              _ ) ->
              Expr_apply
                { fn = of_typed fn; arg = of_typed arg }
        end
      | Typed.Expr.Expr_ident name -> Expr_ident name
      | Typed.Expr.Expr_pattern { expr; pattern_data_items } ->
          Expr_pattern
            {
              expr = of_typed expr;
              pattern_data_items =
                List.map
                  (fun { Typed.Expr.pattern; expr } ->
                    {
                      pattern = pattern;
                      expr = of_typed expr;
                    })
                  pattern_data_items;
            }
      | Typed.Expr.Expr_accessor field -> Expr_accessor field
      | Typed.Expr.Expr_access { expr; field } ->
          Expr_access { expr = of_typed expr; field }
      | Typed.Expr.Expr_record_extend name -> Expr_record_extend name
      | Typed.Expr.Expr_record_select name -> Expr_record_select name
      | Typed.Expr.Expr_record_empty -> Expr_record_empty
      | Typed.Expr.Expr_unit -> Expr_unit
      | Typed.Expr.Expr_kernel kernel -> Expr_kernel (Kernel_value kernel)
      | Typed.Expr.Expr_lambda { params; body } ->
          let params =
            List.map
              (fun (p : Typed.Expr.expr_lambda_param) ->
                { name = p.name; typ = p.typ })
              params
          in
          Expr_lambda { params; body = of_typed body }
      | Typed.Expr.Expr_char c -> Expr_char c
      | Typed.Expr.Expr_string s -> Expr_string s
      | Typed.Expr.Expr_int n -> Expr_int n
      | Typed.Expr.Expr_float f -> Expr_float f
      | Typed.Expr.Expr_list es -> Expr_list (List.map of_typed es)
      | Typed.Expr.Expr_tuple items ->
          Expr_tuple (List.map of_typed items)
      | Typed.Expr.Expr_record_update { record; fields } ->
          Expr_record_update
            {
              record = of_typed record;
              fields =
                List.map
                  (fun (row : Typed.Expr.expr_record_row) ->
                    { name = row.name; value = of_typed row.value })
                  fields;
            }
      | Typed.Expr.Expr_cons { head; tail } ->
          Expr_cons
            { head = of_typed head; tail = of_typed tail }
    in
    { typ; expr }

  let rec merge_nested_lambdas (e : t) : t =
    let e = map_children e ~f:merge_nested_lambdas in
    match e.expr with
    | Expr_lambda { params; body = { expr = Expr_lambda inner; _ } }
      when Names.disjoint
             (bound_by_lambda params)
             (bound_by_lambda inner.params) ->
        let flat = Expr_lambda { params = params @ inner.params; body = inner.body } in
        { e with expr = flat }
    | Expr_constr _ | Expr_binop _ | Expr_let _ | Expr_if_then_else _
    | Expr_record _ | Expr_record_update _ | Expr_apply _ | Expr_ident _
    | Expr_pattern _ | Expr_accessor _ | Expr_access _ | Expr_record_extend _
    | Expr_record_select _ | Expr_record_empty | Expr_unit | Expr_kernel _
    | Expr_lambda _ | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _
    | Expr_list _ | Expr_cons _ | Expr_tuple _ ->
        e
end

module Declaration = struct
  type param = { name : string Data.Located.t; typ : Type.t } [@@deriving show]

  type t = {
    name : string Data.Located.t;
    params : param list;
    body : Expr.t;
    typ : Type.t;
  }
  [@@deriving show]

  let name_of (d : t) = Data.Name.local (Data.Located.unwrap d.name)

  let bound (d : t) : Expr.Names.t =
    Expr.Names.of_list
      (List.map (fun (p : param) -> Data.Name.local (Data.Located.unwrap p.name)) d.params)

  let free ?through (d : t) : Expr.Names.t =
    Expr.free_variables ?through ~bound:(bound d) d.body

  let references_in_all ?through (decls : t list) : Expr.Names.t =
    Expr.union_map (fun (d : t) -> Expr.references ?through d.body) decls

  let of_typed (d : Typed.Declaration.t) : t =
    let params =
      List.map
        (fun (p : Typed.Declaration.param) ->
          { name = p.name; typ = p.typ })
        d.params
    in
    {
      name = d.name;
      params;
      body = Expr.of_typed d.body;
      typ = d.typ;
    }

  let unused_names ~taken =
    let untaken name = not (Expr.Names.mem (Data.Name.local name) taken) in
    Seq.ints 1
    |> Seq.map (fun index -> "eta" ^ string_of_int index)
    |> Seq.filter untaken

  let merge_lambdas (decl : t) : t =
    let absorb ({ params; body } : Expr.expr_lambda) =
      let parameter (p : Expr.expr_lambda_param) =
        { name = p.name; typ = p.typ }
      in
      { decl with params = decl.params @ List.map parameter params; body }
    in
    Expr.lambda_of decl.body |> Option.map absorb |> Option.value ~default:decl

  let arity (decl : t) =
    let decl = merge_lambdas decl in
    let from_kernel =
      match (decl.params, decl.body.expr) with
      | [], Expr.Expr_kernel (Kernel_value kernel) -> Data.Kernel.arity kernel
      | ( [],
          ( Expr_constr _ | Expr_binop _ | Expr_let _ | Expr_if_then_else _
          | Expr_record _ | Expr_record_update _ | Expr_apply _ | Expr_ident _
          | Expr_pattern _ | Expr_accessor _ | Expr_access _
          | Expr_record_extend _ | Expr_record_select _ | Expr_record_empty
          | Expr_unit | Expr_kernel (Kernel_unary _ | Kernel_binary _)
          | Expr_lambda _ | Expr_char _ | Expr_string _ | Expr_int _
          | Expr_float _ | Expr_list _ | Expr_cons _ | Expr_tuple _ ) )
      | _ :: _, _ ->
          0
    in
    List.length decl.params + from_kernel

  let saturate (decl : t) : t =
    let decl = merge_lambdas { decl with body = Expr.merge_nested_lambdas decl.body } in
    let given = arity decl in
    if Type.arrows decl.typ <= given then decl
    else
      let taken =
        Expr.Names.union
          (free decl)
          (bound decl)
      in
      let gap =
        Type.result_after ~applied:given decl.typ
        |> Type.parameters |> List.to_seq
      in
      let extra = Seq.zip (unused_names ~taken) gap |> List.of_seq in
      let parameter (name, typ) =
        { name = Data.Located.at decl.name.region name; typ }
      in
      let applied_to (fn : Expr.t) (name, typ) =
        let argument =
          { Expr.typ; expr = Expr.Expr_ident (Data.Name.local name) }
        in
        {
          Expr.typ = Type.result_after ~applied:1 fn.typ;
          expr = Expr.Expr_apply { fn; arg = argument };
        }
      in
      {
        decl with
        params = decl.params @ List.map parameter extra;
        body = List.fold_left applied_to decl.body extra;
      }

  type error = Bad_recursion of Data.Name.t list

  let show_error (Bad_recursion names) =
    Printf.sprintf "these definitions depend on each other in a cycle: %s"
      (String.concat ", " (List.map Data.Name.to_string names))

  let alive ~roots declarations =
    let uses =
      List.map
        (fun declaration -> (name_of declaration, free declaration))
        declarations
    in
    let rec grown alive_names =
      let next =
        List.fold_left
          (fun found (name, free) ->
            if Expr.Names.mem name found then Expr.Names.union found free else found)
          alive_names uses
      in
      if Expr.Names.equal next alive_names then alive_names else grown next
    in
    let alive_names = grown (Expr.Names.of_list roots) in
    List.filter
      (fun declaration -> Expr.Names.mem (name_of declaration) alive_names)
      declarations

  let in_dependency_order (decls : t list) : (t list, error) result =
    let own = Expr.Names.of_list (List.map name_of decls) in
    let depends_on d = Expr.Names.inter (free d) own in
    let evaluated_before_use (d : t) =
      match d.params with
      | _ :: _ -> false
      | [] -> begin
          match d.body.expr with Expr.Expr_lambda _ -> false | _ -> true
        end
    in
    let rec go ordered = function
      | [] -> Ok (List.rev ordered)
      | Data.Components.Acyclic d :: rest -> go (d :: ordered) rest
      | Data.Components.Cyclic grouped :: rest ->
          if List.exists evaluated_before_use grouped then
            Error (Bad_recursion (List.map name_of grouped))
          else go (List.rev_append grouped ordered) rest
    in
    go [] (Data.Components.of_graph ~name:name_of ~depends_on decls)
end
