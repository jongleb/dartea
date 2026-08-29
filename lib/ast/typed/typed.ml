module Variable : sig
  type 'a t

  type 'a state =
    | Unbound of { level : int; constrained : Data.Constraint.t option }
    | Linked of 'a

  val fresh : Data.Constraint.t option -> 'a t
  val state : 'a t -> 'a state
  val constraint_of : 'a t -> Data.Constraint.t option
  val link : 'a t -> 'a -> unit
  val constrain : 'a t -> Data.Constraint.t -> unit
  val lower_to : from:'a t -> 'a t -> unit
  val enter_level : unit -> unit
  val leave_level : unit -> unit
  val current_level : unit -> int
  val identity : 'a t -> int
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end = struct
  type 'a state =
    | Unbound of { level : int; constrained : Data.Constraint.t option }
    | Linked of 'a

  type 'a t = { identity : int; mutable state : 'a state }

  let born = ref 0
  let depth = ref 0
  let enter_level () = incr depth
  let leave_level () = decr depth
  let current_level () = !depth

  let fresh constrained =
    incr born;
    { identity = !born; state = Unbound { level = !depth; constrained } }

  let state variable = variable.state

  let constraint_of variable =
    match variable.state with
    | Unbound carried -> carried.constrained
    | Linked _ -> None

  let link variable target = variable.state <- Linked target

  let constrain variable carried =
    match variable.state with
    | Unbound held -> variable.state <- Unbound { held with constrained = Some carried }
    | Linked _ -> ()

  let lower_to ~from variable =
    match (from.state, variable.state) with
    | Unbound source, Unbound held when held.level > source.level ->
        variable.state <- Unbound { held with level = source.level }
    | (Unbound _ | Linked _), (Unbound _ | Linked _) -> ()

  let identity variable = variable.identity
  let compare _ left right = Stdlib.Int.compare left.identity right.identity
  let equal _ left right = Stdlib.Int.equal left.identity right.identity

  let pp inner formatter variable =
    match variable.state with
    | Unbound { level; constrained = None } ->
        Format.fprintf formatter "'%d@%d" variable.identity level
    | Unbound { level; constrained = Some carried } ->
        Format.fprintf formatter "'%d@%d:%s" variable.identity level
          (Data.Constraint.name carried)
    | Linked target -> inner formatter target
end

module Type = struct
  open Ppx_compare_lib.Builtin

  type t =
    | TVar of t Variable.t
    | TInt
    | TFloat
    | TChar
    | TBool
    | TStr
    | TUnit
    | TFun of t * t
    | TTup of t list
    | TCustom of Data.Name.t * t list
    | TRecord of t
    | TRowExtend of string * t * t
    | TRowEmpty
  [@@deriving show, compare, equal]

  type scheme = Scheme of t Variable.t list * t
  [@@deriving show, compare, equal]

  module Identity = struct
    type nonrec t = t Variable.t

    let compare left right =
      Stdlib.Int.compare (Variable.identity left) (Variable.identity right)
  end

  module By_variable = Stdlib.Map.Make (Identity)
  module Variables = Stdlib.Set.Make (Identity)

  let fresh_variable constrained = TVar (Variable.fresh constrained)
  let list_name = Data.Name.local "List"
  let list_of element = TCustom (list_name, [ element ])

  let rec head ty =
    match ty with
    | TVar variable -> begin
        match Variable.state variable with
        | Variable.Linked target -> head target
        | Variable.Unbound _ -> ty
      end
    | TInt | TFloat | TChar | TBool | TStr | TUnit | TFun _ | TTup _ | TCustom _
    | TRecord _ | TRowExtend _ | TRowEmpty ->
        ty

  let map_children f ty =
    match ty with
    | TFun (parameter, result) -> TFun (f parameter, f result)
    | TTup items -> TTup (List.map f items)
    | TCustom (name, arguments) -> TCustom (name, List.map f arguments)
    | TRecord row -> TRecord (f row)
    | TRowExtend (label, field, rest) -> TRowExtend (label, f field, f rest)
    | (TVar _ | TInt | TFloat | TChar | TBool | TStr | TUnit | TRowEmpty) as leaf
      ->
        leaf

  let rec zonk ty = map_children zonk (head ty)

  let zonk_scheme (Scheme (quantified, body)) = Scheme (quantified, zonk body)

  let rec fold_variables f collected ty =
    let across collected types = List.fold_left (fold_variables f) collected types in
    match head ty with
    | TVar variable -> f collected variable
    | TInt | TFloat | TChar | TBool | TStr | TUnit | TRowEmpty -> collected
    | TFun (parameter, result) -> across collected [ parameter; result ]
    | TTup items | TCustom (_, items) -> across collected items
    | TRecord row -> fold_variables f collected row
    | TRowExtend (_, field, rest) -> across collected [ field; rest ]

  let iter_variables f ty = fold_variables (fun () variable -> f variable) () ty

  let rec arrows ty =
    match head ty with
    | TFun (_, result) -> 1 + arrows result
    | TVar _ | TInt | TFloat | TChar | TBool | TStr | TUnit | TTup _ | TCustom _
    | TRecord _ | TRowExtend _ | TRowEmpty ->
        0

  let rec result_after ~applied t =
    if applied <= 0 then t
    else
      match head t with
      | TFun (_, result) -> result_after ~applied:(applied - 1) result
      | TVar _ | TInt | TFloat | TChar | TBool | TStr | TUnit | TTup _
      | TCustom _ | TRecord _ | TRowExtend _ | TRowEmpty ->
          t

  let rec parameters ty =
    match head ty with
    | TFun (parameter, result) -> parameter :: parameters result
    | TVar _ | TInt | TFloat | TChar | TBool | TStr | TUnit | TTup _ | TCustom _
    | TRecord _ | TRowExtend _ | TRowEmpty ->
        []

  let function_of parameters ~result =
    List.fold_right (fun parameter tail -> TFun (parameter, tail)) parameters
      result

  let rec substitute bindings t =
    match t with
    | TVar variable -> Option.value ~default:t (By_variable.find_opt variable bindings)
    | TInt | TFloat | TChar | TBool | TStr | TUnit | TRowEmpty | TFun _ | TTup _
    | TCustom _ | TRecord _ | TRowExtend _ ->
        map_children (substitute bindings) t

  type arity = Exactly of int | At_least of int

  let arity typ =
    let rec through arrows = function
      | TFun (_, result) -> through (arrows + 1) result
      | TVar _ -> At_least arrows
      | TInt | TFloat | TChar | TBool | TStr | TUnit | TTup _ | TCustom _
      | TRecord _ | TRowExtend _ | TRowEmpty ->
          Exactly arrows
    in
    through 0 typ
end

module Pattern = struct
  open Ppx_compare_lib.Builtin

  type t = { typ : Type.t; pattern : kind }
  [@@deriving show, compare, equal]

  and kind =
    | P_T_anything
    | P_T_var of string
    | P_T_record of string list
    | P_T_alias of (t * string)
    | P_T_unit
    | P_T_tuple of t list
    | P_T_list of t list
    | P_T_cons of (t * t)
    | P_T_chr of string
    | P_T_str of string
    | P_T_int of int
    | P_T_ctor of (Data.Name.t * t list)
  [@@deriving show, compare, equal]

  let rec map_types on_type p =
    let inner = map_types on_type in
    let pattern =
      match p.pattern with
      | P_T_tuple items -> P_T_tuple (List.map inner items)
      | P_T_list items -> P_T_list (List.map inner items)
      | P_T_cons (head, tail) -> P_T_cons (inner head, inner tail)
      | P_T_ctor (name, arguments) -> P_T_ctor (name, List.map inner arguments)
      | P_T_alias (aliased, name) -> P_T_alias (inner aliased, name)
      | ( P_T_anything | P_T_var _ | P_T_record _ | P_T_unit | P_T_chr _
        | P_T_str _ | P_T_int _ ) as leaf ->
          leaf
    in
    { typ = on_type p.typ; pattern }

  let substitute bindings = map_types (Type.substitute bindings)
  let zonk = map_types Type.zonk

  module Names = Data.Name.Set

  let union_map f items =
    List.fold_left (fun found item -> Names.union found (f item)) Names.empty items

  let rec bound p =
    match p.pattern with
    | P_T_var name -> Names.singleton (Data.Name.local name)
    | P_T_record fields -> Names.of_list (List.map Data.Name.local fields)
    | P_T_tuple items | P_T_list items -> union_map bound items
    | P_T_alias (inner, name) -> Names.add (Data.Name.local name) (bound inner)
    | P_T_cons (head, tail) -> Names.union (bound head) (bound tail)
    | P_T_ctor (_, arguments) -> union_map bound arguments
    | P_T_anything | P_T_unit | P_T_chr _ | P_T_str _ | P_T_int _ -> Names.empty

  let rec references p =
    match p.pattern with
    | P_T_ctor (name, arguments) -> Names.add name (union_map references arguments)
    | P_T_tuple items | P_T_list items -> union_map references items
    | P_T_alias (inner, _) -> references inner
    | P_T_cons (head, tail) -> Names.union (references head) (references tail)
    | P_T_var _ | P_T_record _ | P_T_anything | P_T_unit | P_T_chr _ | P_T_str _
    | P_T_int _ ->
        Names.empty
end

module Expr = struct
  type t = { typ : Type.t; expr : expr; region : Data.Region.t }
  [@@deriving show]

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
    | Expr_kernel of Data.Kernel.t
    | Expr_lambda of expr_lambda
    | Expr_char of string
    | Expr_string of string
    | Expr_int of int
    | Expr_float of float
    | Expr_list of t list
    | Expr_cons of expr_cons
    | Expr_tuple of t list
  [@@deriving show]

  and expr_lambda_param = { name : string Data.Located.t; typ : Type.t }
  [@@deriving show]

  and expr_lambda = { params : expr_lambda_param list; body : t }
  [@@deriving show]

  and expr_cons = { head : t; tail : t } [@@deriving show]

  and expr_constr = { name : Data.Name.t; arguments : t list } [@@deriving show]

  and expr_binop = { name : Data.Operator.t; operands : t * t } [@@deriving show]

  and expr_let_binding_type = { name : string }
  [@@deriving show]

  and expr_let_binding_body = { name : string Data.Located.t; body : t }
  [@@deriving show]

  and expr_let_binding = {
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
  and expr_pattern_case = {
    pattern : Pattern.t;
    expr : t;
    pattern_region : Data.Region.t;
  }
  [@@deriving show]

  and expr_pattern = { expr : t; pattern_data_items : expr_pattern_case list }
  [@@deriving show]

  and expr_access = { expr : t; field : string Data.Located.t } [@@deriving show]

  let rec zonk (e : t) =
    let inner = zonk in
    let expr =
      match e.expr with
      | Expr_constr constr ->
          Expr_constr { constr with arguments = List.map inner constr.arguments }
      | Expr_binop binop ->
          let left, right = binop.operands in
          Expr_binop { binop with operands = (inner left, inner right) }
      | Expr_let { binding = { bind_body = { name; body } }; body = rest } ->
          Expr_let
            {
              binding = { bind_body = { name; body = inner body } };
              body = inner rest;
            }
      | Expr_if_then_else { if_exp; then_exp; else_exp } ->
          Expr_if_then_else
            {
              if_exp = inner if_exp;
              then_exp = inner then_exp;
              else_exp = inner else_exp;
            }
      | Expr_record rows ->
          Expr_record (List.map (fun row -> { row with value = inner row.value }) rows)
      | Expr_apply { fn; arg } -> Expr_apply { fn = inner fn; arg = inner arg }
      | Expr_pattern { expr; pattern_data_items } ->
          Expr_pattern
            {
              expr = inner expr;
              pattern_data_items =
                List.map
                  (fun (case : expr_pattern_case) ->
                    { case with pattern = Pattern.zonk case.pattern; expr = inner case.expr })
                  pattern_data_items;
            }
      | Expr_access { expr; field } -> Expr_access { expr = inner expr; field }
      | Expr_lambda { params; body } ->
          Expr_lambda
            {
              params =
                List.map
                  (fun (param : expr_lambda_param) -> { param with typ = Type.zonk param.typ })
                  params;
              body = inner body;
            }
      | Expr_list items -> Expr_list (List.map inner items)
      | Expr_cons { head; tail } -> Expr_cons { head = inner head; tail = inner tail }
      | Expr_tuple items -> Expr_tuple (List.map inner items)
      | Expr_record_update { record; fields } ->
          Expr_record_update
            {
              record = inner record;
              fields =
                List.map
                  (fun (row : expr_record_row) -> { row with value = inner row.value })
                  fields;
            }
      | ( Expr_ident _ | Expr_accessor _ | Expr_record_extend _
        | Expr_record_select _ | Expr_record_empty | Expr_unit | Expr_kernel _
        | Expr_char _ | Expr_string _ | Expr_int _ | Expr_float _ ) as leaf ->
          leaf
    in
    { typ = Type.zonk e.typ; expr = expr; region = e.region }


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

  let zonk (decl : t) =
    {
      decl with
      params =
        List.map
          (fun (param : param) -> { param with typ = Type.zonk param.typ })
          decl.params;
      body = Expr.zonk decl.body;
      typ = Type.zonk decl.typ;
    }
end
