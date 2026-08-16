module T = Typed
module O = Optimized

let rec spine arguments (e : Typed.Expr.t) =
  match e.expr with
  | Typed.Expr.Expr_apply { fn; arg } -> spine (arg :: arguments) fn
  | _ -> (e, arguments)

let rec type_of_typed (t : T.Type.t) : O.Type.t =
  match t with
  | T.Type.TVar name -> O.Type.TVar name
  | T.Type.TInt -> O.Type.TInt
  | T.Type.TFloat -> O.Type.TFloat
  | T.Type.TChar -> O.Type.TChar
  | T.Type.TBool -> O.Type.TBool
  | T.Type.TStr -> O.Type.TStr
  | T.Type.TUnit -> O.Type.TUnit
  | T.Type.TFun (t1, t2) -> O.Type.TFun (type_of_typed t1, type_of_typed t2)
  | T.Type.TTup ts -> O.Type.TTup (List.map type_of_typed ts)
  | T.Type.TCustom (name, args) ->
      O.Type.TCustom (name, List.map type_of_typed args)
  | T.Type.TRecord t -> O.Type.TRecord (type_of_typed t)
  | T.Type.TRowExtend (name, t1, t2) ->
      O.Type.TRowExtend (name, type_of_typed t1, type_of_typed t2)
  | T.Type.TRowEmpty -> O.Type.TRowEmpty

let rec pattern_of_typed (p : T.Pattern.t) : O.Pattern.t =
  let typ = type_of_typed p.typ in
  let pattern =
    match p.pattern with
    | T.Pattern.P_T_anything -> O.Pattern.P_T_anything
    | T.Pattern.P_T_var name -> O.Pattern.P_T_var name
    | T.Pattern.P_T_record fields -> O.Pattern.P_T_record fields
    | T.Pattern.P_T_unit -> O.Pattern.P_T_unit
    | T.Pattern.P_T_tuple ps ->
        O.Pattern.P_T_tuple (List.map pattern_of_typed ps)
    | T.Pattern.P_T_list ps -> O.Pattern.P_T_list (List.map pattern_of_typed ps)
    | T.Pattern.P_T_cons (hd, tl) ->
        O.Pattern.P_T_cons (pattern_of_typed hd, pattern_of_typed tl)
    | T.Pattern.P_T_chr c -> O.Pattern.P_T_chr c
    | T.Pattern.P_T_str s -> O.Pattern.P_T_str s
    | T.Pattern.P_T_int n -> O.Pattern.P_T_int n
    | T.Pattern.P_T_ctor (name, args) ->
        O.Pattern.P_T_ctor (name, List.map pattern_of_typed args)
    | T.Pattern.P_T_alias (inner, name) ->
        O.Pattern.P_T_alias (pattern_of_typed inner, name)
  in
  { O.Pattern.typ; pattern }

(* Convert expression *)
let rec expr_of_typed (e : T.Expr.t) : O.Expr.t =
  let typ = type_of_typed e.typ in
  let expr =
    match e.expr with
    | T.Expr.Expr_constr { name; arguments } ->
        O.Expr.Expr_constr
          { name; arguments = List.map expr_of_typed arguments }
    | T.Expr.Expr_binop { name; operands = e1, e2 } ->
        O.Expr.Expr_binop
          { name; operands = (expr_of_typed e1, expr_of_typed e2) }
    | T.Expr.Expr_let { binding; body } ->
        let bind_body =
          {
            O.Expr.name = binding.bind_body.name;
            body = expr_of_typed binding.bind_body.body;
          }
        in
        O.Expr.Expr_let
          { binding = { O.Expr.bind_body }; body = expr_of_typed body }
    | T.Expr.Expr_if_then_else { if_exp; then_exp; else_exp } ->
        O.Expr.Expr_if_then_else
          {
            if_exp = expr_of_typed if_exp;
            then_exp = expr_of_typed then_exp;
            else_exp = expr_of_typed else_exp;
          }
    | T.Expr.Expr_record rows ->
        O.Expr.Expr_record
          (List.map
             (fun { T.Expr.name; value } ->
               { O.Expr.name; value = expr_of_typed value })
             rows)
    | T.Expr.Expr_apply { fn; arg } -> begin
        let head, arguments = spine [ arg ] fn in
        match (head.T.Expr.expr, arguments) with
        | T.Expr.Expr_kernel (Unary kernel), [ argument ] ->
            O.Expr.Expr_kernel
              (Kernel_unary { kernel; argument = expr_of_typed argument })
        | T.Expr.Expr_kernel (Binary kernel), [ left; right ] ->
            O.Expr.Expr_kernel
              (Kernel_binary
                 {
                   kernel;
                   left = expr_of_typed left;
                   right = expr_of_typed right;
                 })
        | _ ->
            O.Expr.Expr_apply
              { fn = expr_of_typed fn; arg = expr_of_typed arg }
      end
    | T.Expr.Expr_ident name -> O.Expr.Expr_ident name
    | T.Expr.Expr_pattern { expr; pattern_data_items } ->
        O.Expr.Expr_pattern
          {
            expr = expr_of_typed expr;
            pattern_data_items =
              List.map
                (fun { T.Expr.pattern; expr } ->
                  {
                    O.Expr.pattern = pattern_of_typed pattern;
                    expr = expr_of_typed expr;
                  })
                pattern_data_items;
          }
    | T.Expr.Expr_accessor field -> O.Expr.Expr_accessor field
    | T.Expr.Expr_access { expr; field } ->
        O.Expr.Expr_access { expr = expr_of_typed expr; field }
    | T.Expr.Expr_record_extend name -> O.Expr.Expr_record_extend name
    | T.Expr.Expr_record_select name -> O.Expr.Expr_record_select name
    | T.Expr.Expr_record_empty -> O.Expr.Expr_record_empty
    | T.Expr.Expr_unit -> O.Expr.Expr_unit
    | T.Expr.Expr_kernel kernel -> O.Expr.Expr_kernel (Kernel_value kernel)
    | T.Expr.Expr_lambda { params; body } ->
        let params =
          List.map
            (fun (p : T.Expr.expr_lambda_param) ->
              { O.Expr.name = p.name; typ = type_of_typed p.typ })
            params
        in
        O.Expr.Expr_lambda { params; body = expr_of_typed body }
    | T.Expr.Expr_char c -> O.Expr.Expr_char c
    | T.Expr.Expr_string s -> O.Expr.Expr_string s
    | T.Expr.Expr_int n -> O.Expr.Expr_int n
    | T.Expr.Expr_float f -> O.Expr.Expr_float f
    | T.Expr.Expr_list es -> O.Expr.Expr_list (List.map expr_of_typed es)
    | T.Expr.Expr_tuple items ->
        O.Expr.Expr_tuple (List.map expr_of_typed items)
    | T.Expr.Expr_record_update { record; fields } ->
        O.Expr.Expr_record_update
          {
            record = expr_of_typed record;
            fields =
              List.map
                (fun (row : T.Expr.expr_record_row) ->
                  { O.Expr.name = row.name; value = expr_of_typed row.value })
                fields;
          }
    | T.Expr.Expr_cons { head; tail } ->
        O.Expr.Expr_cons
          { head = expr_of_typed head; tail = expr_of_typed tail }
  in
  { O.Expr.typ; expr }

(* Convert declaration *)
let declaration_of_typed (d : T.Declaration.t) : O.Declaration.t =
  let params =
    List.map
      (fun (p : T.Declaration.param) ->
        { O.Declaration.name = p.name; typ = type_of_typed p.typ })
      d.params
  in
  {
    O.Declaration.name = d.name;
    params;
    body = expr_of_typed d.body;
    typ = type_of_typed d.typ;
  }

(* Convert list of declarations *)
let convert (decls : T.Declaration.t list) : O.Declaration.t list =
  List.map declaration_of_typed decls
