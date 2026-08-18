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
