module J = Ast
module O = Optimized
module Scope = Data.Name.Map

let rec emit_rows ~emit_value ~emit_values env (row : O.Expr.t) (items : O.Expr.t) =
  let arity name = Names.arity_of env.Env.names name in
  let fn, arguments = Blocks.row_call ~arity row in
  let statements, values = emit_values env ((fn :: arguments) @ [ items ]) in
  match values with
  | fn :: rest ->
      let front = List.filteri (fun index _ -> index < List.length rest - 1) rest in
      (statements, J.Array [ fn; J.Array front; List.nth rest (List.length rest - 1) ])
  | [] -> (statements, J.Array [])

and emit_hole ~emit_value ~emit_values env (hole : Blocks.hole) =
  match (hole.kind, Blocks.map_call hole.value) with
  | Rows _, Some (row, items) -> emit_rows ~emit_value ~emit_values env row items
  | (Rows _ | Text | Attribute | Slot _ | Event _ | Children _ | Subtree), (Some _ | None) ->
      emit_value env hole.value

and emit_refresh ~emit_value ~emit_values env index (hole : Blocks.hole) =
  let block = J.Identifier Runtime.block_state in
  let statements, value = emit_hole ~emit_value ~emit_values env hole in
  let put =
    J.ExprStmt (J.call (J.Identifier Runtime.put) [ block; J.int index; value ])
  in
  let held = J.at_index (J.member block Runtime.deps) index in
  let guard_with seen =
    [
      J.If
        {
          test = J.binary J.StrictNotEqual held seen;
          consequent =
            (J.ExprStmt (J.Assignment { left = held; right = seen }) :: statements) @ [ put ];
          alternate = None;
        };
    ]
  in
  match Blocks.guard ~local:(fun name -> Scope.mem name env.Env.scope) hole.value with
  | On input ->
      let _, seen = emit_value env input in
      guard_with seen
  | Once -> guard_with (J.bool true)
  | Always -> statements @ [ put ]

and emit ~emit_value ~emit_values env (e : O.Expr.t) : (J.stmt list * J.expr) option =
  let shape table (form, holes) =
    let name = Forms.name table (Forms.of_form form holes) in
    let body = List.concat (List.mapi (emit_refresh ~emit_value ~emit_values env) holes) in
    ( [],
      J.Object
        [
          J.Field (Runtime.tag, J.string Runtime.block);
          Forms.field Form (J.Identifier name);
          Forms.field Refresh
            (
              J.Arrow { params = [ Runtime.block_state; Runtime.put ]; body = J.ArrowBlock body } );
        ] )
  in
  Option.bind env.Env.forms (fun table ->
      Option.map (shape table) (Blocks.of_expression e))
