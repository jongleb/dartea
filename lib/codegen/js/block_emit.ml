module J = Ast
module O = Optimized
module Scope = Data.Name.Map

type aim = { hole : int; at : int; outer : string; field : string }

let rec emit_rows ~emit_value ~emit_values env (row : O.Expr.t) (items : O.Expr.t) =
  let arity name = Names.arity_of env.Env.names name in
  let fn, arguments = Blocks.row_call ~arity row in
  let statements, values = emit_values env ((fn :: arguments) @ [ items ]) in
  let aimed =
    Option.bind env.Env.forms (fun table ->
        Option.bind (O.Expr.ident_of fn) (Forms.aim_for table))
  in
  match values with
  | made :: rest ->
      let front = List.filteri (fun index _ -> index < List.length rest - 1) rest in
      let list = List.nth rest (List.length rest - 1) in
      let tail =
        match aimed with
        | Some name -> [ J.Identifier name ]
        | None -> []
      in
      (statements, J.Array ([ made; J.Array front; list ] @ tail))
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

and js_of env name =
  match Env.jid_env env name with J.Identifier js -> Some js | _ -> None

and aim_of env ~item ~front index (hole : Blocks.hole) : aim option =
  Option.bind (Blocks.Selector.of_hole hole) (fun (sel : Blocks.Selector.t) ->
      match (js_of env sel.outer, js_of env sel.item) with
      | Some outer, Some owner when String.equal owner item ->
          List.find_index (String.equal outer) front
          |> Option.map (fun at -> { hole = index; at; outer; field = sel.field })
      | (Some _ | None), (Some _ | None) -> None)

and reads env (hole : Blocks.hole) js =
  O.Expr.free_variables ~bound:Data.Name.Set.empty hole.value
  |> Data.Name.Set.elements
  |> List.filter_map (js_of env)
  |> List.exists (String.equal js)

and aims_of env params (holes : Blocks.hole list) : aim list =
  let count = List.length params in
  if count < 2 then []
  else
    let item = List.nth params (count - 1) in
    let front = List.filteri (fun index _ -> index < count - 1) params in
    let found = List.filter_map Fun.id (List.mapi (aim_of env ~item ~front) holes) in
    let aimed index js =
      List.exists (fun (aim : aim) -> aim.hole = index && String.equal aim.outer js) found
    in
    let covered js =
      List.for_all
        (fun (index, hole) -> (not (reads env hole js)) || aimed index js)
        (List.mapi (fun index hole -> (index, hole)) holes)
    in
    if List.for_all (fun (aim : aim) -> covered aim.outer) found then found else []

and aim_expression (aim : aim) =
  J.Object
    [
      Forms.field Hole (J.int aim.hole);
      Forms.field At (J.int aim.at);
      Forms.field Get
        (J.Arrow
           {
             params = [ Runtime.item ];
             body = J.ArrowExpr (J.member (J.Identifier Runtime.item) aim.field);
           });
    ]

and note_aims table env (holes : Blocks.hole list) =
  match env.Env.home with
  | None -> ()
  | Some (fn, params) -> begin
      match aims_of env params holes with
      | [] -> ()
      | found -> Forms.aim table ~fn (J.Array (List.map aim_expression found))
    end

and locals_of env (holes : Blocks.hole list) =
  let free =
    List.fold_left
      (fun seen (hole : Blocks.hole) ->
        Data.Name.Set.union seen (O.Expr.free_variables ~bound:Data.Name.Set.empty hole.value))
      Data.Name.Set.empty holes
  in
  List.filter (fun found -> Scope.mem found env.Env.scope) (Data.Name.Set.elements free)

and refresher_of ~emit_value ~emit_values env locals (holes : Blocks.hole list) =
  let body = List.concat (List.mapi (emit_refresh ~emit_value ~emit_values env) holes) in
  let preamble =
    List.mapi
      (fun index found ->
        match Env.jid_env env found with
        | J.Identifier js ->
            J.ConstDecl { name = js; init = J.at_index (J.Identifier Runtime.refresh_args) index }
        | _ -> J.ExprStmt (J.Identifier Runtime.refresh_args))
      locals
  in
  J.Arrow
    {
      params = [ Runtime.block_state; Runtime.put; Runtime.refresh_args ];
      body = J.ArrowBlock (preamble @ body);
    }

and emit ~emit_value ~emit_values env (e : O.Expr.t) : (J.stmt list * J.expr) option =
  let shape table (form, holes) =
    let name = Forms.name table (Forms.of_form form holes) in
    let locals = locals_of env holes in
    let refresher = Forms.refresher table (refresher_of ~emit_value ~emit_values env locals holes) in
    note_aims table env holes;
    let arguments =
      match List.map (Env.jid_env env) locals with
      | [] -> J.Identifier Runtime.no_args
      | given -> J.Array given
    in
    ( [],
      J.Object
        [
          J.Field (Runtime.tag, J.string Runtime.block);
          Forms.field Form (J.Identifier name);
          Forms.field Refresh (J.Identifier refresher);
          Forms.field Args arguments;
        ] )
  in
  Option.bind env.Env.forms (fun table ->
      Option.map (shape table) (Blocks.of_expression e))
