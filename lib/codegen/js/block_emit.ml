module J = Ast
module O = Optimized
module Scope = Data.Name.Map

type aim = { hole : int; at : int; outer : string; field : string option }

let js_of env name =
  match Env.jid_env env name with J.Identifier js -> Some js | _ -> None

let locals_of env (holes : Blocks.hole list) =
  let free =
    List.fold_left
      (fun seen (hole : Blocks.hole) ->
        Data.Name.Set.union seen (O.Expr.free_variables ~bound:Data.Name.Set.empty hole.value))
      Data.Name.Set.empty holes
  in
  Data.Name.Set.elements free
  |> List.filter_map (fun found ->
         if Scope.mem found env.Env.scope then
           Option.map (fun js -> (found, js)) (js_of env found)
         else None)

let reads env (hole : Blocks.hole) js =
  O.Expr.free_variables ~bound:Data.Name.Set.empty hole.value
  |> Data.Name.Set.elements
  |> List.filter_map (js_of env)
  |> List.exists (String.equal js)

let aim_of env ~item ~front index (hole : Blocks.hole) : aim option =
  Option.bind (Blocks.Selector.of_hole hole) (fun (sel : Blocks.Selector.t) ->
      match (js_of env sel.outer, js_of env sel.item) with
      | Some outer, Some owner when String.equal owner item ->
          List.find_index (String.equal outer) front
          |> Option.map (fun at -> { hole = index; at; outer; field = sel.field })
      | (Some _ | None), (Some _ | None) -> None)

let aims_of env params (holes : Blocks.hole list) : aim list =
  if List.length params < 2 then []
  else
    let item = Blocks.last params in
    let front = Blocks.front params in
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

let aim_expression (aim : aim) =
  let item = J.Identifier Runtime.item in
  let key = Option.fold ~none:item ~some:(J.member item) aim.field in
  J.Object
    [
      Forms.field Hole (J.int aim.hole);
      Forms.field At (J.int aim.at);
      Forms.field Get (J.Arrow { params = [ Runtime.item ]; body = J.ArrowExpr key });
    ]

let note_aims table env (holes : Blocks.hole list) =
  match env.Env.home with
  | None -> ()
  | Some (fn, params) -> begin
      match aims_of env params holes with
      | [] -> ()
      | found -> Forms.aim table ~fn (J.Array (List.map aim_expression found))
    end

let guarded_group ~at ~watched body =
  let held index = J.at_index (J.member (J.Identifier Runtime.block_state) Runtime.deps) (at + index) in
  let stale index seen = J.binary J.StrictNotEqual (held index) seen in
  let test =
    match List.mapi stale watched with
    | [] -> J.bool true
    | first :: rest -> List.fold_left (fun found one -> J.binary J.Or found one) first rest
  in
  let remember index seen = J.ExprStmt (J.Assignment { left = held index; right = seen }) in
  [ J.If { test; consequent = List.mapi remember watched @ body; alternate = None } ]

let emit_rows ~emit_value ~emit_values env (row : O.Expr.t) (items : O.Expr.t) =
  let arity name = Names.arity_of env.Env.names name in
  let fn, arguments = Blocks.row_call ~arity row in
  let statements, values = emit_values env ((fn :: arguments) @ [ items ]) in
  let aims =
    Option.bind env.Env.forms (fun table ->
        Option.bind (O.Expr.ident_of fn) (Forms.aim_for table))
    |> Option.fold ~none:[] ~some:(fun name -> [ J.Identifier name ])
  in
  match values with
  | made :: rest ->
      let called = [ made; J.Array (Blocks.front rest); Blocks.last rest ] in
      (statements, J.Array (called @ aims))
  | [] -> (statements, J.Array [])

let emit_hole ~emit_value ~emit_values env (hole : Blocks.hole) =
  match (hole.kind, Blocks.map_call hole.value) with
  | Rows _, Some (row, items) -> emit_rows ~emit_value ~emit_values env row items
  | (Rows _ | Text | Attribute | Slot _ | Event _ | Children _ | Subtree), (Some _ | None) ->
      emit_value env hole.value

let emit_watched ~emit_value ~emit_values env index (hole : Blocks.hole) =
  let statements, value = emit_hole ~emit_value ~emit_values env hole in
  let put =
    J.ExprStmt
      (J.call (J.Identifier Runtime.put)
         [ J.Identifier Runtime.block_state; J.int index; value ])
  in
  let watched =
    match Blocks.Guard.of_expression ~local:(fun name -> Scope.mem name env.Env.scope) hole.value with
    | Blocks.Guard.On inputs -> List.map (fun input -> snd (emit_value env input)) inputs
    | Blocks.Guard.Once -> [ J.bool true ]
    | Blocks.Guard.Always -> []
  in
  (watched, statements @ [ put ])

let emit_refreshes ~emit_value ~emit_values env (holes : Blocks.hole list) =
  let watched = List.mapi (emit_watched ~emit_value ~emit_values env) holes in
  let written seen = String.concat "," (List.map To_string.expr_to_string seen) in
  let rec sweep at pending taken =
    match pending with
    | [] -> List.rev taken
    | ([], body) :: rest -> sweep at rest (([], at, body) :: taken)
    | (seen, body) :: rest ->
        let key = written seen in
        let alike, others =
          List.partition (fun (kept, _) -> String.equal key (written kept)) rest
        in
        let body = List.concat (body :: List.map snd alike) in
        sweep (at + List.length seen) others ((seen, at, body) :: taken)
  in
  sweep 0 watched []
  |> List.concat_map (fun (watched, at, body) ->
         match watched with [] -> body | _ -> guarded_group ~at ~watched body)

let refresher_of ~emit_value ~emit_values env locals (holes : Blocks.hole list) =
  let carrier = J.Identifier Runtime.carrier in
  let unpack index (_, js) =
    J.ConstDecl { name = js; init = J.member carrier (Runtime.carried index) }
  in
  let body = emit_refreshes ~emit_value ~emit_values env holes in
  J.Arrow
    {
      params = [ Runtime.block_state; Runtime.put; Runtime.carrier ];
      body = J.ArrowBlock (List.mapi unpack locals @ body);
    }

let emit ~emit_value ~emit_values env (e : O.Expr.t) : (J.stmt list * J.expr) option =
  let shape table (form, holes) =
    let local name = Scope.mem name env.Env.scope in
    let guards = Blocks.Guard.slots ~local holes in
    let name = Forms.name table (Forms.of_form ~guards form holes) in
    let locals = locals_of env holes in
    let refresher =
      Forms.refresher table (refresher_of ~emit_value ~emit_values env locals holes)
    in
    note_aims table env holes;
    let carried =
      List.mapi (fun index (found, _) -> J.Field (Runtime.carried index, Env.jid_env env found)) locals
    in
    ( [],
      J.Object
        ([
           J.Field (Runtime.tag, J.string Runtime.block);
           Forms.field Form (J.Identifier name);
           Forms.field Refresh (J.Identifier refresher);
         ]
        @ carried) )
  in
  Option.bind env.Env.forms (fun table ->
      Option.map (shape table) (Blocks.of_expression e))
