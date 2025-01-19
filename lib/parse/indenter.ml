open Parser

type indent_context =
  | Let
  | Let_def
  | Let_inline
  | If
  | Case
  | Case_arm_expr
  | Top_level
  | Expression
[@@deriving show]

type indent_state = {
  stack : (int * indent_context) Stack.t;
  queue : token Queue.t;
  mutable current_string_cnum : int;
  mutable ident_compare : int;
  mutable prev_token : token option;
}

let close_one state next_token =
  if Stack.is_empty state.stack then (
    Stack.push (0, Top_level) state.stack;
    next_token state)
  else (
    ignore @@ Stack.pop state.stack;
    DEDENT)

let push_indent state level context = Stack.push (level, context) state.stack

let get_current_context state =
  match Stack.top_opt state.stack with
  | Some (_, ctx) -> ctx
  | None -> Top_level

let get_current_indent state =
  match Stack.top_opt state.stack with Some (level, _) -> level | None -> 0

let close_until state ident next_token =
  let rec go () =
    match Stack.pop_opt state.stack with
    | None -> Stack.push (0, Top_level) state.stack
    | Some ((_, Let) as x) -> Stack.push x state.stack
    | Some ((_, Let_inline) as x) ->
        Stack.push x state.stack;
        Queue.add DEDENT state.queue
    | Some ((_, If) as x) -> Stack.push x state.stack
    | Some (ident', ctx) when ident < ident' ->
        Queue.add DEDENT state.queue;
        go ()
    | Some (ident', ctx) when ident > ident' ->
        Stack.push (ident', ctx) state.stack
    | Some (ident', Case_arm_expr) when ident = ident' ->
        Queue.add DEDENT state.queue
    | Some (ident', ctx) -> ()
  in
  go ();
  if Queue.is_empty state.queue then next_token state
  else Queue.take state.queue

let count_indent str =
  str |> String.to_seq |> List.of_seq
  |> List.partition (( = ) '\n')
  |> snd
  |> List.fold_left (fun acc _ -> acc + 1) 0

let handle_newline state nl token lexbuf =
  let indent_level = count_indent nl in

  let pos = lexbuf.Lexing.lex_curr_p in

  state.current_string_cnum <- pos.Lexing.pos_cnum - count_indent nl;

  let rec go () =
    let current = get_current_indent state in
    let next_token state = token state lexbuf in

    state.ident_compare <- Int.compare indent_level current;

    match get_current_context state with
    | Top_level when indent_level = 0 -> token state lexbuf
    | Top_level -> close_until state indent_level next_token
    | Expression when indent_level < current -> close_one state next_token
    | Expression -> next_token state
    | (Case | Let) when indent_level > current ->
        let _, l = Stack.pop state.stack in
        push_indent state indent_level l;
        go ()
    | (Case | Let) when indent_level < current ->
        close_until state indent_level next_token
    | Case | Let -> next_token state
    | Case_arm_expr when indent_level = current ->
        ignore @@ Stack.pop state.stack;
        DEDENT
    | Case_arm_expr when indent_level < current ->
        close_until state indent_level next_token
    | Case_arm_expr -> next_token state
    | Let_def when indent_level < current ->
        close_until state indent_level next_token
    | Let_def -> next_token state
    | Let_inline when indent_level < current -> DEDENT
    | Let_inline -> next_token state
    | If when indent_level > current -> next_token state
    | If -> next_token state
  in

  match (state.prev_token, Stack.top_opt state.stack) with
  | Some LET, Some (_, Let_inline) ->
      let i, _ = Stack.pop state.stack in
      push_indent state i Let;
      INDENT
  | _ -> go ()

let handle_equal state lexbuf =
  if get_current_context state = Top_level then (
    push_indent state 1 Expression;
    Queue.add INDENT state.queue);
  if get_current_context state = Let_def then Queue.add INDENT state.queue;
  if get_current_context state = Let_inline then Queue.add INDENT state.queue;
  EQUAL

let handle_case_of state lexbuf =
  push_indent state (get_current_indent state) Case;
  Queue.add INDENT state.queue;
  OF

let handle_arrow state lexbuf =
  if get_current_context state = Case then (
    push_indent state (get_current_indent state) Case_arm_expr;
    Queue.add INDENT state.queue);
  ARROW

let next_token state token lexbuf =
  let token =
    if Queue.is_empty state.queue then token state lexbuf
    else Queue.take state.queue
  in

  state.prev_token <- Some token;
  token

let handle_let state lexbuf =
  push_indent state
    (lexbuf.Lexing.lex_start_p.pos_cnum - state.current_string_cnum)
    Let_inline;
  LET

let handle_let_def state lexbuf =
  match get_current_context state with
  | Let ->
      push_indent state
        (lexbuf.Lexing.lex_start_p.pos_cnum - state.current_string_cnum)
        Let_def;
      LCNAME (Lexing.lexeme lexbuf)
  | Let_def when state.ident_compare = 0 ->
      ignore @@ Stack.pop state.stack;
      push_indent state
        (lexbuf.Lexing.lex_start_p.pos_cnum - state.current_string_cnum + 1)
        Let_def;
      Queue.add (LCNAME (Lexing.lexeme lexbuf)) state.queue;
      DEDENT
  | Let_inline ->
      ignore @@ Stack.pop state.stack;
      push_indent state
        (lexbuf.Lexing.lex_start_p.pos_cnum - state.current_string_cnum + 1)
        Let_inline;
      LCNAME (Lexing.lexeme lexbuf)
  | _ -> LCNAME (Lexing.lexeme lexbuf)

let handle_in state =
  match get_current_context state with
  | Let_inline when state.prev_token = Some DEDENT ->
      ignore @@ Stack.pop state.stack;
      IN
  | Let_inline ->
      ignore @@ Stack.pop state.stack;
      Queue.add IN state.queue;
      DEDENT
  | ctx ->
      if get_current_context state = Let_def then (
        ignore @@ Stack.pop state.stack;
        Queue.add DEDENT state.queue);
      if get_current_context state = Case_arm_expr then (
        ignore @@ Stack.pop state.stack;
        Queue.add DEDENT state.queue;
        ignore @@ Stack.pop state.stack;
        Queue.add DEDENT state.queue);
      ignore @@ Stack.pop state.stack;
      Queue.add IN state.queue;
      DEDENT

let handle_eof state =
  if get_current_context state = Expression then (
    ignore @@ Stack.pop state.stack;
    Queue.add EOF state.queue;
    DEDENT)
  else EOF

let handle_if state =
  push_indent state (get_current_indent state) If;
  IF

let handle_else state =
  ignore @@ Stack.pop state.stack;
  ELSE
