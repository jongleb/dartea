open Parser

type indent_context =
  | Let
  | Let_def
  | Let_inline
  | Case
  | Case_arm_expr
  | Top_level (* For top-level declarations *)
  | Expression (* For expressions and other nested structures *)
[@@deriving show]

type indent_state = {
  stack : (int * indent_context) Stack.t;
  queue : token Queue.t;
  mutable current_string_cnum : int;
  mutable ident_compare : int;
  mutable prev_token : token option;
}

let state =
  let state =
    {
      stack = Stack.create ();
      queue = Queue.create ();
      current_string_cnum = 0;
      ident_compare = 0;
      prev_token = None;
    }
  in
  Stack.push (0, Top_level) state.stack;
  state

let close_one token lexbuf =
  if Stack.is_empty state.stack then (
    Stack.push (0, Top_level) state.stack;
    token lexbuf)
  else (
    ignore @@ Stack.pop state.stack;
    DEDENT)

let push_indent level context = Stack.push (level, context) state.stack

let get_current_context () =
  match Stack.top_opt state.stack with
  | Some (_, ctx) -> ctx
  | None -> Top_level

let get_current_indent () =
  match Stack.top_opt state.stack with Some (level, _) -> level | None -> 0

let close_until ident next_token =
  let rec go () =
    match Stack.pop_opt state.stack with
    | None -> Stack.push (0, Top_level) state.stack
    | Some ((_, Let) as x) -> Stack.push x state.stack
    | Some ((_, Let_inline) as x) ->
        Stack.push x state.stack;
        Queue.add DEDENT state.queue
    | Some (ident', ctx) when ident < ident' ->
        Queue.add DEDENT state.queue;
        go ()
    | Some (ident', ctx) when ident > ident' ->
        Stack.push (ident', ctx) state.stack
    | Some (ident', ctx) -> ()
  in
  go ();
  if Queue.is_empty state.queue then next_token () else Queue.take state.queue

let count_indent str =
  str |> String.to_seq |> List.of_seq
  |> List.partition (( = ) '\n')
  |> snd
  |> List.fold_left (fun acc _ -> acc + 1) 0

let handle_newline nl token lexbuf =
  let indent_level = count_indent nl in

  let pos = lexbuf.Lexing.lex_curr_p in

  state.current_string_cnum <- pos.Lexing.pos_cnum - count_indent nl;

  let rec go () =
    let current = get_current_indent () in
    let next_token () = token lexbuf in

    state.ident_compare <- Int.compare indent_level current;

    match get_current_context () with
    | Top_level when indent_level = 0 -> token lexbuf
    | Top_level -> close_until indent_level next_token
    | Expression when indent_level < current -> close_one token lexbuf
    | Expression -> token lexbuf
    | (Case | Let) when indent_level > current ->
        let _, l = Stack.pop state.stack in
        push_indent indent_level l;
        go ()
    | (Case | Let) when indent_level < current ->
        close_until indent_level next_token
    | Case | Let -> token lexbuf
    | Case_arm_expr when indent_level = current ->
        ignore @@ Stack.pop state.stack;
        DEDENT
    | Case_arm_expr when indent_level < current ->
        close_until indent_level next_token
    | Case_arm_expr -> token lexbuf
    | Let_def when indent_level < current -> close_until indent_level next_token
    | Let_def -> token lexbuf
    | Let_inline when indent_level < current -> DEDENT
    | Let_inline -> token lexbuf
  in
  match (state.prev_token, Stack.top_opt state.stack) with
  | Some LET, Some (_, Let_inline) ->
      let i, _ = Stack.pop state.stack in
      push_indent i Let;
      INDENT
  | _ -> go ()

let handle_equal lexbuf =
  if get_current_context () = Top_level then (
    push_indent 1 Expression;
    Queue.add INDENT state.queue);
  if get_current_context () = Let_def then Queue.add INDENT state.queue;
  if get_current_context () = Let_inline then Queue.add INDENT state.queue;
  EQUAL

let handle_case_of lexbuf =
  push_indent (get_current_indent ()) Case;
  Queue.add INDENT state.queue;
  OF

let handle_arrow lexbuf =
  if get_current_context () = Case then (
    push_indent (get_current_indent ()) Case_arm_expr;
    Queue.add INDENT state.queue);
  ARROW

let next_token token lexbuf =
  let token =
    if Queue.is_empty state.queue then token lexbuf else Queue.take state.queue
  in
  state.prev_token <- Some token;
  token

let handle_let lexbuf =
  push_indent
    (lexbuf.Lexing.lex_start_p.pos_cnum - state.current_string_cnum)
    Let_inline;
  LET

let handle_let_def lexbuf =
  match get_current_context () with
  | Let ->
      push_indent
        (lexbuf.Lexing.lex_start_p.pos_cnum - state.current_string_cnum)
        Let_def;
      LCNAME (Lexing.lexeme lexbuf)
  | Let_def when state.ident_compare = 0 ->
      ignore @@ Stack.pop state.stack;
      push_indent
        (lexbuf.Lexing.lex_start_p.pos_cnum - state.current_string_cnum + 1)
        Let_def;
      Queue.add (LCNAME (Lexing.lexeme lexbuf)) state.queue;
      DEDENT
  | Let_inline ->
      ignore @@ Stack.pop state.stack;
      push_indent
        (lexbuf.Lexing.lex_start_p.pos_cnum - state.current_string_cnum + 1)
        Let_inline;
      LCNAME (Lexing.lexeme lexbuf)
  | _ -> LCNAME (Lexing.lexeme lexbuf)

let handle_in () =
  match get_current_context () with
  | Let_inline when state.prev_token = Some DEDENT ->
      ignore @@ Stack.pop state.stack;
      IN
  | Let_inline ->
      ignore @@ Stack.pop state.stack;
      Queue.add IN state.queue;
      DEDENT
  | _ ->
      if get_current_context () = Let_def then (
        ignore @@ Stack.pop state.stack;
        Queue.add DEDENT state.queue);
      ignore @@ Stack.pop state.stack;
      Queue.add IN state.queue;
      DEDENT
