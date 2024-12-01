open Parser

type indent_context =
  | Let
  | Case
  | TopLevel (* For top-level declarations *)
  | Expression (* For expressions and other nested structures *)

type indent_state = {
  stack : int Stack.t;
  mutable current : int;
  mutable context : indent_context;
  pending_tokens : token Queue.t;
}

let create_state () =
  {
    stack = Stack.create ();
    current = 0;
    context = TopLevel;
    pending_tokens = Queue.create ();
  }

let state =
  let state = create_state () in
  Stack.push 0 state.stack;
  state

(* Helper functions for indentation handling *)
let push_indent level context =
  Stack.push level state.stack;
  state.current <- level;
  state.context <- context

let pop_indent () =
  match Stack.pop_opt state.stack with
  | Some level -> state.current <- level
  | None -> failwith "Invalid stack state"

let get_current_indent () =
  match Stack.top_opt state.stack with Some level -> level | None -> 0

let dequeue () =
  let token = Queue.take state.pending_tokens in

  if token = DEDENT && Stack.is_empty state.stack then (
    Stack.push 0 state.stack;
    state.context <- TopLevel);
  token

let queue_dedents indent_level =
  let rec loop () =
    if Stack.is_empty state.stack then ()
    else
      let last_level = Stack.pop state.stack in
      if last_level <= indent_level then ()
      else (
        Queue.add DEDENT state.pending_tokens;
        loop ())
  in
  loop ()

(* Count spaces at the beginning of line *)
let count_indent str =
  str |> String.to_seq |> List.of_seq
  |> List.partition (( = ) '\n')
  |> snd
  |> List.fold_left (fun acc _ -> acc + 1) 0

let handle_newline nl token lexbuf =
  let indent_level = count_indent nl in
  match state.context with
  | TopLevel ->
      if indent_level = 0 then token lexbuf
      else (
        state.current <- indent_level;
        token lexbuf)
  | Expression ->
      let current = get_current_indent () in
      if indent_level < current then (
        queue_dedents indent_level;
        dequeue ())
      else token lexbuf
  | Let | Case ->
      let current = get_current_indent () in
      if indent_level < current then (
        queue_dedents indent_level;
        dequeue ())
      else if indent_level > current then (
        push_indent indent_level Expression;
        Queue.add INDENT state.pending_tokens;
        token lexbuf)
      else token lexbuf

let handle_equal lexbuf =
  (match state.context with
  | TopLevel ->
      push_indent 1 Expression;
      Queue.add INDENT state.pending_tokens
  (* | Let ->
      (* For let expressions, we need to track the specific indent level *)
      let current_indent = get_current_indent () in
      push_indent (current_indent + 2) Expression;
      Queue.add INDENT !state.pending_tokens;
      EQUAL *)
  | _ -> ());
  EQUAL

let next_token token lexbuf =
  if Queue.is_empty state.pending_tokens then token lexbuf else dequeue ()
