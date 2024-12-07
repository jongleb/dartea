open Parser

type indent_context =
  | Let
  | Case
  | Case_arm_expr
  | Top_level (* For top-level declarations *)
  | Expression (* For expressions and other nested structures *)
[@@deriving show]

type indent_state = {
  stack : (int * indent_context) Stack.t;
  mutable current : int;
  pending_tokens : token Queue.t;
}

let create_state () =
  { stack = Stack.create (); current = 0; pending_tokens = Queue.create () }

let state =
  let state = create_state () in
  Stack.push (0, Top_level) state.stack;
  (* Кладем начальное состояние как пару *)
  state

(* Helper functions for indentation handling *)
let push_indent level context =
  Stack.push (level, context) state.stack;
  state.current <- level

(* И добавим функцию для получения текущего контекста *)
let get_current_context () =
  match Stack.top_opt state.stack with
  | Some (_, ctx) -> ctx
  | None -> Top_level

let get_current_indent () =
  match Stack.top_opt state.stack with
  | Some (level, _) -> level (* Берем только уровень отступа из пары *)
  | None -> 0

let dequeue () =
  let token = Queue.take state.pending_tokens in

  if token = DEDENT && Stack.is_empty state.stack then
    Stack.push (0, Top_level) state.stack (* Кладем пару (отступ, контекст) *);
  token

let queue_dedents indent_level =
  ()
  |> Seq.unfold (fun () ->
         state.stack |> Stack.pop_opt |> Option.map (fun i -> (i, ())))
  |> Seq.take_while (fun (last_level, indent_context) ->
         last_level > indent_level)
  |> Seq.iter (fun _ -> Queue.add DEDENT state.pending_tokens)
(* Count spaces at the beginning of line *)

let count_indent str =
  str |> String.to_seq |> List.of_seq
  |> List.partition (( = ) '\n')
  |> snd
  |> List.fold_left (fun acc _ -> acc + 1) 0

let handle_newline nl token lexbuf =
  let indent_level = count_indent nl in
  match get_current_context () with
  | Top_level ->
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
  | Case_arm_expr ->
      let current = get_current_indent () in
      if indent_level = current then (
        (* На том же уровне отступа - возвращаемся к Case *)
        ignore @@ Stack.pop state.stack;
        (* Убираем текущий Case_arm_expr *)
        DEDENT)
      else if indent_level < current then (
        queue_dedents indent_level;
        dequeue ())
      else token lexbuf
  | Let | Case ->
      let current = get_current_indent () in
      (if indent_level > current then
         let _, l = Stack.pop state.stack in
         push_indent indent_level l);
      if indent_level < current then (
        queue_dedents indent_level;
        if not (Queue.is_empty state.pending_tokens) then dequeue ()
        else token lexbuf)
      else token lexbuf

let handle_equal lexbuf =
  (match get_current_context () with
  | Top_level ->
      push_indent 1 Expression;
      Queue.add INDENT state.pending_tokens
  | Let | Case | Case_arm_expr | Expression -> ());
  EQUAL

let handle_case_of lexbuf =
  push_indent (get_current_indent ()) Case;
  (* Заменили присваивание на push *)
  Queue.add INDENT state.pending_tokens;
  OF

let handle_arrow lexbuf =
  let current_context = get_current_context () in
  (match current_context with
  | Case ->
      push_indent (get_current_indent ()) Case_arm_expr;
      (* Заменили присваивание на push *)
      Queue.add INDENT state.pending_tokens
  | Let | Top_level | Case_arm_expr | Expression -> ());
  ARROW

let next_token token lexbuf =
  if Queue.is_empty state.pending_tokens then token lexbuf else dequeue ()
