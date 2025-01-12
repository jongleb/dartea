open Parser

let show_token token =
  match token with
  | TYPE -> "TYPE"
  | ALIAS -> "ALIAS"
  | LCNAME name -> Printf.sprintf "LCNAME(%s)" name
  | UCNAME name -> Printf.sprintf "UCNAME(%s)" name
  | UCNAME_PATH path -> Printf.sprintf "UCNAME_PATH(%s)" path
  | ACCESSOR accessor -> Printf.sprintf "ACCESSOR(%s)" accessor
  | INT value -> Printf.sprintf "INT(%d)" value
  | STRING str -> Printf.sprintf "STRING(%s)" str
  | FLOAT value -> Printf.sprintf "FLOAT(%f)" value
  | EQUAL -> "="
  | EOF -> "EOF"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | COMMA -> "COMMA"
  | COLON -> "COLON"
  | PIPE -> "PIPE"
  | ARROW -> "ARROW"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | LET -> "LET"
  | IN -> "IN"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | TIMES -> "TIMES"
  | DIV -> "DIV"
  | IF -> "IF"
  | THEN -> "THEN"
  | ELSE -> "ELSE"
  | CASE -> "CASE"
  | OF -> "OF"
  | WILDCARD -> "WILDCARD"
  | CONS -> "CONS"
  | UNIT -> "UNIT"
  | IMPORT -> "IMPORT"
  | AS -> "AS"
  | DOT -> "DOT"
  | TWO_DOTS -> "TWO_DOTS"
  | EXPOSING -> "EXPOSING"
  | EQ_EQ -> "EQ_EQ"
  | GT -> "GT"
  | LT -> "LT"
  | MODULE -> "MODULE"
  | INDENT -> "INDENT"
  | DEDENT -> "DEDENT"

type indent_context =
  | Let
  | Let_def
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
}

let state =
  let state =
    {
      stack = Stack.create ();
      queue = Queue.create ();
      current_string_cnum = 0;
      ident_compare = 0;
    }
  in
  Stack.push (0, Top_level) state.stack;
  (* Кладем начальное состояние как пару *)
  state

let close_one token lexbuf =
  if Stack.is_empty state.stack then (
    Stack.push (0, Top_level) state.stack;
    token lexbuf)
  else (
    ignore @@ Stack.pop state.stack;
    DEDENT)

(* Helper functions for indentation handling *)
let push_indent level context =
  Stack.push (level, context) state.stack;

  prerr_endline
  @@ Printf.sprintf "push_indent, NOW %s"
       (show_indent_context @@ snd @@ Stack.top @@ state.stack)

(* И добавим функцию для получения текущего контекста *)
let get_current_context () =
  match Stack.top_opt state.stack with
  | Some (_, ctx) -> ctx
  | None -> Top_level

let get_current_indent () =
  match Stack.top_opt state.stack with
  | Some (level, _) -> level (* Берем только уровень отступа из пары *)
  | None -> 0

let close_until ident next_token =
  prerr_endline @@ Printf.sprintf "close_until ident %n " ident;
  let rec go () =
    match Stack.pop_opt state.stack with
    | None ->
        prerr_endline "Stack.pop_opt -> None";
        Stack.push (0, Top_level) state.stack
    | Some ((_, Let) as x) -> Stack.push x state.stack
    | Some (ident', ctx) when ident < ident' ->
        prerr_endline
        @@ Printf.sprintf
             "Stack.pop_opt -> Some (ident', _) when ident < ident', ident: \
              %n, ident' : %n , ctx: %s"
             ident ident' (show_indent_context ctx);
        Queue.add DEDENT state.queue;
        go ()
    | Some (ident', ctx) when ident > ident' ->
        prerr_endline "push back";
        Stack.push (ident', ctx) state.stack
    | Some (ident', ctx) ->
        prerr_endline
        @@ Printf.sprintf "Some (ident', ctx), ident: %n, ident' : %n , ctx: %s"
             ident ident' (show_indent_context ctx);
        ()
  in
  go ();
  if Queue.is_empty state.queue then (
    prerr_endline "close_until, branch queue is empty";
    next_token ())
  else (
    prerr_endline
    @@ Printf.sprintf "Queue lenght is %n " (Queue.length state.queue);
    let result = Queue.take state.queue in
    prerr_endline
    @@ Printf.sprintf "close_until, queue isn't empty %s" (show_token result);
    result)

let count_indent str =
  str |> String.to_seq |> List.of_seq
  |> List.partition (( = ) '\n')
  |> snd
  |> List.fold_left (fun acc _ -> acc + 1) 0

let handle_newline nl token lexbuf =
  let indent_level = count_indent nl in

  prerr_endline @@ Printf.sprintf "handle_newline, indent level %n" indent_level;

  let pos = lexbuf.Lexing.lex_curr_p in

  state.current_string_cnum <- pos.Lexing.pos_cnum - count_indent nl;

  let rec go () =
    prerr_endline "Go called";
    let current = get_current_indent () in
    let next_token () = token lexbuf in

    state.ident_compare <- Int.compare indent_level current;

    prerr_endline @@ Printf.sprintf "Current indent context value %n" current;
    prerr_endline
    @@ Printf.sprintf "indent_level < current, %b" (indent_level < current);

    match get_current_context () with
    | Top_level when indent_level = 0 ->
        prerr_endline "Top_level when indent_level = 0";
        token lexbuf
    | Top_level ->
        prerr_endline "Top_level";
        close_until indent_level next_token
    | Expression when indent_level < current ->
        prerr_endline "Expression when indent_level < current";
        close_one token lexbuf
    | Expression ->
        prerr_endline "Expression";
        token lexbuf
    | (Case | Let) when indent_level > current ->
        prerr_endline "Case, indent_level > current";
        let _, l = Stack.pop state.stack in
        push_indent indent_level l;
        go ()
    | (Case | Let) when indent_level < current ->
        prerr_endline "Case when indent_level < current";
        close_until indent_level next_token
    | Case | Let ->
        prerr_endline "Case";
        token lexbuf
    | Case_arm_expr when indent_level = current ->
        prerr_endline "Case_arm_expr";
        ignore @@ Stack.pop state.stack;
        DEDENT
    | Case_arm_expr when indent_level < current ->
        prerr_endline "Case_arm_expr when indent_level < current";
        close_until indent_level next_token
    | Case_arm_expr ->
        prerr_endline "Case_arm_expr";
        token lexbuf
    | Let_def when indent_level < current ->
        prerr_endline "Let_def when indent_level < current";
        close_until indent_level next_token
    | Let_def ->
        prerr_endline "Let_def";
        token lexbuf
  in
  go ()

let handle_equal lexbuf =
  prerr_endline "handle_equal";
  if get_current_context () = Top_level then (
    prerr_endline "handle_equal, Top_level branch";
    push_indent 1 Expression;
    Queue.add INDENT state.queue);
  EQUAL

let handle_case_of lexbuf =
  prerr_endline "handle_case_of";
  push_indent (get_current_indent ()) Case;
  Queue.add INDENT state.queue;
  OF

let handle_arrow lexbuf =
  prerr_endline "handle_arrow";
  if get_current_context () = Case then (
    push_indent (get_current_indent ()) Case_arm_expr;
    Queue.add INDENT state.queue);
  ARROW

let next_token token lexbuf =
  prerr_endline
  @@ Printf.sprintf "next_token, lexeme: %s"
  @@ Lexing.lexeme lexbuf;
  if Queue.is_empty state.queue then (
    let result = token lexbuf in
    prerr_endline
    @@ Printf.sprintf "Queue is empty, result %s" (show_token result);
    result)
  else
    let result = Queue.take state.queue in
    prerr_endline
    @@ Printf.sprintf "Queue isn't empty, result %s" (show_token result);
    result

let handle_let lexbuf =
  push_indent
    (lexbuf.Lexing.lex_start_p.pos_cnum - state.current_string_cnum)
    Let;
  Queue.add INDENT state.queue;
  LET

let handle_let_def lexbuf =
  prerr_endline "handle_let_def";

  match get_current_context () with
  | Let ->
      prerr_endline "handle_let_def, let branch";

      push_indent
        (lexbuf.Lexing.lex_start_p.pos_cnum - state.current_string_cnum)
        Let_def;
      Queue.add INDENT state.queue;
      LCNAME (Lexing.lexeme lexbuf)
  | Let_def when state.ident_compare = 0 ->
      ignore @@ Stack.pop state.stack;
      push_indent
        (lexbuf.Lexing.lex_start_p.pos_cnum - state.current_string_cnum)
        Let_def;
      Queue.add (LCNAME (Lexing.lexeme lexbuf)) state.queue;
      Queue.add INDENT state.queue;
      DEDENT
  | _ -> LCNAME (Lexing.lexeme lexbuf)

let handle_in () =
  prerr_endline "hanlde_in";

  (* match state.ident_compare with
     | 0 -> pattern *)
  if get_current_context () = Let_def then (
    ignore @@ Stack.pop state.stack;
    Queue.add DEDENT state.queue);
  ignore @@ Stack.pop state.stack;
  Queue.add IN state.queue;
  DEDENT
