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
  state.current <- level;

  prerr_endline
  @@ Printf.sprintf "PUSHED, NOW %s"
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

let dequeue () =
  prerr_endline
  @@ Printf.sprintf "DEQUQUE CONTEXT  IS: %s"
       (show_indent_context @@ get_current_context ());

  let token = Queue.take state.pending_tokens in

  if token = DEDENT && Stack.is_empty state.stack then
    Stack.push (0, Top_level) state.stack (* Кладем пару (отступ, контекст) *);

  token

let queue_dedents indent_level =
  let rec dedent () =
    if
      (not (Stack.is_empty state.stack))
      && fst (Stack.top state.stack) > indent_level
    then (
      ignore (Stack.pop state.stack);
      Queue.add DEDENT state.pending_tokens;
      dedent ())
  in
  dedent ()

let count_indent str =
  str |> String.to_seq |> List.of_seq
  |> List.partition (( = ) '\n')
  |> snd
  |> List.fold_left (fun acc _ -> acc + 1) 0

let current_string_cnum = ref 0

let handle_newline nl token lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in

  current_string_cnum := pos.Lexing.pos_cnum - count_indent nl;

  prerr_endline "HANDLE NEWLINE";
  let indent_level = count_indent nl in

  match get_current_context () with
  | Top_level ->
      prerr_endline "Top_level";
      if indent_level = 0 then token lexbuf
      else (
        state.current <- indent_level;
        token lexbuf)
  | Expression ->
      prerr_endline "Expression";
      let current = get_current_indent () in
      if indent_level < current then (
        prerr_endline "INDENT LEVEL LESS THAN CURRENT";
        queue_dedents indent_level;
        dequeue ())
      else token lexbuf
  | Case_arm_expr ->
      prerr_endline "CASE ARM EXPR";
      let current = get_current_indent () in
      if indent_level = current then (
        prerr_endline "INDENT EQUAL CURRENT";
        (* На том же уровне отступа - возвращаемся к Case *)
        prerr_endline @@ show_indent_context @@ snd @@ Stack.pop state.stack;
        (* Убираем текущий Case_arm_expr *)
        DEDENT)
      else if indent_level < current then (
        queue_dedents indent_level;
        dequeue ())
      else token lexbuf
  | Let_def ->
      prerr_endline "LET DEF EXPR";
      let current = get_current_indent () in
      prerr_endline
      @@ Printf.sprintf "CURRENT INDENT: %n" (get_current_indent ());
      prerr_endline @@ Printf.sprintf "INDENT LEVEL IS: %n" indent_level;
      if indent_level = current then (
        prerr_endline "INDENT EQUAL CURRENT";
        prerr_endline @@ show_indent_context @@ snd @@ Stack.pop state.stack;
        DEDENT)
      else if indent_level < current then (
        queue_dedents indent_level;
        dequeue ())
      else token lexbuf
  | (Let | Case) as l ->
      prerr_endline
        (Printf.sprintf "HANDLING THE FOLLOWING %s" (show_indent_context l));
      let current = get_current_indent () in
      prerr_endline @@ Printf.sprintf "CURRENT INDENT: %n" current;
      prerr_endline @@ Printf.sprintf "INDENT LEVEL IS: %n" indent_level;
      if indent_level > current then (
        prerr_endline "INDENT LEVEL MORE THEN CURRENT";
        let _, l = Stack.pop state.stack in
        push_indent indent_level l);
      if indent_level < current then (
        queue_dedents indent_level;
        if not (Queue.is_empty state.pending_tokens) then (
          prerr_endline "DEQUQUE CALLED";
          dequeue ())
        else (
          prerr_endline "TOKEN LEXBUF #1";
          token lexbuf))
      else (
        prerr_endline "TOKEN LEXBUF #2";
        token lexbuf)

let handle_equal lexbuf =
  prerr_endline "HANDLE EQUAL";
  (match get_current_context () with
  | Top_level ->
      push_indent 1 Expression;
      Queue.add INDENT state.pending_tokens
  | Let | Let_def | Case | Case_arm_expr | Expression -> ());
  EQUAL

let handle_case_of lexbuf =
  prerr_endline "HANDLE CASE OF";
  push_indent (get_current_indent ()) Case;
  (* Заменили присваивание на push *)
  Queue.add INDENT state.pending_tokens;
  OF

let handle_arrow lexbuf =
  prerr_endline "HANDLE ARROW";
  let current_context = get_current_context () in
  prerr_endline
    (Printf.sprintf "CONTEXT : %s" (show_indent_context current_context));
  (match current_context with
  | Case ->
      push_indent (get_current_indent ()) Case_arm_expr;
      (* Заменили присваивание на push *)
      Queue.add INDENT state.pending_tokens
  | Let | Let_def | Top_level | Case_arm_expr | Expression -> ());
  ARROW

let next_token token lexbuf =
  prerr_endline @@ Printf.sprintf "NEXT LEXBUF %s" @@ Lexing.lexeme lexbuf;
  let result =
    if Queue.is_empty state.pending_tokens then (
      prerr_endline "QUEUE IS EMPTY";
      token lexbuf)
    else (
      prerr_endline "QUEUE ISN'T EMPTY";
      dequeue ())
  in
  prerr_endline (Printf.sprintf "RESULT OF NEXT TOKEN: %s" (show_token result));
  prerr_endline
  @@ Printf.sprintf "AND REMAINED INDENT : %s"
       (show_indent_context (get_current_context ()));
  result

let handle_let lexbuf =
  prerr_endline "HANDLE LET";
  push_indent (get_current_indent ()) Let;
  Queue.add INDENT state.pending_tokens;
  LET

let show_position position =
  Format.asprintf
    "{pos_fname = %s; npos_lnum = %d; npos_bol = %d; npos_cnum = %d;}"
    (if position.Lexing.pos_fname = "" then {|""|} else position.pos_fname)
    position.pos_lnum position.pos_cnum position.pos_cnum

let handle_let_def lexbuf =
  prerr_endline "HANDLE LET DEF";

  prerr_endline @@ Int.to_string !current_string_cnum;
  prerr_endline @@ show_position lexbuf.Lexing.lex_start_p;

  (match get_current_context () with
  | Let ->
      prerr_endline
      @@ Printf.sprintf "SELECTED  %n"
           (lexbuf.Lexing.lex_start_p.pos_cnum - !current_string_cnum);
      prerr_endline
      @@ Printf.sprintf "AND CURRENT SELECTED  %n" (get_current_indent ());
      push_indent
        (lexbuf.Lexing.lex_start_p.pos_cnum - !current_string_cnum)
        Let_def;
      Queue.add INDENT state.pending_tokens
  | Let_def | Case | Case_arm_expr | Expression | Top_level -> ());
  LCNAME (Lexing.lexeme lexbuf)

let handle_in () =
  prerr_endline "HANDLE IN";
  ignore @@ Stack.pop state.stack;
  ignore @@ Stack.pop state.stack;
  Queue.add DEDENT state.pending_tokens;
  Queue.add DEDENT state.pending_tokens;
  IN
