open Parser

let show_token token =
  match token with
  | LCNAME name -> Printf.sprintf "LCNAME(%s)" name
  | UCNAME name -> Printf.sprintf "UCNAME(%s)" name
  | UCNAME_PATH path -> Printf.sprintf "UCNAME_PATH(%s)" path
  | ACCESS field -> Printf.sprintf "ACCESS(%s)" field
  | INT value -> Printf.sprintf "INT(%d)" value
  | STRING str -> Printf.sprintf "STRING(%s)" str
  | FLOAT value -> Printf.sprintf "FLOAT(%f)" value
  | TYPE -> "TYPE"
  | ALIAS -> "ALIAS"
  | COLON -> "COLON"
  | EQUAL -> "="
  | EOF -> "EOF"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | COMMA -> "COMMA"
  | ARROW -> "ARROW"
  | PIPE -> "PIPE"
  | PIPE_GT -> "PIPE_GT"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | LET -> "LET"
  | IN -> "IN"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | TIMES -> "TIMES"
  | DIV -> "DIV"
  | IF -> "IF"
  | BACKSLASH -> "BACKSLASH"
  | THEN -> "THEN"
  | ELSE -> "ELSE"
  | CASE -> "CASE"
  | OF -> "OF"
  | WILDCARD -> "WILDCARD"
  | UMINUS -> "UMINUS"
  | CONS -> "CONS"
  | UNIT -> "UNIT"
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
  | Let_inline
  | If
  | Case
  | Case_arm_expr
  | Type_alias
  | Type_decl
  | Type_annotation
  | Top_level
  | Expression
[@@deriving show]

type indent_state = {
  stack : (int * indent_context) Stack.t;
  queue : token Queue.t;
  mutable current_string_cnum : int;
  mutable ident_compare : int;
  mutable prev_token : token option;
  mutable case_opened : bool;
  mutable in_type_decl : bool;
  mutable need_dedent_after_type : bool;
}

let close_one state next_token =
  if Stack.is_empty state.stack then (
    Stack.push (0, Top_level) state.stack;
    next_token state)
  else (
    ignore @@ Stack.pop state.stack;
    DEDENT)

let push_indent state level context =
  Stack.push (level, context) state.stack;
  prerr_endline
  @@ Printf.sprintf "push_indent, NOW %s"
       (show_indent_context @@ snd @@ Stack.top @@ state.stack)

let get_current_context state =
  match Stack.top_opt state.stack with
  | Some (_, ctx) -> ctx
  | None -> Top_level

let get_current_indent state =
  match Stack.top_opt state.stack with Some (level, _) -> level | None -> 0

let close_until state ident next_token =
  prerr_endline @@ Printf.sprintf "close_until ident %n " ident;
  let rec go () =
    match Stack.pop_opt state.stack with
    | None ->
        prerr_endline "Stack.pop_opt -> None";
        Stack.push (0, Top_level) state.stack
    | Some ((_, Let) as x) ->
        prerr_endline "Some ((_, Let) as x)";
        Stack.push x state.stack
    | Some ((_, Let_inline) as x) ->
        prerr_endline "Some ((_, Let_inline) as x)";
        Stack.push x state.stack;
        Queue.add DEDENT state.queue
    | Some ((_, If) as x) ->
        prerr_endline "Some ((_, If) as x)";
        Stack.push x state.stack
    | Some ((_, Type_alias) as x) ->
        prerr_endline "Some ((_, Type_alias) as x)";
        Stack.push x state.stack
    | Some ((_, Type_decl) as x) ->
        prerr_endline "Some ((_, Type_decl) as x)";
        Stack.push x state.stack
    | Some ((_, Type_annotation) as x) ->
        prerr_endline "Some ((_, Type_annotation) as x)";
        Stack.push x state.stack
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
    | Some (ident', Case_arm_expr) when ident = ident' ->
        Queue.add DEDENT state.queue
    | Some (ident', ctx) ->
        prerr_endline
        @@ Printf.sprintf "Some (ident', ctx), ident: %n, ident' : %n , ctx: %s"
             ident ident' (show_indent_context ctx);
        ()
  in
  go ();
  if Queue.is_empty state.queue then (
    prerr_endline "close_until, branch queue is empty";
    next_token state)
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

let handle_newline state nl token lexbuf =
  let indent_level = count_indent nl in

  prerr_endline @@ Printf.sprintf "handle_newline, indent level %n" indent_level;

  let pos = lexbuf.Lexing.lex_curr_p in

  state.current_string_cnum <- pos.Lexing.pos_cnum - count_indent nl;

  if state.need_dedent_after_type then (
    prerr_endline "Closing virtual type INDENT with DEDENT";
    state.need_dedent_after_type <- false;

    (match get_current_context state with
    | Type_annotation -> ignore @@ Stack.pop state.stack
    | _ -> ());
    Queue.add DEDENT state.queue);

  match (state.prev_token, Stack.top_opt state.stack) with
  | Some LET, Some (_, Let_inline) ->
      prerr_endline "let multiline";
      let i, _ = Stack.pop state.stack in
      push_indent state i Let;
      INDENT
  | Some COLON, Some (_, Top_level) when indent_level > 0 ->
      prerr_endline
        "After COLON with newline - creating Type_annotation context";
      push_indent state indent_level Type_annotation;
      INDENT
  | _ ->
      let rec go () =
        prerr_endline "Go called";
        let current = get_current_indent state in
        let next_token state = token state lexbuf in

        state.ident_compare <- Int.compare indent_level current;

        prerr_endline
        @@ Printf.sprintf "Current indent context value %n" current;
        prerr_endline
        @@ Printf.sprintf "indent_level < current, %b" (indent_level < current);

        match get_current_context state with
        | Top_level when indent_level = 0 ->
            prerr_endline "Top_level when indent_level = 0";
            token state lexbuf
        | Top_level ->
            prerr_endline "Top_level";
            close_until state indent_level next_token
        | Expression when indent_level < current ->
            prerr_endline "Expression when indent_level < current";
            close_one state next_token
        | Expression ->
            prerr_endline "Expression";
            next_token state
        | (Case | Let) when indent_level > current ->
            prerr_endline "(Case | Let), indent_level > current";
            let _, l = Stack.pop state.stack in
            push_indent state indent_level l;
            go ()
        | (Case | Let) when indent_level < current ->
            prerr_endline "Case when indent_level < current";
            close_until state indent_level next_token
        | Case | Let ->
            prerr_endline "Case | Let";
            next_token state
        | Type_alias when indent_level > current ->
            prerr_endline "Type_alias when indent_level > current";
            next_token state
        | Type_alias when indent_level < current ->
            prerr_endline "Type_alias when indent_level < current";
            close_until state indent_level next_token
        | Type_alias ->
            prerr_endline "Type_alias";
            next_token state
        | Type_decl when indent_level > current ->
            prerr_endline "Type_decl when indent_level > current";
            next_token state
        | Type_decl when indent_level < current ->
            prerr_endline "Type_decl when indent_level < current";
            close_until state indent_level next_token
        | Type_decl ->
            prerr_endline "Type_decl";
            next_token state
        | Type_annotation when indent_level = 0 ->
            prerr_endline "Type_annotation when indent_level = 0 - closing";
            ignore @@ Stack.pop state.stack;
            Queue.add (token state lexbuf) state.queue;
            DEDENT
        | Type_annotation when indent_level < current ->
            prerr_endline
              "Type_annotation when indent_level < current - closing";
            ignore @@ Stack.pop state.stack;
            Queue.add DEDENT state.queue;
            close_until state indent_level next_token
        | Type_annotation ->
            prerr_endline "Type_annotation - continuing";
            next_token state
        | Case_arm_expr when indent_level = current ->
            prerr_endline "Case_arm_expr";
            ignore @@ Stack.pop state.stack;
            DEDENT
        | Case_arm_expr when indent_level < current ->
            prerr_endline "Case_arm_expr when indent_level < current";
            close_until state indent_level next_token
        | Case_arm_expr ->
            prerr_endline "Case_arm_expr";
            next_token state
        | Let_def when indent_level < current ->
            prerr_endline "Let_def when indent_level < current";
            close_until state indent_level next_token
        | Let_def ->
            prerr_endline "Let_def";
            next_token state
        | Let_inline when indent_level < current ->
            prerr_endline "Let_inline when indent_level < current";
            DEDENT
        | Let_inline ->
            prerr_endline "Let_inline";
            next_token state
        | If when indent_level > current -> next_token state
        | If ->
            prerr_endline "If";
            next_token state
      in

      if Queue.is_empty state.queue then go ()
      else
        let result = Queue.take state.queue in
        prerr_endline
        @@ Printf.sprintf "Taking from queue: %s" (show_token result);
        result

let handle_equal state lexbuf =
  prerr_endline "handle_equal";
  state.need_dedent_after_type <- false;

  if get_current_context state = Top_level then (
    prerr_endline "handle_equal, Top_level branch";
    push_indent state 1 Expression;
    Queue.add INDENT state.queue);
  if get_current_context state = Let_def then Queue.add INDENT state.queue;
  if get_current_context state = Let_inline then Queue.add INDENT state.queue;
  if get_current_context state = Type_alias then (
    prerr_endline "handle_equal, Type_alias branch";
    Queue.add INDENT state.queue);
  if get_current_context state = Type_decl then (
    prerr_endline "handle_equal, Type_decl branch";
    Queue.add INDENT state.queue);
  EQUAL

let handle_colon state lexbuf =
  prerr_endline "handle_colon";
  match get_current_context state with
  | Top_level ->
      prerr_endline "handle_colon in Top_level";

      let pos = lexbuf.Lexing.lex_curr_pos in
      let has_newline_after =
        if pos < lexbuf.Lexing.lex_buffer_len then
          let rec check_whitespace i =
            if i >= lexbuf.Lexing.lex_buffer_len then false
            else
              match Bytes.get lexbuf.Lexing.lex_buffer i with
              | ' ' | '\t' -> check_whitespace (i + 1)
              | '\n' -> true
              | _ -> false
          in
          check_whitespace pos
        else false
      in

      if has_newline_after then (
        prerr_endline "Newline after colon - marking for type annotation";

        COLON)
      else (
        prerr_endline "Type on same line after colon - creating virtual INDENT";
        push_indent state 0 Type_annotation;
        state.need_dedent_after_type <- true;
        Queue.add INDENT state.queue;
        COLON)
  | Type_annotation ->
      prerr_endline "handle_colon in Type_annotation";
      COLON
  | _ -> COLON

let handle_case_of state lexbuf =
  prerr_endline
  @@ Printf.sprintf "handle_case_of, ident: %n" (get_current_indent state);
  state.need_dedent_after_type <- false;

  push_indent state (get_current_indent state) Case;
  Queue.add INDENT state.queue;
  OF

let handle_case state =
  state.case_opened <- true;
  state.need_dedent_after_type <- false;

  CASE

let handle_arrow state lexbuf =
  prerr_endline
  @@ Printf.sprintf "handle_arrow, ident: %n" (get_current_indent state);
  state.need_dedent_after_type <- false;

  if get_current_context state = Case then (
    push_indent state (get_current_indent state) Case_arm_expr;
    Queue.add INDENT state.queue);
  ARROW

let next_token state token lexbuf =
  prerr_endline
  @@ Printf.sprintf "next_token, lexeme: %s"
  @@ Lexing.lexeme lexbuf;
  let token =
    if Queue.is_empty state.queue then (
      let result = token state lexbuf in
      prerr_endline
      @@ Printf.sprintf "Queue is empty, result %s" (show_token result);
      result)
    else
      let result = Queue.take state.queue in
      prerr_endline
      @@ Printf.sprintf "Queue isn't empty, result %s" (show_token result);
      result
  in
  state.prev_token <- Some token;
  token

let handle_let state lexbuf =
  prerr_endline "handle let";
  state.need_dedent_after_type <- false;

  push_indent state
    (lexbuf.Lexing.lex_start_p.pos_cnum - state.current_string_cnum)
    Let_inline;
  LET

let handle_let_def state lexbuf =
  prerr_endline
  @@ Printf.sprintf "handle_let_def, %n" (get_current_indent state);

  if state.need_dedent_after_type then (
    prerr_endline "Need virtual DEDENT before function definition";
    state.need_dedent_after_type <- false;

    (match get_current_context state with
    | Type_annotation -> ignore @@ Stack.pop state.stack
    | _ -> ());
    Queue.add (LCNAME (Lexing.lexeme lexbuf)) state.queue;
    DEDENT)
  else if state.case_opened then (
    state.case_opened <- false;
    LCNAME (Lexing.lexeme lexbuf))
  else if get_current_context state = Type_alias then (
    prerr_endline "handle_let_def, Type_alias context - just return LCNAME";
    LCNAME (Lexing.lexeme lexbuf))
  else if get_current_context state = Type_decl then (
    prerr_endline "handle_let_def, Type_decl context - just return LCNAME";
    LCNAME (Lexing.lexeme lexbuf))
  else if get_current_context state = Type_annotation then (
    prerr_endline "handle_let_def, Type_annotation context - just return LCNAME";
    LCNAME (Lexing.lexeme lexbuf))
  else
    match get_current_context state with
    | Let ->
        prerr_endline "handle_let_def, let branch";
        push_indent state
          (lexbuf.Lexing.lex_start_p.pos_cnum - state.current_string_cnum)
          Let_def;
        LCNAME (Lexing.lexeme lexbuf)
    | Let_def when state.ident_compare = 0 ->
        prerr_endline "Let_def when state.ident_compare = 0";
        ignore @@ Stack.pop state.stack;
        push_indent state
          (lexbuf.Lexing.lex_start_p.pos_cnum - state.current_string_cnum + 1)
          Let_def;
        Queue.add (LCNAME (Lexing.lexeme lexbuf)) state.queue;
        DEDENT
    | Let_inline ->
        prerr_endline "let inline";
        prerr_endline
        @@ Printf.sprintf "new one, %n"
             (lexbuf.Lexing.lex_start_p.pos_cnum - state.current_string_cnum + 1);
        ignore @@ Stack.pop state.stack;
        push_indent state
          (lexbuf.Lexing.lex_start_p.pos_cnum - state.current_string_cnum + 1)
          Let_inline;
        LCNAME (Lexing.lexeme lexbuf)
    | _ -> LCNAME (Lexing.lexeme lexbuf)

let handle_lcname = handle_let_def

let handle_in state =
  prerr_endline "hanlde_in";
  state.need_dedent_after_type <- false;

  match get_current_context state with
  | Let_inline when state.prev_token = Some DEDENT ->
      prerr_endline "Let_inline when state.prev_token = Some DEDENT";
      ignore @@ Stack.pop state.stack;
      IN
  | Let_inline ->
      prerr_endline "Let_inline";
      ignore @@ Stack.pop state.stack;
      Queue.add IN state.queue;
      DEDENT
  | ctx ->
      prerr_endline
      @@ Printf.sprintf "handle_id_deafult_case, ctx %s"
           (show_indent_context ctx);
      if get_current_context state = Let_def then (
        prerr_endline "state = Let_def";
        ignore @@ Stack.pop state.stack;
        Queue.add DEDENT state.queue);
      if get_current_context state = Case_arm_expr then (
        prerr_endline "state = Let_def";
        ignore @@ Stack.pop state.stack;
        Queue.add DEDENT state.queue;
        ignore @@ Stack.pop state.stack;
        Queue.add DEDENT state.queue);
      ignore @@ Stack.pop state.stack;
      Queue.add IN state.queue;
      DEDENT

let handle_eof state =
  prerr_endline "handle_eof";

  if state.need_dedent_after_type then (
    prerr_endline "Closing virtual type INDENT at EOF";
    state.need_dedent_after_type <- false;
    (match get_current_context state with
    | Type_annotation -> ignore @@ Stack.pop state.stack
    | _ -> ());
    Queue.add DEDENT state.queue);

  let rec close_all_to_top () =
    match Stack.top_opt state.stack with
    | Some (_, Top_level) -> ()
    | Some _ ->
        ignore @@ Stack.pop state.stack;
        Queue.add DEDENT state.queue;
        close_all_to_top ()
    | None -> Stack.push (0, Top_level) state.stack
  in
  close_all_to_top ();
  Queue.add EOF state.queue;
  if Queue.is_empty state.queue then EOF else Queue.take state.queue

let handle_if state =
  prerr_endline "handle_if";
  state.need_dedent_after_type <- false;

  push_indent state (get_current_indent state) If;
  IF

let handle_else state =
  prerr_endline "handle_else";
  state.need_dedent_after_type <- false;

  ignore @@ Stack.pop state.stack;
  ELSE

let handle_type_alias state lexbuf =
  prerr_endline "handle_type_alias";
  state.need_dedent_after_type <- false;

  push_indent state 0 Type_alias;
  ()

let handle_type_decl state lexbuf =
  prerr_endline "handle_type_decl";
  state.need_dedent_after_type <- false;

  push_indent state 0 Type_decl;
  state.in_type_decl <- true;
  ()

let handle_alias state =
  prerr_endline "handle_alias";
  state.need_dedent_after_type <- false;

  if get_current_context state = Type_decl then (
    ignore @@ Stack.pop state.stack;
    push_indent state 0 Type_alias;
    state.in_type_decl <- false);
  ALIAS
