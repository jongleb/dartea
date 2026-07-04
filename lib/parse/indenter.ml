open Parser

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

type indent_state = {
  stack : (int * indent_context) Stack.t;
  queue : token Queue.t;
  mutable line_start_cnum : int;
  mutable last_indent_cmp : int;
  mutable prev_token : token option;
  mutable case_opened : bool;
  mutable need_dedent_after_type : bool;
}

let initial_state () =
  let state =
    {
      stack = Stack.create ();
      queue = Queue.create ();
      line_start_cnum = 0;
      last_indent_cmp = 0;
      prev_token = None;
      case_opened = false;
      need_dedent_after_type = false;
    }
  in
  Stack.push (0, Top_level) state.stack;
  state

let push state column context = Stack.push (column, context) state.stack
let pop state = ignore (Stack.pop state.stack)
let enqueue state token = Queue.add token state.queue

let current_frame state =
  Option.value (Stack.top_opt state.stack) ~default:(0, Top_level)

let current_context state = snd (current_frame state)
let current_column state = fst (current_frame state)

let dedent state =
  pop state;
  DEDENT

let queue_dedent state =
  pop state;
  enqueue state DEDENT

let token_column state lexbuf =
  lexbuf.Lexing.lex_start_p.pos_cnum - state.line_start_cnum

let close_until state target next_token =
  let rec go () =
    match Stack.pop_opt state.stack with
    | None -> Stack.push (0, Top_level) state.stack
    | Some ((_, (Let | If | Type_alias | Type_decl | Type_annotation)) as frame)
      ->
        Stack.push frame state.stack
    | Some ((_, Let_inline) as frame) ->
        Stack.push frame state.stack;
        enqueue state DEDENT
    | Some (column, _) when target < column ->
        enqueue state DEDENT;
        go ()
    | Some ((column, _) as frame) when target > column ->
        Stack.push frame state.stack
    | Some (_, Case_arm_expr) -> enqueue state DEDENT
    | Some ((_, Let_def) as frame) -> Stack.push frame state.stack
    | Some _ -> ()
  in
  go ();
  if Queue.is_empty state.queue then next_token state else Queue.take state.queue

let indentation_width lexeme =
  String.fold_left (fun n c -> if c = '\n' then n else n + 1) 0 lexeme

let handle_newline state nl token lexbuf =
  let indent_level = indentation_width nl in
  let pos = lexbuf.Lexing.lex_curr_p in
  state.line_start_cnum <- pos.Lexing.pos_cnum - indent_level;

  match (state.prev_token, Stack.top_opt state.stack) with
  | Some COLON, Some (_, Top_level) when indent_level > 0 ->
      push state indent_level Type_annotation;
      INDENT
  | Some LET, Some (_, Let_inline) ->
      let column, _ = Stack.pop state.stack in
      push state column Let;
      INDENT
  | _, Some (_, (Type_alias | Type_decl)) when indent_level = 0 -> dedent state
  | _ ->
      let rec go () =
        let column = current_column state in
        let next_token state = token state lexbuf in
        state.last_indent_cmp <- Int.compare indent_level column;

        match current_context state with
        | Top_level when indent_level = 0 -> next_token state
        | Top_level -> close_until state indent_level next_token
        | Expression when indent_level < column -> dedent state
        | Expression -> next_token state
        | (Case | Let) when indent_level > column ->
            let _, ctx = Stack.pop state.stack in
            push state indent_level ctx;
            go ()
        | (Case | Let) when indent_level < column ->
            close_until state indent_level next_token
        | Case | Let -> next_token state
        | (Type_alias | Type_decl) when indent_level > column -> next_token state
        | (Type_alias | Type_decl) when indent_level < column ->
            queue_dedent state;
            close_until state indent_level next_token
        | Type_alias | Type_decl -> dedent state
        | Type_annotation when indent_level = 0 -> next_token state
        | Type_annotation when indent_level < column ->
            queue_dedent state;
            close_until state indent_level next_token
        | Type_annotation -> next_token state
        | Case_arm_expr when indent_level = column -> dedent state
        | Case_arm_expr when indent_level < column ->
            close_until state indent_level next_token
        | Case_arm_expr -> next_token state
        | Let_def when indent_level < column ->
            close_until state indent_level next_token
        | Let_def -> next_token state
        | Let_inline when indent_level < column -> DEDENT
        | Let_inline -> next_token state
        | If -> next_token state
      in
      if Queue.is_empty state.queue then go () else Queue.take state.queue

let handle_equal state _lexbuf =
  state.need_dedent_after_type <- false;
  state.last_indent_cmp <- -1;
  (match current_context state with
  | Top_level ->
      push state 1 Expression;
      enqueue state INDENT
  | Let_def | Let_inline | Type_alias | Type_decl -> enqueue state INDENT
  | _ -> ());
  EQUAL

let rec newline_follows lexbuf i =
  if i >= lexbuf.Lexing.lex_buffer_len then false
  else
    match Bytes.get lexbuf.Lexing.lex_buffer i with
    | ' ' | '\t' -> newline_follows lexbuf (i + 1)
    | '\n' -> true
    | _ -> false

let handle_colon state lexbuf =
  match current_context state with
  | Top_level ->
      if newline_follows lexbuf lexbuf.Lexing.lex_curr_pos then COLON
      else (
        push state 0 Type_annotation;
        state.need_dedent_after_type <- true;
        enqueue state INDENT;
        COLON)
  | _ -> COLON

let handle_case_of state _lexbuf =
  state.need_dedent_after_type <- false;
  push state (current_column state) Case;
  enqueue state INDENT;
  OF

let handle_case state =
  state.case_opened <- true;
  state.need_dedent_after_type <- false;
  CASE

let handle_arrow state _lexbuf =
  state.need_dedent_after_type <- false;
  if current_context state = Case then (
    push state (current_column state) Case_arm_expr;
    enqueue state INDENT);
  ARROW

let next_token state token lexbuf =
  let token =
    if Queue.is_empty state.queue then token state lexbuf
    else Queue.take state.queue
  in
  state.prev_token <- Some token;
  token

let handle_let state lexbuf =
  state.need_dedent_after_type <- false;
  push state (token_column state lexbuf) Let_inline;
  LET

let handle_lcname state lexbuf =
  let name = LCNAME (Lexing.lexeme lexbuf) in
  let col = token_column state lexbuf in

  if current_context state = Type_annotation && col = 0 then (
    state.need_dedent_after_type <- false;
    enqueue state name;
    dedent state)
  else if current_context state = Type_annotation then name
  else if state.need_dedent_after_type then (
    state.need_dedent_after_type <- false;
    match current_context state with
    | Type_annotation ->
        enqueue state name;
        dedent state
    | _ -> name)
  else if state.case_opened then (
    state.case_opened <- false;
    name)
  else if current_context state = Type_alias || current_context state = Type_decl
  then name
  else
    match current_context state with
    | Let ->
        push state col Let_def;
        name
    | Let_def when state.last_indent_cmp = 0 ->
        pop state;
        push state (col + 1) Let_def;
        enqueue state name;
        DEDENT
    | Let_inline ->
        pop state;
        push state (col + 1) Let_inline;
        name
    | _ -> name

let handle_in state =
  state.need_dedent_after_type <- false;
  match current_context state with
  | Let_inline when state.prev_token = Some DEDENT ->
      pop state;
      IN
  | Let_inline ->
      enqueue state IN;
      dedent state
  | _ ->
      if current_context state = Let_def then queue_dedent state;
      if current_context state = Case_arm_expr then (
        queue_dedent state;
        queue_dedent state);
      enqueue state IN;
      dedent state

let handle_eof state =
  (if state.need_dedent_after_type then (
     state.need_dedent_after_type <- false;
     match current_context state with
     | Type_annotation -> queue_dedent state
     | _ -> ()));

  let rec close_all () =
    match Stack.top_opt state.stack with
    | Some (_, Top_level) -> ()
    | Some _ ->
        queue_dedent state;
        close_all ()
    | None -> Stack.push (0, Top_level) state.stack
  in
  close_all ();

  if not (Queue.fold (fun seen t -> seen || t = EOF) false state.queue) then
    enqueue state EOF;
  if Queue.is_empty state.queue then EOF else Queue.take state.queue

let handle_if state =
  state.need_dedent_after_type <- false;
  push state (current_column state) If;
  IF

let handle_else state =
  state.need_dedent_after_type <- false;
  pop state;
  ELSE

let handle_type_decl state _lexbuf =
  state.need_dedent_after_type <- false;
  push state 0 Type_decl

let handle_alias state =
  state.need_dedent_after_type <- false;
  if current_context state = Type_decl then (
    pop state;
    push state 0 Type_alias);
  ALIAS
