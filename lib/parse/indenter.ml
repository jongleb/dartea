open Parser

type context =
  | Top_level
  | Expression
  | Let
  | Let_binding
  | Let_inline
  | If
  | Case
  | Case_head
  | Case_arm
  | Type_alias
  | Type_decl
  | Type_annotation
  | Delimited

type scope = { column : int; context : context; bracketed : bool }

type t = {
  scopes : scope list;
  pending : token list;
  line_start : int;
  aligned : bool;
  prev_token : token option;
}

let initial =
  { scopes = []; pending = []; line_start = 0; aligned = true; prev_token = None }

let current state =
  match state.scopes with
  | [] -> { column = 0; context = Top_level; bracketed = false }
  | scope :: _ -> scope

let context state = (current state).context
let column state = (current state).column
let debt scopes = List.length (List.filter (fun scope -> scope.bracketed) scopes)

let via change state =
  let scopes = change state.scopes in
  let delta = debt scopes - debt state.scopes in
  ( List.init (abs delta) (fun _ -> if delta > 0 then INDENT else DEDENT),
    { state with scopes } )

let head change = function [] -> [] | scope :: rest -> change scope :: rest
let ( let* ) (tokens, state) step =
  let more, state = step state in
  (tokens @ more, state)

let ( +> ) token (tokens, state) = (token :: tokens, state)

let mark ~column ~context state =
  { state with scopes = { column; context; bracketed = false } :: state.scopes }

let move ~column state =
  { state with scopes = head (fun scope -> { scope with column }) state.scopes }

let retag ~context state =
  { state with scopes = head (fun scope -> { scope with context }) state.scopes }

let push ~column ~context state =
  via (List.cons { column; context; bracketed = true }) state

let bracket state = via (head (fun scope -> { scope with bracketed = true })) state
let unbracket state = via (head (fun scope -> { scope with bracketed = false })) state
let pop state = via (function [] -> [] | _ :: rest -> rest) state

let close_through stops token state =
  let rec drop = function
    | scope :: rest when stops scope.context -> rest
    | scope :: rest when scope.context <> Top_level -> drop rest
    | scopes -> scopes
  in
  let* state = via drop state in
  ([ token ], state)

type reaction = Keep | Close | Unbracket | Realign | Unwind

let react context ~indent ~column =
  match (context, compare indent column) with
  | Type_annotation, _ when indent = 0 -> Keep
  | Expression, -1 -> Close
  | Let, 1 -> Realign
  | Case, 1 -> Realign
  | Case, -1 -> Unwind
  | (Type_alias | Type_decl), 0 -> Close
  | (Type_alias | Type_decl | Type_annotation), -1 -> Unwind
  | Case_arm, 0 -> Close
  | Case_arm, -1 -> Unwind
  | Let_binding, -1 -> Unwind
  | Let_inline, -1 -> Unbracket
  | _ -> Keep

let rec layout state ~indent =
  let column = column state in
  let state = { state with aligned = indent = column } in
  match react (context state) ~indent ~column with
  | Keep -> ([], state)
  | Close -> pop state
  | Unbracket -> unbracket state
  | Realign -> layout (move ~column:indent state) ~indent
  | Unwind ->
      let* state = pop state in
      layout state ~indent

let indentation_width lexeme =
  match String.rindex_opt lexeme '\n' with
  | None -> String.length lexeme
  | Some last_newline -> String.length lexeme - last_newline - 1

let token_column state lexbuf =
  lexbuf.Lexing.lex_start_p.Lexing.pos_cnum - state.line_start

let handle_newline state newline lexbuf =
  let indent = indentation_width newline in
  let state =
    { state with line_start = lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum - indent }
  in
  match (state.prev_token, context state) with
  | Some INDENT, Type_annotation ->
      if indent > 0 then ([], move ~column:indent state) else pop state
  | Some LET, Let_inline -> bracket (retag ~context:Let state)
  | _ -> layout state ~indent

let handle state lexbuf token =
  match token with
  | EQUAL ->
      let state = { state with aligned = false } in
      begin
        match context state with
        | Top_level -> EQUAL +> push ~column:1 ~context:Expression state
        | Let_binding | Let_inline | Type_alias | Type_decl ->
            EQUAL +> bracket state
        | _ -> ([ EQUAL ], state)
      end
  | COLON when context state = Top_level ->
      COLON +> push ~column:0 ~context:Type_annotation state
  | CASE -> ([ CASE ], mark ~column:(column state) ~context:Case_head state)
  | OF ->
      let* state = if context state = Case_head then pop state else ([], state) in
      OF +> push ~column:(column state) ~context:Case state
  | ARROW when context state = Case ->
      ARROW +> push ~column:(column state) ~context:Case_arm state
  | LET -> ([ LET ], mark ~column:(token_column state lexbuf) ~context:Let_inline state)
  | IF -> ([ IF ], mark ~column:(column state) ~context:If state)
  | ELSE -> ELSE +> pop state
  | TYPE -> ([ TYPE ], mark ~column:0 ~context:Type_decl state)
  | ALIAS when context state = Type_decl ->
      ([ ALIAS ], retag ~context:Type_alias state)
  | LBRACE -> ([ LBRACE ], mark ~column:(column state) ~context:Delimited state)
  | RBRACE -> close_through (function Delimited -> true | _ -> false) RBRACE state
  | IN -> close_through (function Let | Let_inline -> true | _ -> false) IN state
  | EOF -> close_through (fun _ -> false) EOF state
  | LCNAME _ ->
      let name_column = token_column state lexbuf in
      begin
        match context state with
        | Type_annotation when name_column = 0 ->
            let* state = pop state in
            ([ token ], state)
        | Let -> ([ token ], mark ~column:name_column ~context:Let_binding state)
        | Let_binding when state.aligned ->
            let* state = unbracket state in
            ([ token ], move ~column:(name_column + 1) state)
        | Let_inline -> ([ token ], move ~column:(name_column + 1) state)
        | _ -> ([ token ], state)
      end
  | token -> ([ token ], state)

let rec next_token state lexbuf =
  match state.pending with
  | token :: rest -> (token, { state with pending = rest; prev_token = Some token })
  | [] ->
      let pending, state =
        match Lexer.token lexbuf with
        | Lexer.Skip -> ([], state)
        | Lexer.Newline newline -> handle_newline state newline lexbuf
        | Lexer.Token token -> handle state lexbuf token
      in
      next_token { state with pending } lexbuf
