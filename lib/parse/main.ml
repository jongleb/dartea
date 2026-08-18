let parse ~file content =
  let lexbuf = Lexing.from_string content in
  Lexing.set_filename lexbuf file;
  let state = ref Indenter.initial in
  let next_token lexbuf =
    let token, updated = Indenter.next_token !state lexbuf in
    state := updated;
    token
  in
  let region () =
    Data.Region.of_lexing (lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)
  in
  match Parser.prog next_token lexbuf with
  | parsed -> Ok parsed
  | exception Parser.Error ->
      Error
        (Reporting.Error.syntax ~region:(region ())
           (Reporting.Syntax_error.Unexpected_input
              { found = Lexing.lexeme lexbuf }))
  | exception Ast.Kind.Frontend.Expr.Too_many_tuple_parts { given; region } ->
      Error
        (Reporting.Error.syntax ~region
           (Reporting.Syntax_error.Too_many_tuple_parts { given }))
  | exception Reporting.Error.Found error -> Error error
