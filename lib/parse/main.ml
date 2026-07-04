let parse content =
  let state = Indenter.initial_state () in
  let lexbuf = Lexing.from_string content in
  try
    let cst = Parser.prog (Lexer.token state) lexbuf in
    Printf.printf "Successfully parsed, AST length: %d\n" (List.length cst);
    Ok cst
  with e ->
    Printf.printf "Parse error: %s\n" (Printexc.to_string e);
    Error e
