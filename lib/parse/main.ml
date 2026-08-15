let parse content =
  let lexbuf = Lexing.from_string content in
  let state = ref Indenter.initial in
  let next_token lexbuf =
    let token, updated = Indenter.next_token !state lexbuf in
    state := updated;
    token
  in
  try
    let cst = Parser.prog next_token lexbuf in
    Printf.printf "Successfully parsed, AST length: %d\n" (List.length cst);
    Ok cst
  with e ->
    Printf.printf "Parse error: %s\n" (Printexc.to_string e);
    Error e
