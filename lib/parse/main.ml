let parse content =
  let open Indenter in
  let state =
    {
      stack = Stack.create ();
      queue = Queue.create ();
      current_string_cnum = 0;
      ident_compare = 0;
      prev_token = None;
      case_opened = false;
      in_type_decl = false;
      need_dedent_after_type = false;
    }
  in
  Stack.push (0, Top_level) state.stack;
  let lexbuf = Lexing.from_string content in
  try
    let cst = Parser.prog (Lexer.token state) lexbuf in
    Printf.printf "Successfully parsed, AST length: %d\n" (List.length cst);
    Ok cst
  with e ->
    Printf.printf "Parse error: %s\n" (Printexc.to_string e);
    Error e
