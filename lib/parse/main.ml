open Lexer
open Indenter_level

let parse state lexbuf = Parser.prog (Lexer.token state) lexbuf

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
    let cst = parse state lexbuf in
    Ok cst
  with e -> Error e
