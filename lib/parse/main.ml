open Lexer
open Indenter_level

let parse lexbuf = Parser.prog Lexer.token lexbuf

let parse content =
  let lexbuf = Lexing.from_string content in
  try
    let cst = parse lexbuf in
    Ok cst
  with e -> Error e
