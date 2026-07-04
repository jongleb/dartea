open OUnit2
open Parse
open Data.Located

let parse state lexbuf = Parser.expr (Lexer.token state) lexbuf

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
    }
  in
  let lexbuf = Lexing.from_string content in
  try
    let cst = parse state lexbuf in
    Ok cst
  with e -> Error e

let parse_access _ =
  assert_equal
    (Frontend.Expr.Expr_access { expr = Expr_ident "a"; field = ~?"b" })
    (Result.get_ok (parse "a.b"))

let suite = [ "parse_access" >:: parse_access ]
