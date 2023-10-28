open Lexer

let hello () =
  let lexbuf = Lexing.from_channel stdin in
  try
    let cst = Parser.prog Lexer.token lexbuf in
    print_endline "Succesfully parsed";
    List.iter
      (fun i -> i |> Format.asprintf "%a" Ast.pp_elm_ast |> print_endline)
      cst
  with
  | Failure msg -> print_endline ("Failure --- " ^ msg)
  | Parsing.Parse_error -> print_endline "Parse error"
  | End_of_file -> print_endline "Parse error: unexpected end of string"
