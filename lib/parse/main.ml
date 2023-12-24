open Lexer

let parse lexbuf =
  let queue = Queue.create () in

  Parser.prog
    (fun i ->
      if Queue.length queue > 0 then Queue.take queue
      else
        let q = i |> Lexer.token |> List.to_seq |> Queue.of_seq in
        Queue.transfer q queue;
        Queue.take queue)
    lexbuf

let main () =
  let lexbuf = Lexing.from_channel stdin in
  try
    let cst = parse lexbuf in
    print_endline "Succesfully parsed";
    List.iter
      (fun i ->
        i |> Format.asprintf "%a" Ast.Kind.Frontend.Impl.pp |> print_endline)
      cst
  with
  | Failure msg -> print_endline ("Failure --- " ^ msg)
  | Parsing.Parse_error -> print_endline "Parse error"
  | End_of_file -> print_endline "Parse error: unexpected end of string"

let parse () =
  let lexbuf = Lexing.from_channel stdin in
  parse lexbuf
