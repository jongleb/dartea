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

let parse content =
  let lexbuf = Lexing.from_string content in
  try
    let cst = parse lexbuf in
    Ok cst
  with _ -> Error "parse error"
