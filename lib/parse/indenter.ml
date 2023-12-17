open Parser

let stack = Stack.create ()
let () = Stack.push 0 stack

let handle_stack len =
  let current = Stack.pop stack in
  if current = len then (
    Stack.push current stack;
    [ Parser.NEWLINE ])
  else if len > current then (
    Stack.push current stack;
    Stack.push len stack;
    [ Parser.INDENT ])
  else [ Parser.DEDENT; Parser.NEWLINE ]

let rec on_eof () =
  let _ = Stack.pop stack in
  if Stack.length stack = 0 then (
    (*FIXME it's hack need to unlock tests, think about it*)
    Stack.push 0 stack;
    [ Parser.EOF ])
  else List.concat [ [ Parser.DEDENT ]; on_eof () ]
