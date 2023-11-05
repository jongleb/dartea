{
  open Parser
  let get = Lexing.lexeme

  exception Error of string
}

let ucname = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let lcname = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let newline = ('\r' | '\n' | "\r\n")*

rule token = parse
  | "type"          { TYPE }
  | "alias"         { ALIAS }
  | lcname          { 
                      let result = Lexing.lexeme lexbuf in
                      if result = "type" then 
                        raise (Error "LCNAME ERROR") 
                      else LCNAME result
                    }
  | ucname          { UCNAME (Lexing.lexeme lexbuf) }
  | '='             { EQUAL }
  | eof             { EOF }
  | "("             { LPAREN }
  | ")"             { RPAREN }
  | "{"             { LBRACE }
  | "}"             { RBRACE }
  | ","             { COMMA }
  | ":"             { COLON }
  | "|"             { PIPE }
  | "->"            { ARROW }
  | newline         { Lexing.new_line lexbuf; NEWLINE }
  | [' ' '\t']      { token lexbuf } (* temporary ignore it *)
  | _               { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
