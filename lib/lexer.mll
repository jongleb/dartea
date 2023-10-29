{
  open Parser
  let get = Lexing.lexeme

  exception Error of string
}

let type_name = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let type_variable = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  | "type"          { TYPE }
  | "alias"         { ALIAS }
  | type_variable   { TYPE_VARIABLE (Lexing.lexeme lexbuf) }
  | type_name       { TYPE_NAME (Lexing.lexeme lexbuf) }
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
  | [' ' '\t' '\n'] { token lexbuf } (* temporary ignore it *)
  | _               { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
