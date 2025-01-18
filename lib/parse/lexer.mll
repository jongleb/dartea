{
  open Parser
  let get = Lexing.lexeme

  exception Error of string

  let escaped_characters = [
    ("\"", "\"");
    ("\\", "\\");
    ("\'", "'");
    ("n", "\n");
    ("t", "\t");
    ("b", "\b");
    ("r", "\r");
    (" ", " ");
  ]
}

let ucname = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let ucname_q = ucname ('.' ucname)*

let lcname = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let whitespace = [' ' '\t']

let indent = '\n' [' ' '\t']*

let int = ['0'-'9'] ['0'-'9' '_']*
let float =
  '-'? ['0'-'9'] ['0'-'9' '_']*
  (('.' ['0'-'9' '_']*) (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)? |
   ('.' ['0'-'9' '_']*)? (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*))


rule token = parse
  | '\n'+ [' ' '\t']* as nl { Indenter.handle_newline nl token lexbuf } 
  | whitespace      { token lexbuf }
  | "type"          { TYPE }
  | "alias"         { ALIAS }
  | "case"          { CASE }
  | "of"            { Indenter.handle_case_of lexbuf }
  | "let"           { Indenter.handle_let lexbuf }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "in"            { Indenter.handle_in() }
  | "import"        { IMPORT }
  | "exposing"      { EXPOSING }
  | "as"            { AS }
  | "module"        { MODULE }
  | lcname          { Indenter.handle_let_def lexbuf }
  | ucname          { UCNAME (Lexing.lexeme lexbuf) }
  | ucname_q        { UCNAME_PATH (Lexing.lexeme lexbuf) }
  | '='             { Indenter.handle_equal lexbuf }
  | eof             { Indenter.handle_eof () }
  | "("             { LPAREN }
  | ")"             { RPAREN } 
  | "{"             { LBRACE }
  | "}"             { RBRACE }
  | "["             { LBRACKET }
  | "]"             { RBRACKET }
  | ","             { COMMA }
  | ":"             { COLON }
  | "|"             { PIPE }
  | "->"            { Indenter.handle_arrow lexbuf }
  | "+"             { PLUS }
  | "-"             { MINUS }
  | "_"             { WILDCARD }
  | "*"             { TIMES }
  | "::"            { CONS }
  | ".."            { TWO_DOTS }
  | "."             { DOT }
  | "/"             { DIV }
  | "()"            { UNIT }
  | "=="            { EQ_EQ }
  | ">"             { GT }
  | "<"             { LT }
  | whitespace "." lcname { ACCESSOR (String.sub (Lexing.lexeme lexbuf) 2 (String.length (Lexing.lexeme lexbuf) - 2)) }
  | int             { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float           { FLOAT (float_of_string(Lexing.lexeme lexbuf)) }
  | '"'             { STRING (string "" lexbuf) }
  | _               { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

  and string acc = parse
  | '"'                 { acc }
  | '\\'                { let esc = escaped lexbuf in string (acc ^ esc) lexbuf }
  | [^'"' '\\']*        { string (acc ^ (Lexing.lexeme lexbuf)) lexbuf }
  | eof                 { raise (Error "STRINGEOC ERROR") }
  
  and escaped = parse
  | _                   { let str = Lexing.lexeme lexbuf in
                          try List.assoc str escaped_characters
                          with Not_found -> raise (Error "ESCAPED NOT_FOUND") 
                        }

{ let token = Indenter.next_token token }
