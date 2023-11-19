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

  (* let update *)
}

let ucname = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let lcname = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let newline = '\r' | '\n' | "\r\n"

let int = ['0'-'9'] ['0'-'9' '_']*

let space_or_tab = [' ' '\t']*

let float =
  '-'? ['0'-'9'] ['0'-'9' '_']*
  (('.' ['0'-'9' '_']*) (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)? |
   ('.' ['0'-'9' '_']*)? (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*))


rule token = parse
  | "type"          { TYPE }
  | "alias"         { ALIAS }
  | "case"           { CASE }
  | "of"            { OF }
  | "let"           { LET }
   | "if"          { IF }
  | "then"         { THEN }
  | "else"           { ELSE }
  | "in"           { IN }
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
  | "["             {LBRACKET}
  | "]"             {RBRACKET}
  | ","             { COMMA }
  | ":"             { COLON }
  | "|"             { PIPE }
  | "->"            { ARROW }
  | "+"       { PLUS }
  | "-"       { MINUS }
  | "_"       { WILDCARD }
  | "*"        {TIMES}
  | "/"        {DIV}
  | int             { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float               { FLOAT (float_of_string(Lexing.lexeme lexbuf)) }
  | '"'                 { STRING (string "" lexbuf) }
  | "\r\n"           { Lexing.new_line lexbuf; token lexbuf;}
  | '\n'        { Lexing.new_line lexbuf; NEWLINE }
  | [' ' '\t']      { token lexbuf;  } (* temporary ignore it *)
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
