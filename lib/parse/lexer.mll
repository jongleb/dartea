{
  open Parser

  type raw = Token of token | Newline of string | Skip

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
  | ('\n' [' ' '\t']*)+ as nl { Newline nl }
  | whitespace      { Skip }
  | "type"          { Token TYPE }
  | "alias"         { Token ALIAS }
  | "case"          { Token CASE }
  | "of"            { Token OF }
  | "let"           { Token LET }
  | "if"            { Token IF }
  | "then"          { Token (THEN) }
  | "else"          { Token ELSE }
  | "in"            { Token IN }
  | "exposing"      { Token (EXPOSING) }
  | "module"        { Token (MODULE) }
  | "import"        { Token (IMPORT) }
  | "as"            { Token (AS) }
  | lcname          { Token (LCNAME (Lexing.lexeme lexbuf)) }
  | ucname          { Token (UCNAME (Lexing.lexeme lexbuf)) }
  | ucname_q        { Token (UCNAME_PATH (Lexing.lexeme lexbuf)) }
  | ucname_q '.' lcname { Token (QUAL_LCNAME (Lexing.lexeme lexbuf)) }
  | '='             { Token EQUAL }
  | eof             { Token EOF }
  | "("             { Token (LPAREN) }
  | ")"             { Token (RPAREN) } 
  | "{"             { Token LBRACE }
  | "}"             { Token RBRACE }
  | "["             { Token (LBRACKET) }
  | "]"             { Token (RBRACKET) }
  | ","             { Token (COMMA) }
  | ":"             { Token COLON }
  | "|"             { Token (PIPE) }
  | "\\"            { Token (BACKSLASH) }
  | "->"            { Token ARROW }
  | "|>"            { Token (PIPE_GT) }
  | "++"            { Token (CONCAT) }
  | "+"             { Token (PLUS) }
  | "-"             { Token (MINUS) }
  | "_"             { Token (WILDCARD) }
  | "*"             { Token (TIMES) }
  | "::"            { Token (CONS) }
  | ".."            { Token (TWO_DOTS) }
  | "/"             { Token (DIV) }
  | "()"            { Token (UNIT) }
  | "=="            { Token (EQ_EQ) }
  | "/="            { Token (NOT_EQ) }
  | ">="            { Token (GT_EQ) }
  | "<="            { Token (LT_EQ) }
  | "&&"            { Token (AND) }
  | "||"            { Token (OR) }
  | ">"             { Token (GT) }
  | "<"             { Token (LT) }
  | int             { Token (INT (int_of_string (Lexing.lexeme lexbuf))) }
  | float           { Token (FLOAT (float_of_string(Lexing.lexeme lexbuf))) }
  | '"'             { Token (STRING (string "" lexbuf)) }
  | "." (lcname as n)      { Token (ACCESS n) }
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


