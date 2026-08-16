{
  open Parser

  type raw = Token of token | Line_start of int | Skip

  let get = Lexing.lexeme

  exception Error of string

  let position lexbuf = lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum

  let from_token_start lexbuf scan =
    let start = lexbuf.Lexing.lex_start_p in
    let value = scan lexbuf in
    lexbuf.Lexing.lex_start_p <- start;
    value

  let line_start lexbuf lexeme =
    match String.rindex_opt lexeme '\n' with
    | None -> position lexbuf
    | Some last -> position lexbuf - (String.length lexeme - last - 1)

  let utf_8_encoded code =
    let buffer = Buffer.create 4 in
    Buffer.add_utf_8_uchar buffer (Uchar.of_int code);
    Buffer.contents buffer

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
let utf8_tail = ['\x80'-'\xbf']
let utf8_character =
    ['\x00'-'\x7f']
  | ['\xc2'-'\xdf'] utf8_tail
  | ['\xe0'-'\xef'] utf8_tail utf8_tail
  | ['\xf0'-'\xf4'] utf8_tail utf8_tail utf8_tail
let indent = '\n' [' ' '\t']*
let int = ['0'-'9'] ['0'-'9' '_']*
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let hexadecimal = "0x" hex_digit (hex_digit | '_')*
let float =
  ['0'-'9'] ['0'-'9' '_']*
  (('.' ['0'-'9' '_']*) (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)? |
   ('.' ['0'-'9' '_']*)? (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*))

rule token = parse
  | ('\n' [' ' '\t']*)+ as nl { Line_start (line_start lexbuf nl) }
  | whitespace      { Skip }
  | "--" [^ '\n']* { Skip }
  | "{-"           { block_comment 1 None lexbuf }
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
  | "<|"            { Token (APPLY_L) }
  | "<<"            { Token (COMPOSE_L) }
  | ">>"            { Token (COMPOSE_R) }
  | "++"            { Token (CONCAT) }
  | "//"            { Token (IDIV) }
  | "^"             { Token (POW) }
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
  | hexadecimal     { Token (INT (int_of_string (Lexing.lexeme lexbuf))) }
  | int             { Token (INT (int_of_string (Lexing.lexeme lexbuf))) }
  | float           { Token (FLOAT (float_of_string(Lexing.lexeme lexbuf))) }
  | "\"\"\""       { Token (STRING (from_token_start lexbuf (block_string ""))) }
  | '"'             { Token (STRING (from_token_start lexbuf (string ""))) }
  | '\''            { Token (CHAR (from_token_start lexbuf character)) }
  | "." (lcname as n)      { Token (ACCESS n) }
  | _               { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

  and block_comment depth opened_line = parse
  | "{-"                { block_comment (depth + 1) opened_line lexbuf }
  | "-}"                { if depth > 1 then block_comment (depth - 1) opened_line lexbuf
                          else
                            match opened_line with
                            | None -> Skip
                            | Some start -> Line_start start
                        }
  | '\n'                { block_comment depth (Some (position lexbuf)) lexbuf }
  | [^ '{' '-' '\n']+   { block_comment depth opened_line lexbuf }
  | eof                 { raise (Error "Unterminated block comment.") }
  | _                   { block_comment depth opened_line lexbuf }

  and block_string acc = parse
  | "\"\"\""            { acc }
  | '\\'                { let esc = escaped lexbuf in block_string (acc ^ esc) lexbuf }
  | [^'"' '\\']+        { block_string (acc ^ (Lexing.lexeme lexbuf)) lexbuf }
  | '"'                 { block_string (acc ^ "\"") lexbuf }
  | eof                 { raise (Error "Unterminated block string.") }

  and character = parse
  | '\\'                { let esc = escaped lexbuf in closing_quote esc lexbuf }
  | [^'\'' '\\' '\x80'-'\xff']
                        { closing_quote (Lexing.lexeme lexbuf) lexbuf }
  | utf8_character      { closing_quote (Lexing.lexeme lexbuf) lexbuf }
  | eof                 { raise (Error "Unterminated character literal.") }
  | _                   { raise (Error "Empty character literal.") }

  and closing_quote value = parse
  | '\''                { value }
  | eof                 { raise (Error "Unterminated character literal.") }
  | _                   { raise (Error "A character literal holds one character.") }

  and string acc = parse
  | '"'                 { acc }
  | '\\'                { let esc = escaped lexbuf in string (acc ^ esc) lexbuf }
  | [^'"' '\\']*        { string (acc ^ (Lexing.lexeme lexbuf)) lexbuf }
  | eof                 { raise (Error "STRINGEOC ERROR") }
  
  and escaped = parse
  | 'u' '{' (hex_digit+ as code) '}'
                        { utf_8_encoded (int_of_string ("0x" ^ code)) }
  | _                   { let str = Lexing.lexeme lexbuf in
                          try List.assoc str escaped_characters
                          with Not_found -> raise (Error "ESCAPED NOT_FOUND") 
                        }


