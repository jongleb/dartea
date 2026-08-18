{
  open Parser

  type raw = Token of token | Line_start of int | Skip

  let get = Lexing.lexeme

  let region lexbuf =
    Data.Region.of_lexing (lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)

  let reject lexbuf problem =
    raise (Reporting.Error.Found (Reporting.Error.syntax ~region:(region lexbuf) problem))

  let reject_from start lexbuf problem =
    let region = Data.Region.of_lexing (start, lexbuf.Lexing.lex_curr_p) in
    raise (Reporting.Error.Found (Reporting.Error.syntax ~region problem))

  let token_start lexbuf = lexbuf.Lexing.lex_start_p

  let position lexbuf = lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum

  let advance_lines lexbuf lexeme =
    let base = lexbuf.Lexing.lex_start_p.Lexing.pos_cnum in
    String.iteri
      (fun index character ->
        if character = '\n' then
          let current = lexbuf.Lexing.lex_curr_p in
          lexbuf.Lexing.lex_curr_p <-
            { current with
              Lexing.pos_lnum = current.Lexing.pos_lnum + 1;
              Lexing.pos_bol = base + index + 1 })
      lexeme

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
  | ('\n' [' ' '\t']*)+ as nl { advance_lines lexbuf nl; Line_start (line_start lexbuf nl) }
  | whitespace      { Skip }
  | "--" [^ '\n']* { Skip }
  | "{-"           { block_comment (token_start lexbuf) 1 None lexbuf }
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
  | "\"\"\""       { let start = token_start lexbuf in
                      Token (STRING (from_token_start lexbuf (block_string start ""))) }
  | '"'             { let start = token_start lexbuf in
                      Token (STRING (from_token_start lexbuf (string start ""))) }
  | '\''            { let start = token_start lexbuf in
                      Token (CHAR (from_token_start lexbuf (character start))) }
  | "." (lcname as n)      { Token (ACCESS n) }
  | _               { reject lexbuf (Reporting.Syntax_error.Unknown_character { found = Lexing.lexeme lexbuf }) }

  and block_comment start depth opened_line = parse
  | "{-"                { block_comment start (depth + 1) opened_line lexbuf }
  | "-}"                { if depth > 1 then block_comment start (depth - 1) opened_line lexbuf
                          else
                            match opened_line with
                            | None -> Skip
                            | Some start -> Line_start start
                        }
  | '\n'                { advance_lines lexbuf (Lexing.lexeme lexbuf);
                          block_comment start depth (Some (position lexbuf)) lexbuf }
  | [^ '{' '-' '\n']+   { block_comment start depth opened_line lexbuf }
  | eof                 { reject_from start lexbuf (Reporting.Syntax_error.Unterminated { what = Block_comment }) }
  | _                   { block_comment start depth opened_line lexbuf }

  and block_string start acc = parse
  | "\"\"\""            { acc }
  | '\\'                { let esc = escaped lexbuf in block_string start (acc ^ esc) lexbuf }
  | [^'"' '\\']+        { advance_lines lexbuf (Lexing.lexeme lexbuf);
                          block_string start (acc ^ (Lexing.lexeme lexbuf)) lexbuf }
  | '"'                 { block_string start (acc ^ "\"") lexbuf }
  | eof                 { reject_from start lexbuf (Reporting.Syntax_error.Unterminated { what = Block_text }) }

  and character start = parse
  | '\''                { reject_from start lexbuf Reporting.Syntax_error.Empty_character }
  | '\\'                { let esc = escaped lexbuf in closing_quote start esc lexbuf }
  | [^'\'' '\\' '\x80'-'\xff']
                        { closing_quote start (Lexing.lexeme lexbuf) lexbuf }
  | utf8_character      { closing_quote start (Lexing.lexeme lexbuf) lexbuf }
  | eof                 { reject_from start lexbuf (Reporting.Syntax_error.Unterminated { what = Character }) }
  | _                   { reject_from start lexbuf Reporting.Syntax_error.Empty_character }

  and closing_quote start value = parse
  | '\''                { value }
  | eof                 { reject_from start lexbuf (Reporting.Syntax_error.Unterminated { what = Character }) }
  | _                   { reject_from start lexbuf Reporting.Syntax_error.Crowded_character }

  and string start acc = parse
  | '"'                 { acc }
  | '\\'                { let esc = escaped lexbuf in string start (acc ^ esc) lexbuf }
  | [^'"' '\\']*        { advance_lines lexbuf (Lexing.lexeme lexbuf);
                          string start (acc ^ (Lexing.lexeme lexbuf)) lexbuf }
  | eof                 { reject_from start lexbuf (Reporting.Syntax_error.Unterminated { what = Text }) }
  
  and escaped = parse
  | 'u' '{' (hex_digit+ as code) '}'
                        { utf_8_encoded (int_of_string ("0x" ^ code)) }
  | _                   { let str = Lexing.lexeme lexbuf in
                          try List.assoc str escaped_characters
                          with Not_found ->
                            reject lexbuf (Reporting.Syntax_error.Unknown_escape { found = str }) 
                        }


