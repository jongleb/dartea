%{
    open Ast.Kind.Frontend
    open Data
    open Expr
    open Pattern
%}

%token <string> LCNAME
%token <string> UCNAME
%token <string> UCNAME_PATH
%token <string> ACCESS
%token <int> INT
%token <string> STRING
%token <float> FLOAT
%token EQUAL
%token EOF
%token LPAREN
%token RPAREN 
%token LBRACE
%token RBRACE
%token COMMA
%token ARROW
%token LBRACKET
%token RBRACKET
%token LET
%token IN
%token PLUS
%token MINUS
%token TIMES
%token DIV
%token IF
%token THEN
%token ELSE
%token CASE
%token OF
%token WILDCARD
%token UNIT
%token TWO_DOTS
%token EXPOSING
%token EQ_EQ GT LT
%token MODULE
%token INDENT DEDENT

%nonassoc ELSE IN
%left PLUS MINUS
%left TIMES DIV
%left EQ_EQ
%left GT LT

%type <Ast.Kind.Frontend.Expr.t> expr


%start <Ast.Kind.Frontend.Impl.t list> prog
%%

prog:
    m=ioption(module_)
    lst = top_decls;
    EOF { List.concat [Option.value ~default:[] m; lst] }

top_decls: l=list(value_decl) { l }

upper_possible_dotted:
    | what=UCNAME { what }
    | what=UCNAME_PATH { what }

loc(X):
    what=X { Located.mk what $loc }

module_:
    | MODULE name=loc(upper_possible_dotted) EXPOSING exposing=exposing
        { [Impl.ModuleName name; Impl.Export exposing] }       

exposing:
    | LPAREN TWO_DOTS RPAREN { Exposing.Open }
    | LPAREN lst=separated_nonempty_list(COMMA, exposing_item) RPAREN { Exposing.Explicit lst }

exposing_item:
    | name=loc(UCNAME) { Exposing.Upper { name; privacy=Private } }
    | name=loc(LCNAME) { Exposing.Upper { name; privacy=Private } }
    | name=loc(UCNAME) LPAREN TWO_DOTS RPAREN { Exposing.Upper { name; privacy=Public($loc) } }    

value_decl:
    | body_part=value_decl_body { Impl.Top_declaration { type_part_data=None; body_part } }

value_decl_body:
    | name=loc(LCNAME) params=list(loc(LCNAME)) EQUAL expr=indented(loc(expr))
        { Declaration.{ name; expr; params } }
   
expr:    
    | e=expr_app { e }
    | e=expr_binop { e }
    | IF if_exp=expr THEN then_exp=expr ELSE else_exp=expr
        { Expr_if_then_else { if_exp; then_exp; else_exp } }
    | CASE expr=scrutinee OF pattern_data_items=indented(list(case_arm))
        { Expr_pattern { expr; pattern_data_items } }
    | LET binding=expr_let_name_bind IN e=expr
        { make_expr_let ~bindings:[binding] e }
    | LET INDENT bindings=expr_let_defs DEDENT IN e=expr
        { make_expr_let ~bindings e }

expr_let_name_bind:
    name=loc(LCNAME) EQUAL INDENT body=expr DEDENT
        {{ bind_type = None; bind_body={ name; body } }}

expr_let_defs: lst=nonempty_list(expr_let_name_bind) { lst }

scrutinee:
    | e=expr_app { e }
    | e=expr_binop { e }

case_arm:
    | pattern=pattern ARROW expr=indented(expr)
        {{ pattern; expr }}    

pattern:
    | name=UCNAME args=nonempty_list(pattern_atom) { P_ctor(name, args) }
    | p=pattern_atom { p }

pattern_atom:
    | i=STRING { P_str i }
    | i=INT { P_int i }
    | i=WILDCARD { P_anything }
    | i=LCNAME { P_var i }
    | name=UCNAME { P_ctor(name, []) }
    | LBRACE lst=separated_list(COMMA, LCNAME) RBRACE { P_record(lst) }
    | LBRACKET lst=separated_list(COMMA, pattern) RBRACKET { P_list(lst) }
    | LPAREN p=pattern RPAREN { p }

expr_binop:
    e1=expr name=binop e2=expr { Expr_binop { name; operands=(e1, e2) } }

expr_app:
    | e=expr_postfix { e }
    | fn=expr_app arg=expr_postfix { Expr_apply { fn; arg } }

expr_postfix:
    | base=expr_applicable fields=list(loc(ACCESS)) {
        List.fold_left (fun acc field -> Expr_access { expr = acc; field }) base fields
    }

expr_applicable: 
    | LPAREN e=expr RPAREN { e }
    | e=LCNAME { Expr_ident e }
    | e=STRING { Expr_string e }
    | e=INT { Expr_int e }
    | e=FLOAT { Expr_float e }
    | e=UNIT { Expr_unit }
    | n=UCNAME { Expr_constr_fixed n }
    | LBRACKET e=separated_list(COMMA, expr) RBRACKET { Expr_list e }
    | LBRACE lst=separated_list(COMMA, expr_record_field) RBRACE { Expr_record lst }

expr_record_field:
    name=LCNAME EQUAL value=expr {{ name; value }}

%inline
binop:
    | PLUS { "+" }
    | MINUS { "-" }
    | DIV { "/" }
    | TIMES { "*" }
    | EQ_EQ { "==" }
    | GT { ">" }
    | LT { "<" }

indented(X): INDENT x=X DEDENT { x }
