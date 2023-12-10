%{ 
    open Ast
    open Data
%}

%token TYPE
%token ALIAS
%token <string> LCNAME
%token <string> UCNAME
%token <string> UCNAME_PATH
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
%token COLON
%token PIPE
%token ARROW
%token NEWLINE
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
%token CONS
%token UNIT
%token IMPORT
%token AS
%token TWO_DOTS
%token EXPOSING

%token INDENT DEDENT

%nonassoc IN
%nonassoc ELSE
// %nonassoc ARROW

%left PLUS MINUS
%left TIMES DIV
// %nonassoc UMINUS


%start <Ast.Impl.t list> prog

// import Html
//     import Html as H
//     import Html as H exposing (..)
//     import Html exposing (Html, div, text)

%%
prog: 
    lst = top_decls; 
    EOF { lst }

top_decls:
    | x=import xs=top_decls { List.concat [[x]; xs;] }
    | NEWLINE xs=top_decls { xs }
    | xs=separated_list(NEWLINE+, decls) { xs }     

upper_possible_dotted:
    | what=UCNAME { what }
    | what=UCNAME_PATH { what }

loc(X):
    what=X { Located.mk what $loc }    

import:
    | IMPORT name=loc(upper_possible_dotted) alias=ioption(preceded(AS, UCNAME))
      exposing_opt=ioption(preceded(EXPOSING, import_exposing)) 
      { 
        let exposing = Option.value ~default:(Import_thing.Explicit []) exposing_opt in
        Impl.Import (Import_thing.{ name; alias; exposing;  })
      }

import_exposing:
    | LPAREN TWO_DOTS RPAREN { Import_thing.Open }
    | LPAREN lst=separated_nonempty_list(COMMA, import_exposing_item) RPAREN { Import_thing.Explicit lst }

import_exposing_item:
    | name=loc(UCNAME) { Import_thing.Upper { name; privacy=Private } }
    | name=loc(LCNAME) { Import_thing.Upper { name; privacy=Private } }
    | name=loc(UCNAME) LPAREN TWO_DOTS RPAREN { Import_thing.Upper { name; privacy=Public($loc) } }    

decls:
    | d=ty_decl { d }
    | v=value_decl { v }

ty_decl:
    | TYPE ALIAS name=UCNAME params=list(LCNAME) ioption(NEWLINE) EQUAL typedef=ty_al_exp_head 
        { Impl.Type_alias({ typedef; name; params }) }
    | TYPE name=UCNAME params=list(LCNAME) EQUAL ctors=separated_nonempty_list(PIPE, ty_constrs_data) 
        { Impl.Type_dec({ name; ctors; params; })}

value_decl:
    | type_part_data=ioption(terminated(value_decl_type, NEWLINE)) body_part=value_decl_body 
        { Impl.Top_declaration ({ type_part_data; body_part }) }

value_decl_type:
    | name=LCNAME COLON type_alias=ty_al_exp_head { Declaration.{ name; type_alias; } }  

value_decl_body:
    | name=LCNAME EQUAL expr=value_decl_body_exprs_top {Declaration.{ name; expr; }}

value_decl_body_exprs_composite:
    | e1=value_decl_body_exprs_top name=value_decl_body_exprs_binop e2=value_decl_body_exprs_top { Expr_binop({ name; operands=(e1, e2) }) }
    | LBRACKET e=separated_list(COMMA, value_decl_body_exprs_top) RBRACKET { Expr_list(e) }
    | LET bindings=separated_nonempty_list(NEWLINE+, value_decl_body_let_def)
      IN body=value_decl_body_exprs_top { Expr_let { bindings; body; } }
    | IF if_exp=value_decl_body_exprs_top 
        THEN then_exp=value_decl_body_exprs_top
        ELSE else_exp=ioption(value_decl_body_exprs_top)
        { Expr_if_then_else({ if_exp; then_exp; else_exp; }) }
    | name=UCNAME arguments=list(value_decl_body_exprs) { Expr_constr({ name; arguments }) }
    | CASE expr=value_decl_body_exprs_top OF INDENT
      pattern_data_items=separated_nonempty_list(NEWLINE, value_decl_body_exprs_case) DEDENT
        { Expr_pattern({ expr; pattern_data_items; })}


value_decl_body_exprs_case:
    | pattern=value_decl_body_exprs_pattern_top ARROW expr=value_decl_body_exprs_top
        {{ pattern; expr; }}

value_decl_body_exprs_pattern_top:
    | e=value_decl_body_exprs_pattern_composite { e }
    | e=value_decl_body_exprs_pattern_plain { e }

value_decl_body_exprs_pattern_composite:
    | e=value_decl_body_exprs_pattern_p_ctor { e }
    | a=value_decl_body_exprs_pattern_plain CONS b=value_decl_body_exprs_pattern_plain { P_cons(a, b) }

value_decl_body_exprs_pattern_p_ctor:
    | name=UCNAME lst=list(value_decl_body_exprs_pattern_p_ctor_body) { Pattern.P_ctor(name, lst) }

value_decl_body_exprs_pattern_p_ctor_body:
    | LPAREN e=value_decl_body_exprs_pattern_composite RPAREN { e }
    | e=value_decl_body_exprs_pattern_plain { e }

value_decl_body_exprs_pattern_plain:
    | i=STRING { P_str(i) }
    | i=INT { P_int(i) }
    | i=WILDCARD { P_anything }
    | i=LCNAME { P_var(i) }
    | LBRACE lst=separated_list(COMMA, LCNAME) RBRACE { P_record(lst) }
    | LBRACKET lst=separated_list(COMMA, value_decl_body_exprs_pattern_top) RBRACKET { Pattern.P_list(lst) }

value_decl_body_exprs_plain: 
    | LBRACE lst=separated_list(COMMA, value_decl_body_exprs_record) RBRACE { Expr_record lst }
    | e=value_decl_body_exprs_ident { e }
    | e=value_decl_body_exprs { e }

value_decl_body_exprs_fn:
    | ident=value_decl_body_exprs_ident args=nonempty_list(value_decl_body_exprs_top_arguments) { Expr_apply { ident; args; } }

value_decl_body_exprs_top:
    | e=value_decl_body_exprs_fn { e }
    | e=value_decl_body_exprs_plain { e }
    | e=value_decl_body_exprs_composite { e }

value_decl_body_exprs_top_arguments:
    | LPAREN e=value_decl_body_exprs_top_arguments_composite RPAREN { e }
    | e=value_decl_body_exprs_plain { e }

value_decl_body_exprs_top_arguments_composite:
    | e=value_decl_body_exprs_composite { e }
    | e=value_decl_body_exprs_fn { e }

value_decl_body_exprs_record:
    name=LCNAME EQUAL value=value_decl_body_exprs_top {Expr.{ name; value }}

value_decl_body_exprs_ident:
    | ident=LCNAME { Expr_ident ident }

value_decl_body_let_def:
    bind_type=ioption(value_decl_body_let_def_type) bind_body=value_decl_body_let_body
        {Expr.{ bind_type; bind_body;}}

value_decl_body_let_def_type:
    name=LCNAME COLON content=ty_al_exp_head NEWLINE+ {Expr.{ name; content; }}

value_decl_body_let_body:
    id=LCNAME EQUAL body=value_decl_body_exprs_top {{ name=id; body; }}        

%inline
value_decl_body_exprs_binop:
    | PLUS { "+" }
    | MINUS { "-" }
    | DIV { "/" }
    | TIMES { "*" }

value_decl_body_exprs:
    | i=STRING { Expr_string i }
    | i=INT { Expr_int i }
    | i=FLOAT { Expr_Float i }

ty_constrs_data:
    | id=UCNAME data=list(ty_al_exp_roots) {{ id; data; }}

ty_al_exp_head:
    | NEWLINE e=ty_al_exp_head { e }
    | e=ty_al_exp { e }
    | fn=ty_al_exp_fun { fn }

ty_al_exp:
    | what=upper_possible_dotted parameters=nonempty_list(ty_al_exp_roots) { Typedef.{body=Tkind_concrete(what); parameters} }
    | e=ty_al_exp_roots { e }

ty_al_exp_roots:
    | what=upper_possible_dotted {{ body=Tkind_concrete(what); parameters=[] }}
    | UNIT { {body=Tkind_unit; parameters=[]} }
    | what=LCNAME { {body=Tkind_var(what); parameters=[]} }
    | LBRACE e=ty_al_rec RBRACE { Typedef.{body=Tkind_record(e); parameters=[]} }
    | LPAREN e=ty_al_exp_paren RPAREN { e }

ty_al_rec:
    | row_type=ioption(ty_al_rec_row_ty) values=separated_list(COMMA, ty_al_exp_rec_data_lst) { Typedef.{ values; row_type;  } }

ty_al_rec_row_ty:
    | what=LCNAME PIPE { what }

ty_al_exp_paren:
    |  e1=ty_al_exp_head e2=preceded(COMMA, separated_nonempty_list(COMMA, ty_al_exp_head)) {{ Typedef.body=Tkind_tuple(e1::e2); parameters=[] }}
    |  fn=ty_al_exp_fun { fn }
    |  e=ty_al_exp { e }

ty_al_exp_fun:
    |  e1=ty_al_exp e2=preceded(ARROW, separated_nonempty_list(ARROW, ty_al_exp))
         { Typedef.{body=Tkind_function({ arguments=(e1::e2) }); parameters=[] }}

ty_al_exp_rec_data_lst:
    | name=LCNAME COLON body=ty_al_exp_head {{ name; body; }}
