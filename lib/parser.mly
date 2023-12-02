%{ 
    open Ast 
%}

%token TYPE
%token ALIAS
%token <string> LCNAME
%token <string> UCNAME
%token <int> INT
%token <string> STRING
%token <int> SPACE
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

%token INDENT DEDENT

%nonassoc IN
%nonassoc ELSE
// %nonassoc ARROW

%left PLUS MINUS
%left TIMES DIV
// %nonassoc UMINUS


%start <Ast.elm_ast list> prog

%%
prog: 
    NEWLINE*; lst = separated_list(NEWLINE+, decls); EOF { lst }

decls:
    | d=ty_decl { d }
    | v=value_decl { v }

ty_decl:
    | TYPE ALIAS id=UCNAME params=list(LCNAME) ioption(NEWLINE) EQUAL data=ty_al_exp_head 
        { Type_alias({ data; id; params }) }
    | TYPE id=UCNAME params=list(LCNAME) EQUAL constrs=separated_nonempty_list(PIPE, ty_constrs_data) 
        { Type_dec({ id; constrs; params; })}

value_decl:
    | type_part_data=value_decl_type NEWLINE body_part=value_decl_body 
        { Declaration ({ type_part_data=Some(type_part_data); body_part }) }

value_decl_type:
    | decl_name=LCNAME COLON type_alias=ty_al_exp_head { { decl_name; type_alias;  } }  

value_decl_body:
    | id=LCNAME EQUAL value=value_decl_body_exprs_top { { name=id; expr=value;} }

value_decl_body_exprs_composite:
    | e1=value_decl_body_exprs_top b=value_decl_body_exprs_binop e2=value_decl_body_exprs_top { Binop({ op_id=b; params=(e1, e2) }) }
    | LBRACKET e=separated_list(COMMA, value_decl_body_exprs_top) RBRACKET { List_constr(e) }
    | LET items=separated_nonempty_list(NEWLINE+, value_decl_body_let_def)
      IN in_=value_decl_body_exprs_top { Let({let_expr_items=items; in_; }) }
    | IF if_exp=value_decl_body_exprs_top 
        THEN then_exp=value_decl_body_exprs_top
        ELSE else_exp=ioption(value_decl_body_exprs_top)
        { If_then_else({ if_exp; then_exp; else_exp; }) }
    | i=UCNAME params=list(value_decl_body_exprs) { Constr({ constr_name=i; params }) }
    | CASE expr=value_decl_body_exprs_top OF INDENT
      pattern_data_items=separated_nonempty_list(NEWLINE, value_decl_body_exprs_case) DEDENT
        { Case_of({ expr; pattern_data_items; })}


value_decl_body_exprs_case:
    | pattern=value_decl_body_exprs_pattern ARROW expr=value_decl_body_exprs_top
        { { pattern; expr; } }

value_decl_body_exprs_pattern:
    | i=STRING { PStr(i) }
    | i=INT { PInt(i) }
    | i=WILDCARD { PAnything }


value_decl_body_exprs_plain: 
    | LBRACE lst=separated_list(COMMA, value_decl_body_exprs_record) RBRACE { Record lst }
    | e=value_decl_body_exprs_ident { e }
    | e=value_decl_body_exprs { e }

value_decl_body_exprs_fn:
    | ident=value_decl_body_exprs_ident args=nonempty_list(value_decl_body_exprs_top_arguments) { Apply { ident; args; } }

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
    name=LCNAME EQUAL value=value_decl_body_exprs_top { {name; value} }

value_decl_body_exprs_ident:
    | ident=LCNAME { Ident ident }

value_decl_body_let_def:
    type_part=ioption(value_decl_body_let_def_type) body_part=value_decl_body_let_body
        { { type_part; body_part;} }

value_decl_body_let_def_type:
    name=LCNAME COLON content=ty_al_exp_head NEWLINE+ { { name; content; } }

value_decl_body_let_body:
    id=LCNAME EQUAL body=value_decl_body_exprs_top { { name=id; body; } }        

%inline
value_decl_body_exprs_binop:
    | PLUS { "+" }
    | MINUS { "-" }
    | DIV { "/" }
    | TIMES { "*" }

value_decl_body_exprs:
    | i=STRING { String_constr(i) }
    | i=INT { Int_constr(i) }
    | i=FLOAT { Float_constr(i) }

ty_constrs_data:
    | id=UCNAME data=list(ty_al_exp_roots) {{ id; data; }}

ty_al_exp_head:
    | NEWLINE e=ty_al_exp_head { e }
    | e=ty_al_exp { e }
    | fn=ty_al_exp_fun { fn }

ty_al_exp:
    | what=UCNAME params=nonempty_list(ty_al_exp_roots) { {content=Concrete(what); params} }
    | e=ty_al_exp_roots { e }

ty_al_exp_roots:
    | what=UCNAME { {content=Concrete(what); params=[]} }
    | what=LCNAME { {content=Type_var(what); params=[]} }
    | LBRACE e=ty_al_rec RBRACE { {content=Record(e); params=[]} }
    | LPAREN e=ty_al_exp_paren RPAREN { e }

ty_al_rec:
    | values=separated_list(COMMA, ty_al_exp_rec_data_lst) {{ values; row_type=None;  }}
    | row_type=ty_al_rec_row_ty values=separated_list(COMMA, ty_al_exp_rec_data_lst) {{ values; row_type=Some(row_type);  }}

ty_al_rec_row_ty:
    | what=LCNAME PIPE { what }

ty_al_exp_paren:
    |  e1=ty_al_exp_head e2=preceded(COMMA, separated_nonempty_list(COMMA, ty_al_exp_head)) { {content=Tuples(e1::e2); params=[]} }
    |  fn=ty_al_exp_fun { fn }
    |  e=ty_al_exp { e }

ty_al_exp_fun:
    |  e1=ty_al_exp e2=preceded(ARROW, separated_nonempty_list(ARROW, ty_al_exp))
         { {content=Function({ arguments=(e1::e2) }); params=[]} }

ty_al_exp_rec_data_lst:
    | key_name=LCNAME COLON type_name=ty_al_exp_head { {key=key_name; value=type_name} }
