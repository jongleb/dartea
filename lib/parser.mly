%{ 
    open Ast 
%}

%token TYPE
%token ALIAS
%token <string> LCNAME
%token <string> UCNAME
// %token <string> DECL_NAME
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

%start <Ast.elm_ast list> prog

%%
prog: 
    lst = list(decls); EOF { lst }

decls:
    | d=ty_al_decl NEWLINE { d }  
    | v=value_decl NEWLINE { v }

ty_al_decl:
    | TYPE ALIAS id=UCNAME params=list(LCNAME) EQUAL data=ty_al_exp_head 
        { Type_alias({ data; id; params }) }
    | TYPE id=UCNAME params=list(LCNAME) EQUAL constrs=separated_nonempty_list(PIPE, ty_constrs_data) 
        { Type_dec({ id; constrs; params; })}

value_decl:
    | type_part_data=ioption(value_decl_type) body_part=value_decl_body 
        { Declaration ({ type_part_data; body_part }) }

value_decl_type:
    | decl_name=LCNAME COLON type_alias=ty_al_exp_head NEWLINE { { decl_name; type_alias;  } }  

value_decl_body:
    | id=LCNAME EQUAL id2=LCNAME { id } 

ty_constrs_data:
    | id=UCNAME data=list(ty_al_exp_roots) {{ id; data; }}

ty_al_exp_head:
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
