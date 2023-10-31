%{ 
    open Ast 
%}

%token TYPE
%token ALIAS
%token <string> TYPE_NAME
%token <string> TYPE_VARIABLE
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

%start <Ast.elm_ast list> prog

%%

prog: 
    lst = list(ty_al_decl); EOF { lst }

ty_al_decl:
    | TYPE ALIAS id=TYPE_NAME params=list(TYPE_VARIABLE) EQUAL data=ty_al_exp_head 
        { Type_alias({ data; id; params }) }
    | TYPE id=TYPE_NAME params=list(TYPE_VARIABLE) EQUAL constrs=separated_nonempty_list(PIPE, ty_constrs_data) 
        { Type_dec({ id; constrs; params; })}

ty_constrs_data:
    | id=TYPE_NAME data=list(ty_al_exp_roots) {{ id; data; }}

ty_al_exp_head:
    | e=ty_al_exp { e }
    | fn=ty_al_exp_fun { fn }

ty_al_exp:
    | what=TYPE_NAME params=nonempty_list(ty_al_exp_roots) { {content=Concrete(what); params} }
    | e=ty_al_exp_roots { e }

ty_al_exp_roots:
    | what=TYPE_NAME { {content=Concrete(what); params=[]} }
    | what=TYPE_VARIABLE { {content=Type_var(what); params=[]} }
    | LBRACE e=ty_al_rec RBRACE { {content=Record(e); params=[]} }
    | LPAREN e=ty_al_exp_paren RPAREN { e }

ty_al_rec:
    | values=separated_list(COMMA, ty_al_exp_rec_data_lst) {{ values; row_type=None;  }}
    | row_type=ty_al_rec_row_ty values=separated_list(COMMA, ty_al_exp_rec_data_lst) {{ values; row_type=Some(row_type);  }}

ty_al_rec_row_ty:
    | what=TYPE_VARIABLE PIPE { what }

ty_al_exp_paren:
    |  e1=ty_al_exp_head e2=preceded(COMMA, separated_nonempty_list(COMMA, ty_al_exp_head)) { {content=Tuples(e1::e2); params=[]} }
    |  fn=ty_al_exp_fun { fn }
    |  e=ty_al_exp { e }

ty_al_exp_fun:
    |  e1=ty_al_exp e2=preceded(ARROW, separated_nonempty_list(ARROW, ty_al_exp))
         { {content=Function({ arguments=(e1::e2) }); params=[]} }

ty_al_exp_rec_data_lst:
    | key_name=TYPE_VARIABLE COLON type_name=ty_al_exp_head { {key=key_name; value=type_name} }
