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
%token PIPE_GT
%token UMINUS
%token CONS
%token BACKSLASH
%token TYPE
%token ALIAS
%token COLON
%token PIPE
%token CONCAT

%nonassoc ELSE IN
%nonassoc ARROW

%left UMINUS
%left PIPE_GT
%left PLUS MINUS CONCAT
%left TIMES DIV
%left EQ_EQ
%left GT LT

%type <Ast.Kind.Frontend.Expr.t> expr
%type <Ast.Kind.Frontend.Typedef.Impl.t> type_expr
%type <Ast.Kind.Frontend.Typedef.Kind.t> type_atom_no_parens
%type <Ast.Kind.Frontend.Typedef.Impl.t> type_in_parens
%type <Ast.Kind.Frontend.Typedef.Impl.t> type_in_parens_content

%start <Ast.Kind.Frontend.Impl.t list> prog
%%

prog:
    m=ioption(module_)
    lst = top_decls;
    EOF { List.concat [Option.value ~default:[] m; lst] }

top_decls: l=list(top_decl) { l }

top_decl:
    | d=value_decl_with_type { d }
    | d=type_alias_decl { d }
    | d=type_decl { d }

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


type_alias_decl:
    | TYPE ALIAS name=loc(UCNAME) params=list(loc(LCNAME)) EQUAL typedef=indented(type_expr)
        { Impl.Type_alias { name; params; typedef } }


type_decl:
    | TYPE name=UCNAME params=list(LCNAME) EQUAL ctors=indented(type_constructors)
        { Impl.Type_dec { Typedecl.name; params; ctors } }

type_constructors:
    | first=type_constructor rest=list(type_constructor_pipe)
        { first :: rest }

type_constructor:
    | id=UCNAME data=list(type_constructor_arg)
        { { Typedecl.id; data } }

type_constructor_arg:
    | t=type_atom_no_parens { { Typedef.Impl.parameters = []; body = t } }
    | t=type_in_parens { t }

type_constructor_pipe:
    | PIPE ctor=type_constructor { ctor }


type_expr:
    | t=type_function { t }

type_function:
    | args=separated_nonempty_list(ARROW, type_app)
        { 
          match args with
          | [single] -> single
          | _ -> { Typedef.Impl.parameters = []; 
                   body = Typedef.Kind.Tkind_function { Typedef.Type_function.arguments = args } }
        }

type_app:
    | t=type_atom_no_parens { { Typedef.Impl.parameters = []; body = t } }
    | t=type_in_parens { t }
    | base=type_app arg=type_atom_no_parens 
        { 
          { Typedef.Impl.parameters = base.Typedef.Impl.parameters @ [{ Typedef.Impl.parameters = []; body = arg }];
            body = base.Typedef.Impl.body }
        }
    | base=type_app arg=type_in_parens
        { 
          { Typedef.Impl.parameters = base.Typedef.Impl.parameters @ [arg];
            body = base.Typedef.Impl.body }
        }

type_in_parens:
    | LPAREN t=type_in_parens_content RPAREN { t }

type_in_parens_content:
    |  { { Typedef.Impl.parameters = []; body = Typedef.Kind.Tkind_unit } }
    | t=type_function { t }
    | t=type_function COMMA rest=separated_nonempty_list(COMMA, type_function)
        { 
          let all_args = t :: rest in
          { Typedef.Impl.parameters = []; body = Typedef.Kind.Tkind_tuple all_args }
        }

type_atom_no_parens:
    | name=loc(LCNAME) { Typedef.Kind.Tkind_var name }
    | name=loc(upper_possible_dotted) { Typedef.Kind.Tkind_concrete name }
    | UNIT { Typedef.Kind.Tkind_unit }
    | LBRACE fields=type_record_fields RBRACE
        { fields }

type_record_fields:
    | row=LCNAME PIPE field=type_record_field rest=type_record_fields_rest
        { 
          let fields = field :: rest in
          Typedef.Kind.Tkind_record { Typedef.Type_record.values = fields; row_type = Some (Located.mk row $loc(row)) } 
        }
    | field=type_record_field rest=type_record_fields_rest
        { 
          let fields = field :: rest in
          Typedef.Kind.Tkind_record { Typedef.Type_record.values = fields; row_type = None } 
        }

type_record_fields_rest:
    |  { [] }
    | COMMA field=type_record_field rest=type_record_fields_rest { field :: rest }

type_record_field:
    | name=LCNAME COLON body=type_expr
        { { Typedef.Type_record_row.name = Located.mk name $loc(name); body } }

value_decl_with_type:
    | name1=loc(LCNAME) COLON INDENT type_alias=type_expr DEDENT
      name2=loc(LCNAME) params=list(loc(LCNAME)) EQUAL expr=indented(loc(expr))
        { 
          let type_part_data = Some Declaration.{ name=name1; type_alias } in
          let body_part = Declaration.{ name=name2; expr; params } in
          Impl.Top_declaration { type_part_data; body_part }
        }
    | name=loc(LCNAME) params=list(loc(LCNAME)) EQUAL expr=indented(loc(expr))
        { 
          let body_part = Declaration.{ name; expr; params } in
          Impl.Top_declaration { type_part_data=None; body_part }
        }
   
expr:    
    | e=expr_pipe { e }
    | MINUS e=expr %prec UMINUS { Expr_unop { name = Located.mk "-" $loc; operand = e } }
    | e=expr_app { e }
    | e=expr_binop { e }
    | BACKSLASH params=nonempty_list(loc(LCNAME)) ARROW body=expr %prec ARROW
        { Expr_lambda { params; body } }
    | IF if_exp=expr THEN then_exp=expr ELSE else_exp=expr
        { Expr_if_then_else { if_exp; then_exp; else_exp } }
    | CASE expr=scrutinee OF pattern_data_items=indented(list(case_arm))
        { Expr_pattern { expr; pattern_data_items } }
    | LET binding=expr_let_name_bind IN e=expr
        { make_expr_let ~bindings:[binding] e }
    | LET INDENT bindings=expr_let_defs DEDENT IN e=expr
        { make_expr_let ~bindings e }

expr_pipe:
    | arg=expr PIPE_GT fn=expr { Expr_apply { fn; arg } }

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
    | head=pattern_atom CONS tail=pattern { P_cons(head, tail) }
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
    | CONCAT { "++" }
    | EQ_EQ { "==" }
    | GT { ">" }
    | LT { "<" }

indented(X): INDENT x=X DEDENT { x }
