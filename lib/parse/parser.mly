%{
    open Ast.Kind.Frontend
    open Data
    open Expr
    open Pattern

    let located loc value = Located.at (Region.of_lexing loc) value
%}

%token <string> LCNAME
%token <string> UCNAME
%token <string> UCNAME_PATH
%token <string> QUAL_LCNAME
%token <string> ACCESS
%token <string> ACCESSOR
%token <int> INT
%token <string> STRING
%token <string> CHAR
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
%token IMPORT
%token AS
%token EQ_EQ GT LT NOT_EQ GT_EQ LT_EQ AND OR
%token MODULE
%token INDENT DEDENT
%token PIPE_GT
%token APPLY_L
%token COMPOSE_L
%token COMPOSE_R
%token IDIV
%token POW
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
%nonassoc AS

%right APPLY_L
%left PIPE_GT
%right OR
%right AND
%nonassoc EQ_EQ NOT_EQ GT LT GT_EQ LT_EQ
%right CONCAT CONS
%left PLUS MINUS
%left TIMES DIV IDIV
%right POW
%left COMPOSE_L
%right COMPOSE_R
%nonassoc UMINUS

%type <Ast.Kind.Frontend.Expr.t> expr
%type <Ast.Kind.Frontend.Typedef.Impl.t> type_expr
%type <Ast.Kind.Frontend.Typedef.Kind.t> type_atom_no_parens
%type <Ast.Kind.Frontend.Typedef.Impl.t> type_in_parens
%type <Ast.Kind.Frontend.Typedef.Impl.t> type_in_parens_content

%start <Ast.Kind.Frontend.Impl.t list> prog
%%

prog:
    m=ioption(module_)
    imports = list(import_)
    lst = top_decls;
    EOF { List.concat [Option.value ~default:[] m; imports; lst] }

top_decls: l=list(top_decl) { l }

top_decl:
    | d=value_decl_with_type { d }
    | d=type_alias_decl { d }
    | d=type_decl { d }

upper_possible_dotted:
    | what=UCNAME { what }
    | what=UCNAME_PATH { what }

loc(X):
    what=X { located $loc what }

module_:
    | MODULE name=loc(upper_possible_dotted) EXPOSING exposing=exposing
        { [Impl.ModuleName name; Impl.Export exposing] }

import_:
    | IMPORT name=loc(upper_possible_dotted) alias=ioption(import_alias) exp=ioption(import_exposing)
        { Impl.Import { name; alias;
                        exposing = Option.value ~default:(Exposing.Explicit []) exp } }

import_alias:
    | AS name=UCNAME { name }

import_exposing:
    | EXPOSING e=exposing { e }

exposing:
    | LPAREN TWO_DOTS RPAREN { Exposing.Open }
    | LPAREN lst=separated_nonempty_list(COMMA, exposing_item) RPAREN { Exposing.Explicit lst }

exposing_item:
    | name=loc(UCNAME) { Exposing.Upper { name; privacy=Private } }
    | name=loc(LCNAME) { Exposing.Lower name }
    | name=loc(UCNAME) LPAREN TWO_DOTS RPAREN { Exposing.Upper { name; privacy=Public (Region.of_lexing $loc) } }


type_alias_decl:
    | TYPE ALIAS name=loc(UCNAME) params=list(loc(LCNAME)) EQUAL typedef=indented(type_expr)
        { Impl.Type_alias { name; params; typedef } }


type_decl:
    | TYPE name=loc(UCNAME) params=list(LCNAME) EQUAL ctors=indented(type_constructors)
        { Impl.Type_dec { Typedecl.name; params; ctors } }

type_constructors:
    | first=type_constructor rest=list(type_constructor_pipe)
        { first :: rest }

type_constructor:
    | id=loc(UCNAME) data=list(type_constructor_arg)
        { { Typedecl.id; data } }

type_constructor_arg:
    | t=type_atom_no_parens { { Typedef.Impl.parameters = []; body = t } }
    | t=type_in_parens { t }

type_constructor_pipe:
    | PIPE ctor=type_constructor { ctor }


type_expr:
    | t=type_function { t }

type_function:
    | t=type_app { t }
    | first=type_app ARROW rest=arrow_chain
        { let arguments, result = rest in
          { Typedef.Impl.parameters = [];
            body = Typedef.Kind.Tkind_function
                     { Typedef.Type_function.arguments = first :: arguments; result } } }

arrow_chain:
    | t=type_app { ([], t) }
    | t=type_app ARROW rest=arrow_chain
        { let arguments, result = rest in (t :: arguments, result) }

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
        { { Typedef.Type_record_row.name = located $loc(name) name; body } }

value_decl_with_type:
    | name1=loc(LCNAME) COLON INDENT type_alias=type_expr DEDENT
      name2=loc(LCNAME) params=list(pattern_atom) EQUAL written=indented(expr)
        { 
          let type_part_data = Some Declaration.{ name=name1; type_alias } in
          let params, expr = make_parameters ~params written in
          let body_part = Declaration.{ name=name2; expr; params } in
          Impl.Top_declaration { type_part_data; body_part }
        }
    | name=loc(LCNAME) params=list(pattern_atom) EQUAL written=indented(expr)
        { 
          let params, expr = make_parameters ~params written in
          let body_part = Declaration.{ name; expr; params } in
          Impl.Top_declaration { type_part_data=None; body_part }
        }
   
expr:    
    | e=expr_lowered_binop { e }
    | minus=MINUS e=expr %prec UMINUS
        { located $loc (Expr_unop { name = located $loc(minus) "-"; operand = e }) }
    | e=expr_app { e }
    | e=expr_binop { e }
    | BACKSLASH params=nonempty_list(pattern_atom) ARROW written=expr %prec ARROW
        { let params, body = make_parameters ~params written in
          located $loc (Expr_lambda { params; body }) }
    | IF if_exp=expr THEN then_exp=expr ELSE else_exp=expr
        { located $loc (Expr_if_then_else { if_exp; then_exp; else_exp }) }
    | CASE expr=scrutinee OF pattern_data_items=indented(list(case_arm))
        { located $loc (Expr_pattern { expr; pattern_data_items }) }
    | LET binding=expr_let_name_bind IN e=expr
        { make_expr_let ~bindings:[binding] e }
    | LET INDENT bindings=expr_let_defs DEDENT IN e=expr
        { make_expr_let ~bindings e }

expr_lowered_binop:
    | head=expr CONS tail=expr { located $loc (Expr_cons { head; tail }) }
    | arg=expr PIPE_GT fn=expr { located $loc (Expr_apply { fn; arg }) }
    | fn=expr APPLY_L arg=expr { located $loc (Expr_apply { fn; arg }) }
    | outer=expr op=COMPOSE_L inner=expr
        { make_expr_apply ~args:[outer; inner]
            (make_qualified ~region:(Region.of_lexing $loc(op)) "Basics.composeL") }
    | inner=expr op=COMPOSE_R outer=expr
        { make_expr_apply ~args:[inner; outer]
            (make_qualified ~region:(Region.of_lexing $loc(op)) "Basics.composeR") }

expr_let_name_bind:
    | name=loc(LCNAME) params=list(pattern_atom) EQUAL INDENT written=expr DEDENT
        { let params, body = make_parameters ~params written in
          Bind_value { bind_type = None;
                       bind_body={ name;
                                   body = make_expr_lambda ~region:(Region.of_lexing $loc) ~params body } } }
    | annotated=LCNAME COLON INDENT content=type_expr DEDENT
      name=loc(LCNAME) params=list(pattern_atom) EQUAL INDENT written=expr DEDENT
        { let params, body = make_parameters ~params written in
          Bind_value { bind_type = Some { name = annotated; content };
                       bind_body={ name;
                                   body = make_expr_lambda ~region:(Region.of_lexing $loc) ~params body } } }
    | pattern=destructuring_pattern EQUAL INDENT value=expr DEDENT
        { Bind_pattern { pattern; value } }

destructuring_pattern:
    | LPAREN p=pattern RPAREN { p }
    | LPAREN p=pattern COMMA rest=separated_nonempty_list(COMMA, pattern) RPAREN
        { located $loc (P_tuple(p :: rest)) }
    | LBRACE lst=separated_list(COMMA, LCNAME) RBRACE { located $loc (P_record(lst)) }

expr_let_defs: lst=nonempty_list(expr_let_name_bind) { lst }

scrutinee:
    | e=expr_app { e }
    | e=expr_binop { e }

case_arm:
    | pattern=pattern ARROW expr=indented(expr)
        {{ pattern; expr }}    

pattern:
    | name=upper_possible_dotted args=nonempty_list(pattern_atom) { located $loc (P_ctor(name, args)) }
    | head=pattern_atom CONS tail=pattern { located $loc (P_cons(head, tail)) }
    | p=pattern AS name=LCNAME { located $loc (P_alias(p, name)) }
    | p=pattern_atom { p }

pattern_atom:
    | i=STRING { located $loc (P_str i) }
    | i=CHAR { located $loc (P_chr i) }
    | i=INT { located $loc (P_int i) }
    | MINUS i=INT { located $loc (P_int (-i)) }
    | WILDCARD { located $loc P_anything }
    | UNIT { located $loc P_unit }
    | i=LCNAME { located $loc (P_var i) }
    | name=upper_possible_dotted { located $loc (P_ctor(name, [])) }
    | LBRACE lst=separated_list(COMMA, LCNAME) RBRACE { located $loc (P_record(lst)) }
    | LBRACKET lst=separated_list(COMMA, pattern) RBRACKET { located $loc (P_list(lst)) }
    | LPAREN p=pattern RPAREN { p }
    | LPAREN p=pattern COMMA rest=separated_nonempty_list(COMMA, pattern) RPAREN
        { located $loc (P_tuple(p :: rest)) }

expr_binop:
    e1=expr name=binop e2=expr { located $loc (Expr_binop { name; operands=(e1, e2) }) }

expr_app:
    | e=expr_postfix { e }
    | fn=expr_app arg=expr_postfix { located $loc (Expr_apply { fn; arg }) }

expr_postfix:
    | base=expr_applicable fields=list(loc(ACCESS)) {
        List.fold_left
          (fun acc field ->
             Located.at (Region.merge acc.Located.region field.Located.region)
               (Expr_access { expr = acc; field }))
          base fields
    }

expr_applicable:
    | LPAREN e=expr RPAREN { e }
    | LPAREN name=binop RPAREN { make_operator_value ~region:(Region.of_lexing $loc) name }
    | LPAREN PIPE_GT RPAREN { make_qualified ~region:(Region.of_lexing $loc) "Basics.apR" }
    | LPAREN APPLY_L RPAREN { make_qualified ~region:(Region.of_lexing $loc) "Basics.apL" }
    | LPAREN COMPOSE_L RPAREN { make_qualified ~region:(Region.of_lexing $loc) "Basics.composeL" }
    | LPAREN COMPOSE_R RPAREN { make_qualified ~region:(Region.of_lexing $loc) "Basics.composeR" }
    | LPAREN first=expr COMMA rest=separated_nonempty_list(COMMA, expr) RPAREN
        { make_expr_tuple ~region:(Region.of_lexing $loc) (first :: rest) }
    | e=LCNAME { located $loc (Expr_ident e) }
    | e=STRING { located $loc (Expr_string e) }
    | e=CHAR { located $loc (Expr_char e) }
    | field=loc(ACCESSOR) { located $loc (Expr_accessor field) }
    | e=INT { located $loc (Expr_int e) }
    | e=FLOAT { located $loc (Expr_float e) }
    | UNIT { located $loc Expr_unit }
    | n=UCNAME { located $loc (Expr_constr_fixed n) }
    | n=QUAL_LCNAME { make_qualified ~region:(Region.of_lexing $loc) n }
    | n=UCNAME_PATH { make_qualified ~region:(Region.of_lexing $loc) n }
    | LBRACKET e=separated_list(COMMA, expr) RBRACKET { located $loc (Expr_list e) }
    | LBRACE RBRACE { located $loc (Expr_record []) }
    | LBRACE name=LCNAME EQUAL value=expr rest=record_literal_rest RBRACE
        { located $loc (Expr_record ({ name; value } :: rest)) }
    | LBRACE record=loc(LCNAME) PIPE fields=record_update_fields RBRACE
        { located $loc (Expr_record_update
            { record = Located.map (fun name -> Expr_ident name) record; fields }) }
    | LBRACE record=loc(QUAL_LCNAME) PIPE fields=record_update_fields RBRACE
        { located $loc (Expr_record_update
            { record = make_qualified ~region:record.Located.region record.Located.thing; fields }) }

record_literal_rest:
    | { [] }
    | COMMA fields=separated_nonempty_list(COMMA, expr_record_field) { fields }

record_update_fields:
    | fields=separated_nonempty_list(COMMA, expr_record_field) { fields }

expr_record_field:
    name=LCNAME EQUAL value=expr {{ name; value }}

%inline
binop:
    | PLUS { "+" }
    | MINUS { "-" }
    | DIV { "/" }
    | IDIV { "//" }
    | POW { "^" }
    | TIMES { "*" }
    | CONCAT { "++" }
    | EQ_EQ { "==" }
    | NOT_EQ { "/=" }
    | GT { ">" }
    | LT { "<" }
    | GT_EQ { ">=" }
    | LT_EQ { "<=" }
    | AND { "&&" }
    | OR { "||" }

indented(X): INDENT x=X DEDENT { x }
