(* JavaScript AST for code generation *)

type identifier = string [@@deriving show]

type literal =
  | Int of int
  | Float of float
  | String of string
  | Bool of bool
  | Null
[@@deriving show]

type binop =
  | Plus
  | Minus
  | Multiply
  | Divide
  | Modulo
  | Equal
  | NotEqual
  | StrictEqual
  | StrictNotEqual
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  | And
  | Or
[@@deriving show]

type unop = Not | Negative | Typeof [@@deriving show]

type expr =
  | Literal of literal
  | Identifier of identifier
  | Binary of { left : expr; op : binop; right : expr }
  | Unary of { op : unop; arg : expr }
  | Call of { callee : expr; args : expr list }
  | Function of { params : identifier list; body : stmt list }
  | Arrow of { params : identifier list; body : arrow_body }
  | Member of { object_ : expr; property : expr; computed : bool }
  | Conditional of { test : expr; consequent : expr; alternate : expr }
  | Object of (identifier * expr) list
  | Array of expr list
  | Assignment of { left : expr; right : expr }
[@@deriving show]

and arrow_body = ArrowExpr of expr | ArrowBlock of stmt list [@@deriving show]

and stmt =
  | ExprStmt of expr
  | Return of expr option
  | VarDecl of { name : identifier; init : expr option }
  | ConstDecl of { name : identifier; init : expr }
  | FunctionDecl of {
      name : identifier;
      params : identifier list;
      body : stmt list;
    }
  | If of { test : expr; consequent : stmt list; alternate : stmt list option }
  | Block of stmt list
  | Switch of { discriminant : expr; cases : case list }
  | While of { test : expr; body : stmt list }
  | Continue
[@@deriving show]

and case = { test : expr option; consequent : stmt list } [@@deriving show]

type program = stmt list [@@deriving show]
