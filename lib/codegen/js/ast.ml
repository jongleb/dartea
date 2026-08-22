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
  | Exponent
  | BitOr
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
  | New of { callee : expr; args : expr list }
  | Function of { params : identifier list; body : stmt list }
  | Arrow of { params : identifier list; body : arrow_body }
  | Member of { object_ : expr; property : expr; computed : bool }
  | Conditional of { test : expr; consequent : expr; alternate : expr }
  | Object of object_member list
  | Array of expr list
  | Assignment of { left : expr; right : expr }
[@@deriving show]

and object_member = Field of identifier * expr | Spread of expr
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
  | Throw of expr
  | Import_namespace of { local : identifier; from : string }
  | Export of identifier list
  | Comment of string
[@@deriving show]

and case = { test : expr option; consequent : stmt list } [@@deriving show]

type program = stmt list [@@deriving show]

let rec expression_references (wanted : identifier) (e : expr) : bool =
  let references = expression_references wanted in
  let any = List.exists references in
  match e with
  | Identifier name -> String.equal name wanted
  | Literal _ -> false
  | Binary { left; right; _ } -> references left || references right
  | Unary { arg; _ } -> references arg
  | Call { callee; args } | New { callee; args } ->
      references callee || any args
  | Function { body; _ } -> List.exists (statement_references wanted) body
  | Arrow { body = ArrowExpr result; _ } -> references result
  | Arrow { body = ArrowBlock body; _ } ->
      List.exists (statement_references wanted) body
  | Member { object_; property; computed } ->
      references object_ || (computed && references property)
  | Conditional { test; consequent; alternate } ->
      references test || references consequent || references alternate
  | Object members ->
      List.exists
        (function Field (_, value) | Spread value -> references value)
        members
  | Array items -> any items
  | Assignment { left; right } -> references left || references right

and statement_references (wanted : identifier) (s : stmt) : bool =
  let references = expression_references wanted in
  let block = List.exists (statement_references wanted) in
  match s with
  | ExprStmt e | Throw e | ConstDecl { init = e; _ }
  | VarDecl { init = Some e; _ }
  | Return (Some e) ->
      references e
  | Return None | VarDecl { init = None; _ } | Continue | Import_namespace _
  | Export _ | Comment _ ->
      false
  | FunctionDecl { body; _ } | Block body | While { body; _ } -> block body
  | If { test; consequent; alternate } ->
      references test || block consequent
      || Option.fold ~none:false ~some:block alternate
  | Switch { discriminant; cases } ->
      references discriminant
      || List.exists
           (fun { test; consequent } ->
             Option.fold ~none:false ~some:references test || block consequent)
           cases

let references (wanted : identifier) (p : program) : bool =
  List.exists (statement_references wanted) p
