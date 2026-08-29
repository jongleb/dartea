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

let rec fold_expression (visit : 'a -> expr -> 'a) (found : 'a) (e : expr) : 'a
    =
  let inside = fold_expression visit in
  let all = List.fold_left inside in
  let found = visit found e in
  match e with
  | Identifier _ | Literal _ -> found
  | Binary { left; right; _ } -> inside (inside found left) right
  | Unary { arg; _ } -> inside found arg
  | Call { callee; args } | New { callee; args } ->
      all (inside found callee) args
  | Function { body; _ } -> List.fold_left (fold_statement visit) found body
  | Arrow { body = ArrowExpr result; _ } -> inside found result
  | Arrow { body = ArrowBlock body; _ } ->
      List.fold_left (fold_statement visit) found body
  | Member { object_; property; computed } ->
      if computed then inside (inside found object_) property
      else inside found object_
  | Conditional { test; consequent; alternate } ->
      inside (inside (inside found test) consequent) alternate
  | Object members ->
      List.fold_left
        (fun found -> function Field (_, value) | Spread value ->
          inside found value)
        found members
  | Array items -> all found items
  | Assignment { left; right } -> inside (inside found left) right

and fold_statement (visit : 'a -> expr -> 'a) (found : 'a) (s : stmt) : 'a =
  let inside = fold_expression visit in
  let block = List.fold_left (fold_statement visit) in
  match s with
  | ExprStmt e | Throw e | ConstDecl { init = e; _ }
  | VarDecl { init = Some e; _ }
  | Return (Some e) ->
      inside found e
  | Return None | VarDecl { init = None; _ } | Continue | Import_namespace _
  | Export _ | Comment _ ->
      found
  | FunctionDecl { body; _ } | Block body | While { body; _ } -> block found body
  | If { test; consequent; alternate } ->
      let found = block (inside found test) consequent in
      Option.fold ~none:found ~some:(block found) alternate
  | Switch { discriminant; cases } ->
      List.fold_left
        (fun found { test; consequent } ->
          let found = Option.fold ~none:found ~some:(inside found) test in
          block found consequent)
        (inside found discriminant)
        cases

let fold (visit : 'a -> expr -> 'a) (found : 'a) (p : program) : 'a =
  List.fold_left (fold_statement visit) found p

let references (wanted : identifier) (p : program) : bool =
  fold
    (fun found expression ->
      found
      ||
      match expression with
      | Identifier name -> String.equal name wanted
      | _ -> false)
    false p

let members_of ~(object_ : identifier) (p : program) : identifier list =
  List.rev
    (fold
       (fun found expression ->
         match expression with
         | Member
             {
               object_ = Identifier owner;
               property = Identifier name;
               computed = false;
             }
           when String.equal owner object_ ->
             if List.mem name found then found else name :: found
         | _ -> found)
       [] p)

let int n = Literal (Int n)
let string text = Literal (String text)
let bool truth = Literal (Bool truth)
let binary op left right = Binary { left; op; right }
let call callee args = Call { callee; args }

let member object_ property =
  Member { object_; property = Identifier property; computed = false }

let at_index object_ index =
  Member { object_; property = Literal (Int index); computed = true }

let assign name value =
  ExprStmt (Assignment { left = Identifier name; right = value })

let returning_when test result =
  If { test; consequent = [ Return (Some result) ]; alternate = None }

let is_object subject =
  binary StrictEqual (Unary { op = Typeof; arg = subject }) (string "object")
