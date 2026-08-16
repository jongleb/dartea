module J = Ast

let member object_ property =
  J.Member { object_; property = J.Identifier property; computed = false }

let call callee arguments =
  J.Call { callee = J.Identifier callee; args = arguments }

let method_call object_ method_ arguments =
  J.Call { callee = member object_ method_; args = arguments }

let binary left op right = J.Binary { left; op; right }

let unary_operation (kernel : Data.Kernel.unary) (subject : J.expr) : J.expr =
  match kernel with
  | String_length -> member subject "length"
  | String_from_number -> call "String" [ subject ]
  | String_to_int_unsafe -> call "Number" [ subject ]
  | String_is_int ->
      binary
        (binary subject J.StrictNotEqual (J.Literal (J.String "")))
        J.And
        (method_call (J.Identifier "Number") "isInteger"
           [ call "Number" [ subject ] ])

let binary_operation (kernel : Data.Kernel.binary) (left : J.expr)
    (right : J.expr) : J.expr =
  match kernel with
  | String_append -> binary left J.Plus right

let value (kernel : Data.Kernel.t) : J.expr =
  let arrow parameters body =
    J.Arrow { params = parameters; body = J.ArrowExpr body }
  in
  match kernel with
  | Unary unary -> arrow [ "x" ] (unary_operation unary (J.Identifier "x"))
  | Binary binary ->
      arrow [ "a"; "b" ]
        (binary_operation binary (J.Identifier "a") (J.Identifier "b"))
