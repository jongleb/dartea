module J = Ast

let member object_ property =
  J.Member { object_; property = J.Identifier property; computed = false }

let call callee arguments =
  J.Call { callee = J.Identifier callee; args = arguments }

let method_call object_ method_ arguments =
  J.Call { callee = member object_ method_; args = arguments }

let math name arguments = method_call (J.Identifier "Math") name arguments
let binary left op right = J.Binary { left; op; right }

let runtime name arguments =
  J.Call
    { callee = member (J.Identifier Runtime.module_name) name; args = arguments }

let nullary_value (kernel : Data.Kernel.nullary) : J.expr =
  match kernel with
  | Basics_pi -> member (J.Identifier "Math") "PI"
  | Basics_e -> member (J.Identifier "Math") "E"

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
  | String_to_float_unsafe -> call "Number" [ subject ]
  | String_is_float ->
      binary
        (binary subject J.StrictNotEqual (J.Literal (J.String "")))
        J.And
        (J.Unary { op = J.Not; arg = call "isNaN" [ call "Number" [ subject ] ] })
  | Basics_to_float -> subject
  | Basics_round -> math "round" [ subject ]
  | Basics_floor -> math "floor" [ subject ]
  | Basics_ceiling -> math "ceil" [ subject ]
  | Basics_truncate -> binary subject J.BitOr (J.Literal (J.Int 0))
  | Basics_is_nan -> call "isNaN" [ subject ]
  | Basics_is_infinite ->
      binary
        (binary subject J.StrictEqual (J.Identifier "Infinity"))
        J.Or
        (binary subject J.StrictEqual
           (J.Unary { op = J.Negative; arg = J.Identifier "Infinity" }))
  | Basics_sqrt -> math "sqrt" [ subject ]
  | Basics_log -> math "log" [ subject ]
  | Basics_cos -> math "cos" [ subject ]
  | Basics_sin -> math "sin" [ subject ]
  | Basics_tan -> math "tan" [ subject ]
  | Basics_acos -> math "acos" [ subject ]
  | Basics_asin -> math "asin" [ subject ]
  | Basics_atan -> math "atan" [ subject ]
  | Basics_not -> J.Unary { op = J.Not; arg = subject }
  | Char_to_code -> runtime Runtime.char_to_code [ subject ]
  | Char_from_code -> runtime Runtime.char_from_code [ subject ]
  | Char_to_upper -> method_call subject "toUpperCase" []
  | Char_to_lower -> method_call subject "toLowerCase" []

let binary_operation (kernel : Data.Kernel.binary) (left : J.expr)
    (right : J.expr) : J.expr =
  match kernel with
  | String_append -> binary left J.Plus right
  | Utils_compare -> runtime Runtime.compare [ left; right ]
  | Basics_mod_by -> runtime Runtime.mod_by [ left; right ]
  | Basics_remainder_by -> binary right J.Modulo left
  | Basics_atan2 -> math "atan2" [ left; right ]
  | Basics_xor -> binary left J.StrictNotEqual right

let value (kernel : Data.Kernel.t) : J.expr =
  let arrow parameters body =
    J.Arrow { params = parameters; body = J.ArrowExpr body }
  in
  match kernel with
  | Nullary nullary -> nullary_value nullary
  | Unary unary -> arrow [ "x" ] (unary_operation unary (J.Identifier "x"))
  | Binary binary ->
      arrow [ "a"; "b" ]
        (binary_operation binary (J.Identifier "a") (J.Identifier "b"))
