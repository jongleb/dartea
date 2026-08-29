module T = Typed.Type
module J = Ast

let rec describe (ty : T.t) =
  match T.head ty with
  | T.TInt -> "Int"
  | T.TFloat -> "Float"
  | T.TChar -> "Char"
  | T.TBool -> "Bool"
  | T.TStr -> "String"
  | T.TUnit -> "()"
  | T.TFun _ -> "a function"
  | T.TTup parts -> "( " ^ String.concat ", " (List.map describe parts) ^ " )"
  | T.TCustom (name, _) -> Data.Name.base name
  | T.TRecord _ -> "a record"
  | T.TVar _ -> "a type variable"
  | T.TRowExtend _ | T.TRowEmpty -> "a record row"

let rec fields row =
  match T.head row with
  | T.TRowExtend (name, ty, rest) -> (name, ty) :: fields rest
  | T.TRowEmpty -> []
  | T.TVar _ | T.TInt | T.TFloat | T.TChar | T.TBool | T.TStr | T.TUnit
  | T.TFun _ | T.TTup _ | T.TCustom _ | T.TRecord _ ->
      []

let arrow params body = J.Arrow { params; body = J.ArrowExpr body }
let value = J.Identifier "value"
let path = J.Identifier "path"
let given = J.Identifier "given"
let check_with ~helper arguments = arrow [ "value"; "path" ] (J.call (J.Identifier helper) ([ value; path ] @ arguments))
let primitive ~fits ~wanted = check_with ~helper:"$$flagPrim" [ arrow [ "given" ] fits; J.string wanted ]
let take_with helper inside = check_with ~helper [ inside ]
let tuple_of written = check_with ~helper:"$$flagTuple" [ J.Array written ]

let record_of rows written =
  check_with ~helper:"$$flagRecord"
    [ J.Object (List.map2 (fun (name, _) code -> J.Field (name, code)) rows written) ]

let typeof_is kind = J.binary J.StrictEqual (J.Unary { op = J.Typeof; arg = given }) (J.string kind)

let rec decoder (ty : T.t) =
  match T.head ty with
  | T.TInt ->
      Ok
        (primitive
           ~fits:(J.call (J.member (J.Identifier "Number") "isInteger") [ given ])
           ~wanted:"an INT")
  | T.TFloat -> Ok (primitive ~fits:(typeof_is "number") ~wanted:"a FLOAT")
  | T.TBool -> Ok (primitive ~fits:(typeof_is "boolean") ~wanted:"a BOOL")
  | T.TStr -> Ok (primitive ~fits:(typeof_is "string") ~wanted:"a STRING")
  | T.TUnit -> Ok (arrow [] (J.Literal J.Null))
  | T.TCustom (name, arguments) -> custom ty name arguments
  | T.TTup parts -> tuple ty parts
  | T.TRecord row -> record ty row
  | T.TVar _ | T.TChar | T.TFun _ | T.TRowExtend _ | T.TRowEmpty ->
      Error (describe ty)

and custom ty name arguments =
  match (Data.Name.base name, arguments) with
  | "Value", _ -> Ok (arrow [ "value" ] value)
  | "Maybe", [ inside ] -> Result.map (take_with "$$flagMaybe") (decoder inside)
  | "List", [ inside ] -> Result.map (take_with "$$flagList") (decoder inside)
  | _, _ -> Error (describe ty)

and gather wanted =
  let join ty found =
    match (decoder ty, found) with
    | Ok one, Ok rest -> Ok (one :: rest)
    | Error refused, _ -> Error refused
    | Ok _, Error refused -> Error refused
  in
  List.fold_right join wanted (Ok [])

and tuple ty parts =
  match gather parts with
  | Error refused -> Error refused
  | Ok [] -> Error (describe ty)
  | Ok written -> Ok (tuple_of written)

and record ty row =
  match fields row with
  | [] -> Error (describe ty)
  | rows -> Result.map (record_of rows) (gather (List.map snd rows))
