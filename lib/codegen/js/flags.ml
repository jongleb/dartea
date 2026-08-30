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
let value_param = "value"
let path_param = "path"
let given_param = "given"
let value = J.Identifier value_param
let path = J.Identifier path_param
let given = J.Identifier given_param

let check_with ~helper arguments =
  arrow [ value_param; path_param ] (J.call (J.Identifier helper) ([ value; path ] @ arguments))

let primitive ~fits ~wanted =
  check_with ~helper:Runtime.flag_prim [ arrow [ given_param ] fits; J.string wanted ]
let take_with helper inside = check_with ~helper [ inside ]
let tuple_of written = check_with ~helper:Runtime.flag_tuple [ J.Array written ]

let record_of rows written =
  check_with ~helper:Runtime.flag_record
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
  match (Primitives.Known_type.of_name name, arguments) with
  | Some Primitives.Known_type.Value, _ -> Ok (arrow [ "value" ] value)
  | Some Primitives.Known_type.Maybe, [ inside ] ->
      Result.map (take_with Runtime.flag_maybe) (decoder inside)
  | Some Primitives.Known_type.List, [ inside ] ->
      Result.map (take_with Runtime.flag_list) (decoder inside)
  | (Some _ | None), _ -> Error (describe ty)

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

type encoding = J.expr option

let encoded (enc : encoding) (payload : J.expr) =
  match enc with None -> payload | Some fn -> J.call fn [ payload ]

let convert body = Some (arrow [ value_param ] body)

let rec encoder (ty : T.t) : (encoding, string) result =
  match T.head ty with
  | T.TInt | T.TFloat | T.TBool | T.TStr -> Ok None
  | T.TUnit -> Ok (convert (J.Literal J.Null))
  | T.TCustom (name, arguments) -> encoder_custom ty name arguments
  | T.TTup parts -> encoder_tuple parts
  | T.TRecord row -> encoder_record row
  | T.TVar _ | T.TChar | T.TFun _ | T.TRowExtend _ | T.TRowEmpty ->
      Error (describe ty)

and encoder_custom ty name arguments =
  match (Primitives.Known_type.of_name name, arguments) with
  | Some Primitives.Known_type.Value, _ -> Ok None
  | Some Primitives.Known_type.Maybe, [ inside ] ->
      Result.map
        (fun enc ->
          convert
            (J.Conditional
               {
                 test =
                   J.binary J.StrictEqual
                     (J.Unary { op = J.Typeof; arg = value })
                     (J.string "object");
                 consequent = encoded enc (J.member value "_0");
                 alternate = J.Literal J.Null;
               }))
        (encoder inside)
  | Some Primitives.Known_type.List, [ inside ] ->
      Result.map
        (fun enc ->
          Some
            (arrow [ value_param ]
               (J.call (J.Identifier Runtime.port_list)
                  [ Option.value enc ~default:(arrow [ given_param ] given); value ])))
        (encoder inside)
  | (Some _ | None), _ -> Error (describe ty)

and encoder_gather (held : (encoding, string) result list) :
    (encoding list, string) result =
  List.fold_right
    (fun one more ->
      match (one, more) with
      | Ok enc, Ok rest -> Ok (enc :: rest)
      | Error refused, _ | _, Error refused -> Error refused)
    held (Ok [])

and all_identity encs = List.for_all Option.is_none encs

and encoder_tuple parts =
  Result.map
    (fun encs ->
      if all_identity encs then None
      else
        convert
          (J.Array (List.mapi (fun index enc -> encoded enc (J.at_index value index)) encs)))
    (encoder_gather (List.map encoder parts))

and encoder_record row =
  let rows = fields row in
  Result.map
    (fun encs ->
      if all_identity encs then None
      else
        convert
          (J.Object
             (List.map2
                (fun (name, _) enc -> J.Field (name, encoded enc (J.member value name)))
                rows encs)))
    (encoder_gather (List.map (fun (_, ty) -> encoder ty) rows))
