open Typed
open Typed.Type

type t = {
  given : (int, string) Hashtbl.t;
  rounds : (Data.Constraint.t, int) Hashtbl.t;
  mutable plain : int;
}

let fail format = Printf.ksprintf failwith format
let letters = "abcdefghijklmnopqrstuvwxyz"

let numbering text round =
  if round = 0 then text else text ^ string_of_int (round + 1)

let naming () =
  { given = Hashtbl.create 16; rounds = Hashtbl.create 4; plain = 0 }

let round_of naming carried =
  let taken = Option.value ~default:0 (Hashtbl.find_opt naming.rounds carried) in
  Hashtbl.replace naming.rounds carried (taken + 1);
  taken

let letter naming =
  let taken = naming.plain in
  naming.plain <- taken + 1;
  numbering
    (String.make 1 letters.[taken mod String.length letters])
    (taken / String.length letters)

let name_of naming variable =
  match Hashtbl.find_opt naming.given (Variable.identity variable) with
  | Some name -> name
  | None ->
      let name =
        match Variable.constraint_of variable with
        | Some carried ->
            numbering (Data.Constraint.name carried) (round_of naming carried)
        | None -> letter naming
      in
      Hashtbl.add naming.given (Variable.identity variable) name;
      name

let within naming ty =
  let rec written ty =
    match Type.head ty with
    | TVar variable -> name_of naming variable
    | TInt -> "Int"
    | TFloat -> "Float"
    | TChar -> "Char"
    | TBool -> "Bool"
    | TUnit -> "()"
    | TStr -> "String"
    | TFun (parameter, result) ->
        parenthesised parameter ^ " -> " ^ written result
    | TTup items -> "( " ^ String.concat ", " (List.map written items) ^ " )"
    | TCustom (name, []) -> Data.Name.to_string name
    | TCustom (name, arguments) ->
        Data.Name.to_string name ^ " "
        ^ String.concat " " (List.map parenthesised arguments)
    | TRecord row -> "{ " ^ String.concat ", " (fields row) ^ " }"
    | (TRowExtend _ | TRowEmpty) as row ->
        "{ " ^ String.concat ", " (fields row) ^ " }"
  and fields row =
    match Type.head row with
    | TRowEmpty -> []
    | TVar variable -> [ name_of naming variable ]
    | TRowExtend (label, typ, rest) ->
        Printf.sprintf "%s : %s" label (written typ) :: fields rest
    | TInt | TFloat | TChar | TBool | TStr | TUnit | TFun _ | TTup _
    | TCustom _ | TRecord _ ->
        [ written row ]
  and parenthesised ty =
    match Type.head ty with
    | TFun _ | TCustom (_, _ :: _) -> "(" ^ written ty ^ ")"
    | TVar _ | TInt | TFloat | TChar | TBool | TStr | TUnit | TTup _
    | TCustom (_, []) | TRecord _ | TRowExtend _ | TRowEmpty ->
        written ty
  in
  written ty

let of_type ty = within (naming ()) ty
