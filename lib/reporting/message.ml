open Typed
open Typed.Type

type t = {
  given : (int, string) Hashtbl.t;
  rounds : (Data.Constraint.t, int) Hashtbl.t;
  mutable plain : int;
}

let letters = "abcdefghijklmnopqrstuvwxyz"

let number text round =
  if round = 0 then text else text ^ string_of_int (round + 1)

let namer () =
  { given = Hashtbl.create 16; rounds = Hashtbl.create 4; plain = 0 }

let round_of namer carried =
  let taken = Option.value ~default:0 (Hashtbl.find_opt namer.rounds carried) in
  Hashtbl.replace namer.rounds carried (taken + 1);
  taken

let letter namer =
  let taken = namer.plain in
  namer.plain <- taken + 1;
  number
    (String.make 1 letters.[taken mod String.length letters])
    (taken / String.length letters)

let name_of namer variable =
  match Hashtbl.find_opt namer.given (Variable.identity variable) with
  | Some name -> name
  | None ->
      let name =
        match Variable.constraint_of variable with
        | Some carried ->
            number (Data.Constraint.name carried) (round_of namer carried)
        | None -> letter namer
      in
      Hashtbl.add namer.given (Variable.identity variable) name;
      name

let same_shape left right =
  match (left, right) with
  | TVar one, TVar other -> Variable.equal Typed.Type.equal one other
  | TInt, TInt
  | TFloat, TFloat
  | TChar, TChar
  | TBool, TBool
  | TUnit, TUnit
  | TStr, TStr
  | TRowEmpty, TRowEmpty
  | TFun _, TFun _
  | TRecord _, TRecord _
  | TRowExtend _, TRowExtend _ ->
      true
  | TTup these, TTup those -> List.length these = List.length those
  | TCustom (name, these), TCustom (other_name, those) ->
      Data.Name.equal name other_name && List.length these = List.length those
  | ( ( TVar _ | TInt | TFloat | TChar | TBool | TUnit | TStr | TRowEmpty
      | TFun _ | TTup _ | TCustom _ | TRecord _ | TRowExtend _ ),
      _ ) ->
      false

let arrow_length ty =
  let rec count found ty =
    match Type.head ty with TFun (_, result) -> count (found + 1) result | _ -> found
  in
  count 0 ty

let constraint_of ty =
  match Type.head ty with
  | TVar variable -> Variable.constraint_of variable
  | TInt | TFloat | TChar | TBool | TStr | TUnit | TFun _ | TTup _ | TCustom _
  | TRecord _ | TRowExtend _ | TRowEmpty ->
      None

let could_fit one other =
  match (one, other) with
  | TVar _, _ | _, TVar _ -> true
  | _, _ -> same_shape one other

let told_apart left right =
  match (Type.head left, Type.head right) with
  | TInt, TFloat | TFloat, TInt -> [ Hint.Int_float ]
  | TInt, TStr -> [ Hint.String_from_int ]
  | TFloat, TStr -> [ Hint.String_from_float ]
  | TStr, TInt -> [ Hint.String_to_int ]
  | TStr, TFloat -> [ Hint.String_to_float ]
  | _, TBool -> [ Hint.Anything_to_bool ]
  | TCustom (name, [ inside ]), other
    when Primitives.Known_type.fits Primitives.Known_type.Maybe name
         && could_fit (Type.head inside) other ->
      [ Hint.Anything_from_maybe ]
  | TFun _, TFun _ ->
      [
        Hint.Arity_mismatch
          { found = arrow_length left; expected = arrow_length right };
      ]
  | _, _ -> (
      match (constraint_of left, constraint_of right) with
      | Some required, None ->
          [ Hint.Bad_flex_super { direction = Have; required; found = right } ]
      | None, Some required ->
          [ Hint.Bad_flex_super { direction = Need; required; found = left } ]
      | Some _, Some _ | None, None -> [])

let rec fields_of row =
  match Type.head row with
  | TRowExtend (label, typ, rest) ->
      let fields, tail = fields_of rest in
      ((label, typ) :: fields, tail)
  | TRecord inner -> fields_of inner
  | settled -> ([], settled)

let separate separator parts =
  match parts with
  | [] -> Doc.empty
  | first :: rest ->
      Doc.beside
        (first
        :: List.concat_map (fun part -> [ Doc.text separator; part ]) rest)

let children_of ty =
  match ty with
  | TFun (parameter, result) -> [ parameter; result ]
  | TTup items -> items
  | TCustom (_, arguments) -> arguments
  | TVar _ | TInt | TFloat | TChar | TBool | TStr | TUnit | TRowEmpty
  | TRecord _ | TRowExtend _ ->
      []

let rec written namer ~against ty =
  let head = Type.head ty in
  match against with
  | Some other when not (same_shape (Type.head other) head) ->
      (Doc.yellow (plain namer ty), told_apart ty other)
  | Some _ | None ->
    let theirs =
      match against with
      | None -> []
      | Some other -> children_of (Type.head other)
    in
    let seen = ref [] in
    let child index inner =
      let doc, problems = written namer ~against:(List.nth_opt theirs index) inner in
      seen := !seen @ problems;
      doc
    in
    let nest index inner =
      match Type.head inner with
      | TFun _ | TCustom (_, _ :: _) ->
          Doc.beside [ Doc.text "("; child index inner; Doc.text ")" ]
      | TVar _ | TInt | TFloat | TChar | TBool | TStr | TUnit | TTup _
      | TCustom (_, []) | TRecord _ | TRowExtend _ | TRowEmpty ->
          child index inner
    in
    let doc =
      match head with
      | TVar variable -> Doc.text (name_of namer variable)
      | TInt -> Doc.text "Int"
      | TFloat -> Doc.text "Float"
      | TChar -> Doc.text "Char"
      | TBool -> Doc.text "Bool"
      | TUnit -> Doc.text "()"
      | TStr -> Doc.text "String"
      | TFun (parameter, result) ->
          Doc.beside [ nest 0 parameter; Doc.text " -> "; child 1 result ]
      | TTup items ->
          Doc.beside
            [
              Doc.text "( ";
              separate ", " (List.mapi (fun index item -> child index item) items);
              Doc.text " )";
            ]
      | TCustom (name, []) -> Doc.text (Data.Name.base name)
      | TCustom (name, arguments) ->
          Doc.beside
            (Doc.text (Data.Name.base name)
            :: List.concat_map
                 (fun (index, argument) -> [ Doc.text " "; nest index argument ])
                 (List.mapi (fun index argument -> (index, argument)) arguments))
      | TRecord _ | TRowExtend _ | TRowEmpty ->
          let doc, problems = record namer ~against ty in
          seen := !seen @ problems;
          doc
    in
    (doc, !seen)

and plain namer ty = fst (written namer ~against:None ty)

and record namer ~against ty =
  let ours, tail = fields_of ty in
  let theirs =
    match against with Some other -> fst (fields_of other) | None -> []
  in
  let theirs_of label = List.assoc_opt label theirs in
  let problems = ref [] in
  let field (label, typ) =
    let doc, found =
      match against with
      | None -> (plain namer typ, [])
      | Some _ -> written namer ~against:(theirs_of label) typ
    in
    problems := !problems @ found;
    let colour =
      match (against, theirs_of label) with
      | Some _, None -> Doc.yellow (Doc.text (label ^ " : "))
      | Some _, Some _ | None, _ -> Doc.text (label ^ " : ")
    in
    Doc.beside [ colour; doc ]
  in
  let absent =
    match against with
    | None -> []
    | Some _ ->
        List.filter
          (fun (label, _) -> Option.is_none (List.assoc_opt label ours))
          theirs
        |> List.map fst
  in
  let extra =
    match against with
    | None -> []
    | Some _ ->
        List.filter
          (fun (label, _) -> Option.is_none (theirs_of label))
          ours
        |> List.map fst
  in
  let about =
    match (absent, extra) with
    | [], [] -> []
    | _ :: _, _ -> [ Hint.Fields_missing absent ]
    | [], typo :: _ ->
        [ Hint.Field_typo { typo; possibilities = List.map fst theirs } ]
  in
  let entries = List.map field ours in
  let entries =
    match Type.head tail with
    | TVar variable -> entries @ [ Doc.text (name_of namer variable) ]
    | TRowEmpty | TInt | TFloat | TChar | TBool | TStr | TUnit | TFun _
    | TTup _ | TCustom _ | TRecord _ | TRowExtend _ ->
        entries
  in
  ( Doc.beside [ Doc.text "{ "; separate ", " entries; Doc.text " }" ],
    !problems @ about )

let alone namer ty = plain namer ty
let within namer ty = Doc.to_string ~colours:false (alone namer ty)
let of_type ty = within (namer ()) ty

let comparison namer ~found ~expected =
  let found_doc, these = written namer ~against:(Some expected) found in
  let expected_doc, those = written namer ~against:(Some found) expected in
  (found_doc, expected_doc, these @ those)
