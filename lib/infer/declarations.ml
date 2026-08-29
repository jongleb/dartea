open Typed
open Typed.Type
open Expressions

type infer_result = {
  values : Value_env.t;
  declarations : Typed.Declaration.t list;
  typedecls : Canonical.Typedecl.t list;
  errors : Reporting.Error.t list;
}

type group_member = {
  position : int;
  declaration : Canonical.Declaration.t;
  member_name : Data.Name.t;
  assumption : Type.t option;
}

type inferred_member = {
  member : group_member;
  result : Typed.Declaration.t;
  typ : Type.t;
}

let zonk result =
  {
    result with
    values = Value_env.zonk result.values;
    declarations = List.map Declaration.zonk result.declarations;
  }

let declared_name (declaration : Canonical.Declaration.t) =
  Data.Name.local (Data.Located.unwrap declaration.body_part.name)

let rec after_parameters expected (params : string Data.Located.t list) assumption =
  match (params, assumption) with
  | [], _ | _, [] -> Some expected
  | param :: rest, argument :: others -> begin
      match Type.head expected with
      | TFun (declared, result) ->
          Unify.types ~region:param.region
            ~category:(Reporting.Category.Local (Data.Name.local param.thing))
            ~expected:(No_expectation declared) argument;
          after_parameters result rest others
      | TVar _ | TInt | TFloat | TChar | TBool | TStr | TUnit | TTup _
      | TCustom _ | TRecord _ | TRowExtend _ | TRowEmpty ->
          None
    end

let infer_declaration type_env visible
    { Canonical.Declaration.body_part; type_part_data } =
  let param_types, visible_in_the_body =
    assume_parameters visible body_part.params
  in
  let name = Data.Located.unwrap body_part.name in
  let written =
    Option.map
      (fun (type_part : Canonical.Declaration.type_part) ->
        Type_env.expand_written ~region:type_part.name.region type_env
          type_part.type_alias)
      type_part_data
  in
  let result_of_the_annotation =
    Option.map
      (fun expected ->
        (expected, after_parameters expected body_part.params param_types))
      written
  in
  let typed_body = infer_with_env body_part.expr visible_in_the_body type_env in
  let inferred_type = function_of param_types ~result:typed_body.typ in
  Option.iter
    (fun (whole, result) ->
      let found, expected =
        match result with
        | Some result -> (typed_body.typ, result)
        | None -> (inferred_type, whole)
      in
      annotate ~region:body_part.expr.region ~name
        ~category:(category_of body_part.expr) ~expected found)
    result_of_the_annotation;
  ( {
      Typed.Declaration.name = body_part.name;
      params =
        List.map2
          (fun name typ -> { Typed.Declaration.name; typ })
          body_part.params param_types;
      body = typed_body;
      typ = inferred_type;
    },
    inferred_type )

let member (position, (declaration : Canonical.Declaration.t)) =
  let assumption =
    match declaration.type_part_data with
    | Some _ -> None
    | None -> Some (fresh_variable None)
  in
  { position; declaration; member_name = declared_name declaration; assumption }

let infer_member type_env inside results member =
  let result, typ = infer_declaration type_env inside member.declaration in
  Option.iter
    (fun assumption ->
      expect ~expected:assumption member.declaration.body_part.expr typ)
    member.assumption;
  { member; result; typ } :: results

let infer_group type_env visible group =
  let results =
    infer_deeper (fun () ->
        let members = List.map member group in
        let inside =
          List.fold_left
            (fun visible member ->
              match member.assumption with
              | None -> visible
              | Some typ ->
                  Value_env.bind member.member_name (Scheme ([], typ)) visible)
            visible members
        in
        List.fold_left (infer_member type_env inside) [] members)
  in
  let scope =
    List.fold_left
      (fun visible found ->
        Value_env.bind found.member.member_name
          (generalize found.typ)
          visible)
      visible results
  in
  ( scope,
    List.rev_map
      (fun found -> (found.member.position, found.result))
      results )

let announce type_env visible (declarations : Canonical.Declaration.t list) =
  List.fold_left
    (fun collected (declaration : Canonical.Declaration.t) ->
      match declaration.type_part_data with
      | None -> collected
      | Some annotation ->
          Value_env.bind (declared_name declaration)
            (generalize
               (infer_deeper (fun () ->
                    Type_env.expand_written ~region:annotation.name.region
                      type_env annotation.type_alias)))
            collected)
    visible declarations

let imported_values type_env (imports : Interface.t list) =
  List.concat_map (fun (interface : Interface.t) -> interface.values) imports
  |> List.fold_left
       (fun known (value : Interface.value) ->
         Value_env.bind value.name value.scheme known)
       (Type_env.constructor_values type_env)

let infer_toplevel ~(imports : Interface.t list) (module_ : Canonical.Module.t) =
  let type_env = Type_env.build ~imports module_ in
  let visible = imported_values type_env imports in
  let values, typed_decls, found =
    List.mapi (fun position declaration -> (position, declaration))
      module_.top_declarations
    |> Canonicalization.Scope.in_dependency_order ~declaration:snd
    |> List.fold_left
         (fun (visible, collected, found) group ->
           match infer_group type_env visible group with
           | visible, declaration -> (visible, List.rev_append declaration collected, found)
           | exception Reporting.Error.Found error ->
               (visible, collected, error :: found))
         (announce type_env visible module_.top_declarations, [], [])
  in
  let in_source_order =
    List.sort (fun (left, _) (right, _) -> Int.compare left right) typed_decls
    |> List.map snd
  in
  zonk
    {
      values;
      declarations = in_source_order;
      typedecls = Type_env.typedecls type_env;
      errors = List.rev found;
    }

let interface_of (module_ : Canonical.Module.t) (result : infer_result) :
    Interface.t =
  let values =
    List.fold_left
      (fun collected (d : Canonical.Declaration.t) ->
        let name = Data.Located.unwrap d.body_part.name in
        Value_env.find (Data.Name.local name) result.values
        |> Option.map (fun scheme -> (name, scheme) :: collected)
        |> Option.value ~default:collected)
      [] module_.top_declarations
  in
  Interface.of_module ~values module_
