open Typed
open Typed.Type
module Name_map = Data.Name.Map
open Expressions

type infer_result = {
  values : Value_env.t;
  declarations : Typed.Declaration.t list;
  siblings_env : (Data.Name.t * int) list Name_map.t;
  constructors : Type_env.ctor_info list;
  typedecls : Canonical.Typedecl.t list;
  errors : Reporting.Error.t list;
}

type group_member = {
  position : int;
  declaration : Canonical.Declaration.t;
  member_name : Data.Name.t;
  assumed : Type.t option;
}

type inferred_member = {
  member : group_member;
  typed : Typed.Declaration.t;
  checked : Type.t;
}

let zonk result =
  {
    result with
    values = Value_env.zonk result.values;
    declarations = List.map Declaration.zonk result.declarations;
  }

let infer_toplevel ~(imports : Interface.t list) (module_ : Canonical.Module.t) =
  let type_env = Type_env.build ~imports module_ in
  let visible =
    List.concat_map (fun (interface : Interface.t) -> interface.values) imports
    |> List.fold_left
         (fun known (value : Interface.value) ->
           Value_env.bind value.name value.scheme known)
         (Type_env.constructor_values type_env)
  in

  let infer_declaration { Canonical.Declaration.body_part; type_part_data } ctx =
    let assumed, visible_in_the_body = assume_parameters ctx body_part.params in
    let name = Data.Located.unwrap body_part.name in
    let written =
      Option.map
        (fun (type_part : Canonical.Declaration.type_part) ->
          Type_env.expand_written ~region:type_part.name.region type_env
            type_part.type_alias)
        type_part_data
    in
    let rec after_parameters expected params assumed =
      match (params, assumed) with
      | [], _ | _, [] -> Some expected
      | (param : string Data.Located.t) :: rest, argument :: others -> begin
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
    in
    let result_of_the_annotation =
      Option.map
        (fun expected -> (expected, after_parameters expected body_part.params assumed))
        written
    in
    let typed_body =
      infer_with_env body_part.expr visible_in_the_body
        type_env
    in
    let inferred_type = function_of assumed ~result:typed_body.typ in
    Option.iter
      (fun (whole, result) ->
        let found, expected =
          match result with
          | Some result -> (typed_body.typ, result)
          | None -> (inferred_type, whole)
        in
        against_the_annotation ~region:body_part.expr.region ~name
          ~category:(category_of body_part.expr) ~expected found)
      result_of_the_annotation;
    ( {
        Typed.Declaration.name = body_part.name;
        params =
          List.map2
            (fun name typ -> { Typed.Declaration.name; typ })
            body_part.params assumed;
        body = typed_body;
        typ = inferred_type;
      },
      inferred_type )
  in

  let infer_group ctx group =
    let member (position, (declaration : Canonical.Declaration.t)) =
      let assumed =
        match declaration.type_part_data with
        | Some _ -> None
        | None -> Some (fresh_variable None)
      in
      {
        position;
        declaration;
        member_name =
          Data.Name.local (Data.Located.unwrap declaration.body_part.name);
        assumed;
      }
    in
    let infer_member inside inferred member =
      let typed, checked = infer_declaration member.declaration inside in
      Option.iter
        (fun assumed ->
          matching ~expected:assumed member.declaration.body_part.expr checked)
        member.assumed;
      { member; typed; checked } :: inferred
    in
    let inferred =
      infer_deeper (fun () ->
          let members = List.map member group in
          let inside =
            List.fold_left
              (fun visible member ->
                match member.assumed with
                | None -> visible
                | Some typ ->
                    Value_env.bind member.member_name (Scheme ([], typ)) visible)
              ctx members
          in
          List.fold_left (infer_member inside) [] members)
    in
    let generalized =
      List.fold_left
        (fun visible inferred ->
          Value_env.bind inferred.member.member_name
            (generalize inferred.checked)
            visible)
        ctx inferred
    in
    ( generalized,
      List.rev_map
        (fun inferred -> (inferred.member.position, inferred.typed))
        inferred )
  in

  let announced =
    List.fold_left
      (fun collected (declaration : Canonical.Declaration.t) ->
        match declaration.type_part_data with
        | None -> collected
        | Some annotation ->
            Value_env.bind
              (Data.Name.local (Data.Located.unwrap declaration.body_part.name))
              (generalize
                 (infer_deeper (fun () ->
                      Type_env.expand_written ~region:annotation.name.region
                        type_env annotation.type_alias)))
              collected)
      visible module_.top_declarations
  in

  let final_values, typed_decls, found =
    List.mapi
      (fun position declaration -> (position, declaration))
      module_.top_declarations
    |> Canonicalization.Declaration_graph.in_dependency_order ~declaration:snd
    |> List.fold_left
         (fun (ctx, collected, found) group ->
           match infer_group ctx group with
           | ctx, typed -> (ctx, List.rev_append typed collected, found)
           | exception Reporting.Error.Found error ->
               (ctx, collected, error :: found))
         (announced, [], [])
  in

  let as_declared =
    List.sort (fun (left, _) (right, _) -> Int.compare left right) typed_decls
    |> List.map snd
  in

  zonk
    {
      values = final_values;
      declarations = as_declared;
      siblings_env = Type_env.siblings type_env;
      constructors = Type_env.constructor_infos type_env;
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
