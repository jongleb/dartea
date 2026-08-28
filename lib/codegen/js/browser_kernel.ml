let provided =
  [
    ("VirtualDom", "node", 3);
    ("VirtualDom", "text", 1);
    ("VirtualDom", "attribute", 2);
    ("VirtualDom", "style", 2);
  ]

let arity (name : Data.Name.t) =
  match name with
  | Data.Name.Local _ -> None
  | Data.Name.Global { module_name; exported_name } ->
      List.find_map
        (fun (module_, exported, arity) ->
          if String.equal module_ module_name && String.equal exported exported_name
          then Some arity
          else None)
        provided

let needed_by module_names =
  List.exists
    (fun (module_, _, _) -> List.mem module_ module_names)
    provided

let runtime_name (name : Data.Name.t) =
  match name with
  | Data.Name.Local local -> "$$" ^ local
  | Data.Name.Global { module_name; exported_name } ->
      "$$" ^ module_name ^ "$" ^ exported_name

let reference name =
  Ast.Member
    {
      object_ = Ast.Identifier Runtime.browser_module_name;
      property = Ast.Identifier (runtime_name name);
      computed = false;
    }
