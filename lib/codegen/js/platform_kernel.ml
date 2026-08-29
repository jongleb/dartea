type home = Browser | Json

let provided =
  [
    (Browser, "VirtualDom", "node", 3);
    (Browser, "VirtualDom", "keyedNode", 3);
    (Browser, "VirtualDom", "nodeNS", 4);
    (Browser, "VirtualDom", "keyedNodeNS", 4);
    (Browser, "VirtualDom", "attributeNS", 3);
    (Browser, "VirtualDom", "text", 1);
    (Browser, "VirtualDom", "attribute", 2);
    (Browser, "VirtualDom", "property", 2);
    (Browser, "VirtualDom", "style", 2);
    (Browser, "VirtualDom", "on", 2);
    (Browser, "VirtualDom", "lazy", 2);
    (Browser, "VirtualDom", "lazy2", 3);
    (Browser, "VirtualDom", "lazy3", 4);
    (Browser, "VirtualDom", "lazy4", 5);
    (Browser, "VirtualDom", "lazy5", 6);
    (Browser, "VirtualDom", "lazy6", 7);
    (Browser, "VirtualDom", "lazy7", 8);
    (Browser, "VirtualDom", "lazy8", 9);
    (Browser, "VirtualDom", "map", 2);
    (Browser, "VirtualDom", "mapAttribute", 2);
    (Browser, "Browser", "sandbox", 1);
    (Browser, "Browser", "document", 1);
    (Browser, "Task", "succeed", 1);
    (Browser, "Task", "fail", 1);
    (Browser, "Task", "andThen", 2);
    (Browser, "Task", "onError", 2);
    (Browser, "Task", "perform", 2);
    (Browser, "Task", "attempt", 2);
    (Browser, "Dom", "focus", 1);
    (Browser, "Time", "every", 2);
    (Json, "Json", "isString", 1);
    (Json, "Json", "isBool", 1);
    (Json, "Json", "isNumber", 1);
    (Json, "Json", "isInt", 1);
    (Json, "Json", "isNull", 1);
    (Json, "Json", "isArray", 1);
    (Json, "Json", "isObject", 1);
    (Json, "Json", "hasField", 2);
    (Json, "Json", "unsafeField", 2);
    (Json, "Json", "length", 1);
    (Json, "Json", "unsafeIndex", 2);
    (Json, "Json", "identity", 1);
    (Json, "Json", "stringify", 1);
    (Json, "Json", "isValid", 1);
    (Json, "Json", "unsafeParse", 1);
    (Json, "Json", "emptyArray", 0);
    (Json, "Json", "pushed", 2);
    (Json, "Json", "emptyObject", 0);
    (Json, "Json", "withField", 3);
  ]

let found (name : Data.Name.t) =
  match name with
  | Data.Name.Local _ -> None
  | Data.Name.Global { module_name; exported_name } ->
      List.find_opt
        (fun (_, module_, exported, _) ->
          String.equal module_ module_name && String.equal exported exported_name)
        provided

let arity name = Option.map (fun (_, _, _, arity) -> arity) (found name)

let module_of home =
  match home with
  | Browser -> (Runtime.browser_module_name, Runtime.browser_source)
  | Json -> (Runtime.json_module_name, Runtime.json_source)

let homes used =
  List.filter
    (fun home ->
      List.exists
        (fun name ->
          match found name with
          | Some (living, _, _, _) -> living = home
          | None -> false)
        used)
    [ Browser; Json ]

let runtimes used = List.map module_of (homes used)
let module_names = List.map (fun home -> fst (module_of home)) [ Browser; Json ]

let runtime_name (name : Data.Name.t) =
  match name with
  | Data.Name.Local local -> "$$" ^ local
  | Data.Name.Global { module_name; exported_name } ->
      "$$" ^ module_name ^ "$" ^ exported_name

let reference name =
  let home =
    match found name with
    | Some (living, _, _, _) -> living
    | None -> Browser
  in
  Ast.Member
    {
      object_ = Ast.Identifier (fst (module_of home));
      property = Ast.Identifier (runtime_name name);
      computed = false;
    }
