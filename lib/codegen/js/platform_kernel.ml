type home = Browser | Json

module Kernel_module = struct
  type t =
    | Browser
    | Dom
    | Json
    | Port
    | Task
    | Time
    | Url
    | VirtualDom
  [@@deriving to_string]
end

let table =
  [
    (Browser, Kernel_module.VirtualDom, "node", 3);
    (Browser, Kernel_module.VirtualDom, "keyedNode", 3);
    (Browser, Kernel_module.VirtualDom, "nodeNS", 4);
    (Browser, Kernel_module.VirtualDom, "keyedNodeNS", 4);
    (Browser, Kernel_module.VirtualDom, "attributeNS", 3);
    (Browser, Kernel_module.VirtualDom, "text", 1);
    (Browser, Kernel_module.VirtualDom, "attribute", 2);
    (Browser, Kernel_module.VirtualDom, "property", 2);
    (Browser, Kernel_module.VirtualDom, "style", 2);
    (Browser, Kernel_module.VirtualDom, "on", 2);
    (Browser, Kernel_module.VirtualDom, "lazy", 2);
    (Browser, Kernel_module.VirtualDom, "lazy2", 3);
    (Browser, Kernel_module.VirtualDom, "lazy3", 4);
    (Browser, Kernel_module.VirtualDom, "lazy4", 5);
    (Browser, Kernel_module.VirtualDom, "lazy5", 6);
    (Browser, Kernel_module.VirtualDom, "lazy6", 7);
    (Browser, Kernel_module.VirtualDom, "lazy7", 8);
    (Browser, Kernel_module.VirtualDom, "lazy8", 9);
    (Browser, Kernel_module.VirtualDom, "map", 2);
    (Browser, Kernel_module.VirtualDom, "mapAttribute", 2);
    (Browser, Kernel_module.Browser, "sandbox", 1);
    (Browser, Kernel_module.Browser, "document", 1);
    (Browser, Kernel_module.Browser, "element", 1);
    (Browser, Kernel_module.Browser, "application", 2);
    (Browser, Kernel_module.Browser, "pushUrl", 2);
    (Browser, Kernel_module.Browser, "replaceUrl", 2);
    (Browser, Kernel_module.Browser, "go", 2);
    (Browser, Kernel_module.Browser, "load", 1);
    (Browser, Kernel_module.Browser, "reload", 1);
    (Browser, Kernel_module.Browser, "on", 3);
    (Browser, Kernel_module.Browser, "onAnimationFrame", 1);
    (Browser, Kernel_module.Browser, "onAnimationFrameDelta", 1);
    (Browser, Kernel_module.Url, "percentEncode", 1);
    (Browser, Kernel_module.Url, "percentDecode", 1);
    (Browser, Kernel_module.Task, "succeed", 1);
    (Browser, Kernel_module.Task, "fail", 1);
    (Browser, Kernel_module.Task, "andThen", 2);
    (Browser, Kernel_module.Task, "onError", 2);
    (Browser, Kernel_module.Task, "perform", 2);
    (Browser, Kernel_module.Task, "attempt", 2);
    (Browser, Kernel_module.Dom, "focus", 1);
    (Browser, Kernel_module.Time, "every", 2);
    (Browser, Kernel_module.Port, Data.Kernel.Port.string_of_direction Outgoing, 2);
    (Browser, Kernel_module.Port, Data.Kernel.Port.string_of_direction Incoming, 2);
    (Json, Kernel_module.Json, "isString", 1);
    (Json, Kernel_module.Json, "isBool", 1);
    (Json, Kernel_module.Json, "isNumber", 1);
    (Json, Kernel_module.Json, "isInt", 1);
    (Json, Kernel_module.Json, "isNull", 1);
    (Json, Kernel_module.Json, "isArray", 1);
    (Json, Kernel_module.Json, "isObject", 1);
    (Json, Kernel_module.Json, "hasField", 2);
    (Json, Kernel_module.Json, "unsafeField", 2);
    (Json, Kernel_module.Json, "length", 1);
    (Json, Kernel_module.Json, "unsafeIndex", 2);
    (Json, Kernel_module.Json, "identity", 1);
    (Json, Kernel_module.Json, "stringify", 1);
    (Json, Kernel_module.Json, "isValid", 1);
    (Json, Kernel_module.Json, "unsafeParse", 1);
    (Json, Kernel_module.Json, "emptyArray", 0);
    (Json, Kernel_module.Json, "pushed", 2);
    (Json, Kernel_module.Json, "emptyObject", 0);
    (Json, Kernel_module.Json, "withField", 3);
  ]

let found (name : Data.Name.t) =
  match name with
  | Data.Name.Local _ -> None
  | Data.Name.Global { module_name; exported_name } ->
      List.find_opt
        (fun (_, module_, exported, _) ->
          String.equal (Kernel_module.to_string module_) module_name
          && String.equal exported exported_name)
        table

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
