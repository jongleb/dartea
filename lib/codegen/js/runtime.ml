let module_name = "Dartea_runtime"
let source = Runtime_source.dartea_runtime
let browser_module_name = "Dartea_browser"
let browser_source = Runtime_source.dartea_browser
let json_module_name = "Dartea_json"
let json_source = Runtime_source.dartea_json
let mount = "$$mount"
let tag = "TAG"
let block = "block"
let head = "hd"
let tail = "tl"
let payload index = "_" ^ string_of_int index

let form index = "$$form" ^ string_of_int index
let refresher index = "$$r" ^ string_of_int index
let aim index = "$$aim" ^ string_of_int index
let carried index = "a" ^ string_of_int index
let block_state = "$$b"
let put = "$$put"
let carrier = "$$v"
let item = "$$i"
let deps = "deps"

let flag_prim = "$$flagPrim"
let flag_maybe = "$$flagMaybe"
let flag_list = "$$flagList"
let flag_tuple = "$$flagTuple"
let flag_record = "$$flagRecord"
let port_list = "$$portList"
let raw = "$$raw"
let port_label = "port"
let port_where name = port_label ^ " " ^ name

let engine_source = Runtime_source.dartea_engine
let curry = "$$curry"
let widest_apply = 9
let apply index = "$$apply" ^ string_of_int index
let apply1 = apply 1
let apply2 = apply 2
let append = "$$append"
let equal = "$$eq"
let compare = "$$cmp"
let mod_by = "$$modBy"
let char_to_code = "$$charToCode"
let char_from_code = "$$charFromCode"
let string_to_list = "$$stringToList"
let string_from_list = "$$stringFromList"
let list_map = "$$listMap"
let list_filter = "$$listFilter"
let list_reverse = "$$listReverse"
let list_length = "$$listLength"
let string_split = "$$stringSplit"

let files =
  [
    (module_name, source);
    (browser_module_name, browser_source);
    (json_module_name, json_source);
  ]

let all =
  (curry :: List.init widest_apply (fun index -> apply (index + 1)))
  @ [
      append; equal; compare; mod_by; char_to_code; char_from_code;
      string_to_list; string_from_list; string_split; list_map; list_filter; list_reverse; list_length;
    ]