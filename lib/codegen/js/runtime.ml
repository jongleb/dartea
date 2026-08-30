let module_name = "Dartea_runtime"
let source = Runtime_source.dartea_runtime
let browser_module_name = "Dartea_browser"
let browser_source = Runtime_source.dartea_browser
let json_module_name = "Dartea_json"
let json_source = Runtime_source.dartea_json
let mount = "$$mount"
let tag = "TAG"
let block = "block"
let form index = "$$form" ^ string_of_int index
let put = "$$put"
let refresher index = "$$r" ^ string_of_int index
let refresh_args = "$$a"
let block_state = "$$b"
let deps = "deps"
let head = "hd"
let tail = "tl"
let payload index = "_" ^ string_of_int index
let engine_source = Runtime_source.dartea_engine
let curry = "$$curry"
let apply1 = "$$apply1"
let apply2 = "$$apply2"
let append = "$$append"
let equal = "$$eq"
let compare = "$$cmp"
let mod_by = "$$modBy"
let char_to_code = "$$charToCode"
let char_from_code = "$$charFromCode"
let string_to_list = "$$stringToList"
let string_from_list = "$$stringFromList"
let string_split = "$$stringSplit"

let files =
  [
    (module_name, source);
    (browser_module_name, browser_source);
    (json_module_name, json_source);
  ]

let all =
  [
    curry; apply1; apply2; append; equal; compare; mod_by; char_to_code; char_from_code;
    string_to_list; string_from_list; string_split;
  ]
