let module_name = "Dartea_runtime"
let source = Runtime_source.dartea_runtime
let browser_module_name = "Dartea_browser"
let browser_source = Runtime_source.dartea_browser
let json_module_name = "Dartea_json"
let json_source = Runtime_source.dartea_json
let mount = "$$mount"
let engine_source = Runtime_source.dartea_engine
let curry = "$$curry"
let append = "$$append"
let equal = "$$eq"
let compare = "$$cmp"
let mod_by = "$$modBy"
let char_to_code = "$$charToCode"
let char_from_code = "$$charFromCode"
let string_to_list = "$$stringToList"
let string_from_list = "$$stringFromList"
let string_split = "$$stringSplit"

let all =
  [
    curry; append; equal; compare; mod_by; char_to_code; char_from_code;
    string_to_list; string_from_list; string_split;
  ]
