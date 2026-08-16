let by ~key all =
  let table = Hashtbl.create (2 * List.length all) in
  List.iter (fun value -> Hashtbl.replace table (key value) value) all;
  table
