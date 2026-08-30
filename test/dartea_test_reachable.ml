open OUnit2

let measured folder =
  Reachable.shown folder
    (Reachable.measure
       (Sample.delivered ~delivery:Dartea.Delivery.default
          (Sample.compiled_in ~entry:None
             (Filename.concat Sample.playground_root folder))))

let expected =
  [
    "bench 53/55 declarations, 11999/12194 bytes";
    "browser 2/2 declarations, 92/92 bytes";
    "comparison 92/98 declarations, 8852/9937 bytes";
    "counter 13/13 declarations, 1825/1825 bytes";
    "crossmod 10/10 declarations, 951/951 bytes";
    "currying 18/18 declarations, 1361/1361 bytes";
    "elm_code 128/128 declarations, 22702/22702 bytes";
    "fib 4/4 declarations, 530/530 bytes";
    "http 100/105 declarations, 18763/19069 bytes";
    "spa 117/122 declarations, 32420/32726 bytes";
    "todomvc 147/147 declarations, 26368/26368 bytes";
  ]

let test_playgrounds _ =
  assert_equal ~printer:(String.concat "\n")
    (List.sort String.compare expected)
    (List.sort String.compare (List.map measured Sample.playgrounds))

let names (blocks : Codegen_js.Shake.block list) =
  List.map (fun (block : Codegen_js.Shake.block) -> block.name) blocks

let counted source =
  List.length
    (List.filter
       (fun line -> String.starts_with ~prefix:"const " line)
       (String.split_on_char '\n' source))

let opening = [ "let "; "var "; "function "; "class " ]

let test_runtimes_survive_a_total_shake _ =
  List.iter
    (fun (module_name, source) ->
      let file = Codegen_js.Shake.parse source in
      List.iter
        (fun line ->
          List.iter
            (fun keyword ->
              assert_bool
                (module_name ^ " has a top-level `" ^ String.trim keyword
               ^ "` the shaker cannot see: " ^ line)
                (not (String.starts_with ~prefix:keyword line)))
            opening)
        (String.split_on_char '\n' source);
      assert_equal ~printer:string_of_int
        ~msg:(module_name ^ " has helpers the split did not find")
        (counted source) (List.length file.blocks);
      assert_equal ~printer:Sample.names
        ~msg:(module_name ^ " lost helpers when everything is a root")
        (names file.blocks)
        (names
           (Codegen_js.Shake.parse
              (Codegen_js.Shake.alive ~roots:(names file.blocks) source))
           .blocks);
      List.iter
        (fun exported ->
          assert_bool
            (module_name ^ " exports " ^ exported ^ ", which is not a helper")
            (List.mem exported (names file.blocks)))
        file.exported)
    Codegen_js.Runtime.files

let helpers source = names (Codegen_js.Shake.parse source).blocks

let test_every_declared_kernel_exists _ =
  let language = helpers Codegen_js.Runtime.source in
  List.iter
    (fun helper ->
      assert_bool
        (helper ^ " is declared in Runtime but missing from the runtime file")
        (List.mem helper language))
    Codegen_js.Runtime.all;
  List.iter
    (fun (home, module_name, exported_name, _) ->
      let spelled =
        "$$" ^ Codegen_js.Platform_kernel.Kernel_module.to_string module_name ^ "$" ^ exported_name
      in
      assert_bool
        (spelled ^ " is in the kernel table but missing from its runtime file")
        (List.mem spelled
           (helpers (snd (Codegen_js.Platform_kernel.module_of home)))))
    Codegen_js.Platform_kernel.table

let suite =
  [
    "playgrounds" >:: test_playgrounds;
    "every_declared_kernel_exists" >:: test_every_declared_kernel_exists;
    "runtimes_survive_a_total_shake" >:: test_runtimes_survive_a_total_shake;
  ]
