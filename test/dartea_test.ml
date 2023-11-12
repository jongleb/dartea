open OUnit2

let suite =
  "Dartea_test"
  >::: [
         "test_type_aliases" >::: Dartea_test_type_alias.suite;
         "test_types" >::: Dartea_test_type.suite;
         "test_decls" >::: Dartea_test_decl.suite;
       ]

let () = run_test_tt_main suite
