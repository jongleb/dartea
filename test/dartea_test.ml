open OUnit2

(* FIXME: SUPER DIRTY HACK . REMOVE ME SOMEWHEN*)
let () = Data.Located._is_loc_empty_mode := true

let suite =
  "Dartea_test"
  >::: [
         "test_decls" >::: Dartea_test_decl.suite;
         (* "test_exprs" >::: Dartea_test_decl_expr.suite; *)
         (* "test_type_aliases" >::: Dartea_test_type_alias.suite;
                   "test_types" >::: Dartea_test_type.suite;
         *)
         (* "test_frontend_module" >::: Dartea_test_frontend_module.suite; *)
         (* "test_infer" >::: Dartea_infer_test.suite; *)
       ]

let () = run_test_tt_main suite
