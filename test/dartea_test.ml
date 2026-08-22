open OUnit2

let suite =
  "Dartea_test"
  >::: [
         "test_decls" >::: Dartea_test_decl.suite;
         (* "test_exprs" >::: Dartea_test_decl_expr.suite; *)
         "test_type_aliases" >::: Dartea_test_type_alias.suite;
         "test_types" >::: Dartea_test_type.suite;
         "test_type_system" >::: Dartea_test_type_system.suite;
         "test_indent" >::: Dartea_test_indent.suite;
         "test_layout_laws" >::: Dartea_test_layout_laws.suite;
         "test_region_laws" >::: Dartea_test_region_laws.suite;
         "test_reports" >::: Dartea_test_reports.suite;
         "test_codegen" >::: Dartea_test_codegen.suite;
         "test_emitted" >::: Dartea_test_emitted.suite;
         "test_project" >::: Dartea_test_project.suite;
         "test_exhaustive" >::: Dartea_test_exhaustive.suite;
         "test_frontend_module" >::: Dartea_test_frontend_module.suite;
         "test_canonical_module" >::: Dartea_test_canonical_module.suite;
         "test_scope" >::: Dartea_test_scope.suite;
         "test_declaration_graph" >::: Dartea_test_declaration_graph.suite;
         "test_resolve_names" >::: Dartea_test_resolve_names.suite;
         "test_interface" >::: Dartea_test_interface.suite;
         "test_crossmod" >::: Dartea_test_crossmod.suite;
         "test_c_runtime" >::: Dartea_test_c_runtime.suite;
         "test_ir" >::: Dartea_test_ir.suite;
         "test_dependency_sort" >::: Dartea_test_dependency_sort.suite;
         "test_infer_laws" >::: Dartea_test_infer_laws.suite;
         "test_mutation" >::: Dartea_test_mutation.suite;
         (* "test_infer" >::: Dartea_infer_test.suite; *)
       ]

let () = run_test_tt_main suite
