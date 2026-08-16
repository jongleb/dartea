open OUnit2
module Names = Data.Name.Set
module By_name = Data.Name.Map

type graph = {
  vertices : Data.Name.t list;
  edges : (Data.Name.t * Data.Name.t) list;
}

let vertex index = Data.Name.local (Printf.sprintf "v%d" index)

let edge_map graph =
  List.fold_left
    (fun acc (source, target) ->
      let reached =
        By_name.find_opt source acc |> Option.value ~default:Names.empty
      in
      By_name.add source (Names.add target reached) acc)
    By_name.empty graph.edges

let raw_components graph =
  let edges = edge_map graph in
  let depends_on name =
    By_name.find_opt name edges |> Option.value ~default:Names.empty
  in
  Data.Components.strongly_connected ~name:Fun.id ~depends_on graph.vertices

let components graph = List.map Data.Components.members (raw_components graph)

let successors graph source =
  List.filter_map
    (fun (from, target) ->
      if Data.Name.equal from source then Some target else None)
    graph.edges

let reachable graph source =
  let rec go seen = function
    | [] -> seen
    | current :: rest ->
        if Names.mem current seen then go seen rest
        else go (Names.add current seen) (successors graph current @ rest)
  in
  go Names.empty (successors graph source)

let mutually_reachable graph left right =
  Data.Name.equal left right
  || Names.mem right (reachable graph left)
     && Names.mem left (reachable graph right)

let position_of components name =
  let rec go position = function
    | [] -> None
    | component :: rest ->
        if List.exists (Data.Name.equal name) component then Some position
        else go (position + 1) rest
  in
  go 0 components

let sorted names = List.sort Data.Name.compare names
let normalised components = List.sort compare (List.map sorted components)

let show_names names =
  "[" ^ String.concat " " (List.map Data.Name.to_string names) ^ "]"

let show_components components =
  String.concat " " (List.map show_names components)

let show_raw_components components =
  List.map
    (fun (component : Data.Name.t Data.Components.component) ->
      match component with
      | Acyclic name -> "acyclic " ^ Data.Name.to_string name
      | Cyclic names -> "cyclic " ^ show_names names)
    components
  |> String.concat " "

let print_graph graph =
  Printf.sprintf "vertices %s edges %s" (show_names graph.vertices)
    (String.concat " "
       (List.map
          (fun (from, target) ->
            Data.Name.to_string from ^ "->" ^ Data.Name.to_string target)
          graph.edges))

let assert_components ~expected graph =
  assert_equal ~printer:show_components expected (components graph)

let assert_raw_components ~expected graph =
  assert_equal ~printer:show_raw_components expected (raw_components graph)

let graph_of ~size edges =
  {
    vertices = List.init size vertex;
    edges = List.map (fun (from, target) -> (vertex from, vertex target)) edges;
  }

let test_empty _ = assert_components ~expected:[] (graph_of ~size:0 [])

let test_isolated_vertices _ =
  assert_components ~expected:[ [ vertex 0 ]; [ vertex 1 ] ]
    (graph_of ~size:2 [])

let test_chain_puts_dependencies_first _ =
  assert_components
    ~expected:[ [ vertex 2 ]; [ vertex 1 ]; [ vertex 0 ] ]
    (graph_of ~size:3 [ (0, 1); (1, 2) ])

let test_self_loop _ =
  assert_components ~expected:[ [ vertex 0 ] ] (graph_of ~size:1 [ (0, 0) ])

let test_self_loop_is_cyclic _ =
  assert_raw_components ~expected:[ Cyclic [ vertex 0 ] ]
    (graph_of ~size:1 [ (0, 0) ])

let test_lone_vertex_is_acyclic _ =
  assert_raw_components ~expected:[ Acyclic (vertex 0) ] (graph_of ~size:1 [])

let test_chain_is_all_acyclic _ =
  assert_raw_components
    ~expected:[ Acyclic (vertex 2); Acyclic (vertex 1); Acyclic (vertex 0) ]
    (graph_of ~size:3 [ (0, 1); (1, 2) ])

let test_group_is_cyclic _ =
  assert_raw_components
    ~expected:[ Cyclic [ vertex 0; vertex 1 ] ]
    (graph_of ~size:2 [ (0, 1); (1, 0) ])

let test_two_cycle _ =
  assert_components
    ~expected:[ [ vertex 0; vertex 1 ] ]
    (graph_of ~size:2 [ (0, 1); (1, 0) ])

let test_cycle_with_a_tail _ =
  assert_components
    ~expected:[ [ vertex 2 ]; [ vertex 0; vertex 1 ] ]
    (graph_of ~size:3 [ (0, 1); (1, 0); (1, 2) ])

let test_two_independent_cycles _ =
  assert_components
    ~expected:[ [ vertex 0; vertex 1 ]; [ vertex 2; vertex 3 ] ]
    (graph_of ~size:4 [ (0, 1); (1, 0); (2, 3); (3, 2) ])

let test_diamond _ =
  assert_components
    ~expected:[ [ vertex 3 ]; [ vertex 1 ]; [ vertex 2 ]; [ vertex 0 ] ]
    (graph_of ~size:4 [ (0, 1); (0, 2); (1, 3); (2, 3) ])

let test_repeated_vertices_are_visited_once _ =
  let graph = graph_of ~size:2 [ (0, 1) ] in
  assert_components
    ~expected:[ [ vertex 1 ]; [ vertex 0 ] ]
    { graph with vertices = graph.vertices @ graph.vertices }

let test_a_dependency_outside_the_member_list_is_not_an_edge _ =
  assert_components
    ~expected:[ [ vertex 0 ] ]
    { vertices = [ vertex 0 ]; edges = [ (vertex 0, vertex 1) ] }

let compile src = Dartea.Compiler.compile_source src

let test_value_cycle_is_rejected _ =
  let src = {j|
a : Int
a = b + 1

b : Int
b = a + 1
|j} in
  match compile src with
  | _ -> assert_failure "expected a cycle to be reported"
  | exception Failure message ->
      assert_bool ("unexpected message: " ^ message)
        (Node_runner.contains message ~needle:"cycle"
        && Node_runner.contains message ~needle:"a"
        && Node_runner.contains message ~needle:"b")

let test_function_cycle_is_accepted _ =
  let src = {j|
f : Int -> Int
f x =
    if x <= 0 then
        0
    else
        g (x - 1)

g : Int -> Int
g x =
    f x + 1

result : Int
result = f 3
|j} in
  assert_equal ~printer:(fun s -> s) "3"
    (Node_runner.evaluate ~compiled:(compile src) ~expr:"Main.result")

let test_forward_reference_is_reordered _ =
  let src = {j|
a : Int
a = b + 1

b : Int
b = 2
|j} in
  assert_equal ~printer:(fun s -> s) "3"
    (Node_runner.evaluate ~compiled:(compile src) ~expr:"Main.a")

open QCheck2

let graph_gen =
  let open Gen in
  let* size = int_range 1 8 in
  let* pairs =
    list_size
      (int_range 0 (2 * size))
      (pair (int_range 0 (size - 1)) (int_range 0 (size - 1)))
  in
  return
    {
      vertices = List.init size vertex;
      edges =
        List.sort_uniq compare pairs
        |> List.map (fun (from, target) -> (vertex from, vertex target));
    }

let law_partitions_the_vertices =
  Test.make ~count:1000 ~name:"the components partition the vertices"
    ~print:print_graph graph_gen (fun graph ->
      sorted (List.concat (components graph)) = sorted graph.vertices)

let law_components_are_non_empty =
  Test.make ~count:1000 ~name:"no component is empty" ~print:print_graph
    graph_gen (fun graph -> List.for_all (fun c -> c <> []) (components graph))

let law_component_is_mutual_reachability =
  Test.make ~count:1000
    ~name:"two vertices share a component exactly when mutually reachable"
    ~print:print_graph graph_gen (fun graph ->
      let components = components graph in
      List.for_all
        (fun left ->
          List.for_all
            (fun right ->
              (position_of components left = position_of components right)
              = mutually_reachable graph left right)
            graph.vertices)
        graph.vertices)

let law_dependencies_come_first =
  Test.make ~count:1000
    ~name:"a component is emitted after everything it depends on"
    ~print:print_graph graph_gen (fun graph ->
      let components = components graph in
      List.for_all
        (fun (from, target) ->
          match (position_of components from, position_of components target) with
          | Some user, Some used -> used <= user
          | _ -> false)
        graph.edges)

let law_vertex_order_is_irrelevant =
  Test.make ~count:1000
    ~name:"the set of components does not depend on the order of the vertices"
    ~print:(fun (graph, _) -> print_graph graph)
    (Gen.pair graph_gen Gen.nat_small)
    (fun (graph, offset) ->
      let rec split count items =
        if count = 0 then ([], items)
        else
          match items with
          | [] -> ([], [])
          | item :: rest ->
              let front, back = split (count - 1) rest in
              (item :: front, back)
      in
      let front, back =
        split (offset mod List.length graph.vertices) graph.vertices
      in
      normalised (components graph)
      = normalised (components { graph with vertices = back @ front }))

let law_reversing_edges_keeps_the_components =
  Test.make ~count:1000
    ~name:"the components of a graph and of its transpose are the same"
    ~print:print_graph graph_gen (fun graph ->
      let transposed =
        { graph with edges = List.map (fun (a, b) -> (b, a)) graph.edges }
      in
      normalised (components graph) = normalised (components transposed))

let law_acyclic_exactly_when_not_reachable_from_itself =
  Test.make ~count:1000
    ~name:"a component is acyclic exactly when its vertex cannot reach itself"
    ~print:print_graph graph_gen (fun graph ->
      List.for_all
        (fun (component : Data.Name.t Data.Components.component) ->
          match component with
          | Acyclic name -> not (Names.mem name (reachable graph name))
          | Cyclic names ->
              List.for_all
                (fun name -> Names.mem name (reachable graph name))
                names)
        (raw_components graph))

let law_cyclic_groups_are_never_singletons_without_a_self_edge =
  Test.make ~count:1000
    ~name:"a cyclic component is a real cycle, never a bare vertex"
    ~print:print_graph graph_gen (fun graph ->
      List.for_all
        (fun (component : Data.Name.t Data.Components.component) ->
          match component with
          | Acyclic _ -> true
          | Cyclic [ name ] -> List.mem (name, name) graph.edges
          | Cyclic names -> List.length names > 1)
        (raw_components graph))

let law_a_component_is_an_acyclic_condensation =
  Test.make ~count:1000 ~name:"distinct components never depend on each other"
    ~print:print_graph graph_gen (fun graph ->
      let components = components graph in
      List.for_all
        (fun (from, target) ->
          position_of components from = position_of components target
          || not (mutually_reachable graph from target))
        graph.edges)

let suite =
  [
    "empty" >:: test_empty;
    "isolated_vertices" >:: test_isolated_vertices;
    "chain_puts_dependencies_first" >:: test_chain_puts_dependencies_first;
    "self_loop" >:: test_self_loop;
    "self_loop_is_cyclic" >:: test_self_loop_is_cyclic;
    "lone_vertex_is_acyclic" >:: test_lone_vertex_is_acyclic;
    "chain_is_all_acyclic" >:: test_chain_is_all_acyclic;
    "group_is_cyclic" >:: test_group_is_cyclic;
    "value_cycle_is_rejected" >:: test_value_cycle_is_rejected;
    "function_cycle_is_accepted" >:: test_function_cycle_is_accepted;
    "forward_reference_is_reordered" >:: test_forward_reference_is_reordered;
    "two_cycle" >:: test_two_cycle;
    "cycle_with_a_tail" >:: test_cycle_with_a_tail;
    "two_independent_cycles" >:: test_two_independent_cycles;
    "diamond" >:: test_diamond;
    "repeated_vertices_are_visited_once" >:: test_repeated_vertices_are_visited_once;
    "a_dependency_outside_the_member_list_is_not_an_edge"
    >:: test_a_dependency_outside_the_member_list_is_not_an_edge;
  ]
  @ QCheck_ounit.to_ounit2_test_list
      [
        law_partitions_the_vertices;
        law_components_are_non_empty;
        law_component_is_mutual_reachability;
        law_dependencies_come_first;
        law_vertex_order_is_irrelevant;
        law_reversing_edges_keeps_the_components;
        law_a_component_is_an_acyclic_condensation;
        law_acyclic_exactly_when_not_reachable_from_itself;
        law_cyclic_groups_are_never_singletons_without_a_self_edge;
      ]
