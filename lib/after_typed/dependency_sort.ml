module O = Optimized
module Names = Scope.Names

type error = Bad_recursion of Data.Name.t list

let show_error (Bad_recursion names) =
  Printf.sprintf "these definitions depend on each other in a cycle: %s"
    (String.concat ", " (List.map Data.Name.to_string names))

let sort_declarations (decls : O.Declaration.t list) :
    (O.Declaration.t list, error) result =
  let name (d : O.Declaration.t) = Data.Name.local (Data.Located.unwrap d.name) in
  let declared = Names.of_list (List.map name decls) in
  let depends_on d = Names.inter (Scope.free_in_declaration d) declared in
  let evaluated_before_use (d : O.Declaration.t) =
    match d.params with
    | _ :: _ -> false
    | [] -> begin
        match d.body.expr with O.Expr.Expr_lambda _ -> false | _ -> true
      end
  in
  let rec go ordered = function
    | [] -> Ok (List.rev ordered)
    | Data.Components.Acyclic d :: rest -> go (d :: ordered) rest
    | Data.Components.Cyclic grouped :: rest ->
        if List.exists evaluated_before_use grouped then
          Error (Bad_recursion (List.map name grouped))
        else go (List.rev_append grouped ordered) rest
  in
  go [] (Data.Components.strongly_connected ~name ~depends_on decls)
