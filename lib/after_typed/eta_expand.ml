module O = Optimized

let unused_names ~taken =
  let untaken name = not (Scope.Names.mem (Data.Name.local name) taken) in
  Seq.ints 1
  |> Seq.map (fun index -> "eta" ^ string_of_int index)
  |> Seq.filter untaken

let rec lambdas_merged (e : O.Expr.t) : O.Expr.t =
  let e = Subexpressions.transform e ~f:lambdas_merged in
  match e.expr with
  | O.Expr.Expr_lambda { params; body = { expr = O.Expr.Expr_lambda inner; _ } }
    when Scope.Names.disjoint
           (Scope.bound_by_lambda params)
           (Scope.bound_by_lambda inner.params) ->
      let merged =
        O.Expr.Expr_lambda
          { params = params @ inner.params; body = inner.body }
      in
      { e with expr = merged }
  | _ -> e

let body_lambda_merged (decl : O.Declaration.t) : O.Declaration.t =
  match decl.body.expr with
  | O.Expr.Expr_lambda { params; body } ->
      let parameter (p : O.Expr.expr_lambda_param) =
        { O.Declaration.name = p.name; typ = p.typ }
      in
      { decl with params = decl.params @ List.map parameter params; body }
  | _ -> decl

let declaration_arity (decl : O.Declaration.t) =
  let decl = body_lambda_merged decl in
  let from_kernel =
    match (decl.params, decl.body.expr) with
    | [], O.Expr.Expr_kernel (Kernel_value kernel) -> Data.Kernel.arity kernel
    | _, _ -> 0
  in
  List.length decl.params + from_kernel

let saturated (decl : O.Declaration.t) : O.Declaration.t =
  let decl = body_lambda_merged { decl with body = lambdas_merged decl.body } in
  let provided = declaration_arity decl in
  if O.Type.arrows decl.typ <= provided then decl
  else
    let taken =
      Scope.Names.union
        (Scope.free_in_declaration decl)
        (Scope.bound_by_declaration decl)
    in
    let missing =
      O.Type.result_after ~applied:provided decl.typ
      |> O.Type.parameters |> List.to_seq
    in
    let added = Seq.zip (unused_names ~taken) missing |> List.of_seq in
    let parameter (name, typ) =
      { O.Declaration.name = Data.Located.at decl.name.region name; typ }
    in
    let applied_to (fn : O.Expr.t) (name, typ) =
      let argument =
        { O.Expr.typ; expr = O.Expr.Expr_ident (Data.Name.local name) }
      in
      {
        O.Expr.typ = O.Type.result_after ~applied:1 fn.typ;
        expr = O.Expr.Expr_apply { fn; arg = argument };
      }
    in
    {
      decl with
      params = decl.params @ List.map parameter added;
      body = List.fold_left applied_to decl.body added;
    }

let saturate = List.map saturated
