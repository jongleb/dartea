module J = Ast
module Scope = Data.Name.Map

type t = {
  scope : string Scope.t;
  names : Names.t;
  instances : Instances.t;
  forms : Forms.t option;
  home : (Data.Name.t * string list) option;
}


let temp env = Names.temp env.names

let bind_one env src =
  let js = Names.fresh env.names (Names.of_name src) in
  ({ env with scope = Scope.add src js env.scope }, js)

let jid_env env src =
  match Scope.find_opt src env.scope with
  | Some js -> J.Identifier js
  | None -> Names.expression_of src


let declared_arity env name =
  if Scope.mem name env.scope then None else Names.arity_of env.names name

let bind_binds env binds =
  let env, rev =
    List.fold_left
      (fun (env, acc) (src, occ) ->
        let env, js = bind_one env src in
        (env, J.ConstDecl { name = js; init = occ } :: acc))
      (env, []) binds
  in
  (env, List.rev rev)

let bind_params env names =
  let env, rev =
    List.fold_left
      (fun (env, acc) src ->
        let env, js = bind_one env src in
        (env, js :: acc))
      (env, []) names
  in
  (env, List.rev rev)

