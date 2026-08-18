open Typed
open Typed.Type
module By_name = Data.Name.Map

type t = scheme By_name.t

let empty = By_name.empty

let primitives =
  List.fold_left
    (fun collected (name, scheme) ->
      By_name.add (Data.Name.local name) scheme collected)
    empty Primitives.values

let find name env = By_name.find_opt name env
let bind name scheme env = By_name.add name scheme env

let bind_one name ty env =
  By_name.add (Data.Name.local name) (Scheme ([], ty)) env

let shadow ~by env = By_name.union (fun _ _ inner -> Some inner) env by
let zonk env = By_name.map Type.zonk_scheme env

let binders_of_both ~region here there =
  By_name.union
    (fun name _ _ ->
      Reporting.Error.raise_name ~region
        (Reporting.Name_error.Duplicate_binder
           { name = Data.Name.to_string name }))
    here there

let names env = By_name.fold (fun name _ collected -> name :: collected) env []
let schemes env = By_name.fold (fun _ scheme collected -> scheme :: collected) env []
let equal same one other = By_name.equal same one other
