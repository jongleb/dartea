module Module_map = struct
  open Base

  module T = struct
    type t = string [@@deriving sexp, compare]
  end

  include T
  include Base.Comparable.Make (T)
end

let compile path =
  File_loader.Files.(
    path |> current_folder
    |> List.map (fun Elm_file.{ path; content } ->
           Ast.Kind.Frontend.Module.(
             content |> Parse.Main.parse |> Result.map of_impl
             |> Result.map (fun module_ ->
                    (Data.Located.unwrap module_.name, module_))))
    |> Base.Result.combine_errors)
  |> Result.map (Base.Map.of_alist (module Module_map))
