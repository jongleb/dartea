type t

val load :
  provided:string list -> Fpath.t -> (t, Diagnostic.Failure.t) result

val of_list : Elm_file.t list -> t
val files : t -> Elm_file.t list
