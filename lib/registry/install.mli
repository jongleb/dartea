val run :
  Fpath.t -> say:(string -> unit) -> Packages.Pick.t list

val add :
  Fpath.t -> package:string -> say:(string -> unit) -> Packages.Pick.t list
