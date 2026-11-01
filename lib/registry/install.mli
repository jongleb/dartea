type t =
  | Locked of Packages.Pick.t list
  | Fetched of { file : string; packages : Packages.Pick.t list }

val run :
  Fpath.t -> provided:string list -> say:(string -> unit) -> t

val add :
  Fpath.t -> package:string -> say:(string -> unit) -> Packages.Pick.t list
