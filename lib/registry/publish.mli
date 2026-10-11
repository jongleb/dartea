type mode = Publish | Rehearse

type outcome =
  | Published of Packages.Manifest.listing
  | Rehearsed of Packages.Manifest.listing
  | Npm_failed of int

val run : Fpath.t -> modules:string list -> mode -> outcome
