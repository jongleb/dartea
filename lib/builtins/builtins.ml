let values : (string * Typed.Type.scheme) list =
  Primitives.values @ Prelude_temp.values

let types : Canonical.Typedecl.t list = Prelude_temp.types
