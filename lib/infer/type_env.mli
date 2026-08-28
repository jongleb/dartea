type t
type ctor_info = { name : Data.Name.t; arity : int; index : int; total : int }

val build : imports:Interface.t list -> Canonical.Module.t -> t

val expand_written :
  region:Data.Region.t -> t -> Canonical.Typedef.Impl.t -> Typed.Type.t

val constructor_of :
  Data.Name.t ->
  t ->
  (Canonical.Typedecl.t * Canonical.Typedecl.type_ctor) option

val constructor_scheme :
  t -> Canonical.Typedecl.t -> Canonical.Typedecl.type_ctor -> Typed.Type.scheme

val constructor_values : t -> Value_env.t
val constructor_infos : t -> ctor_info list
val siblings : t -> (Data.Name.t * int) list Data.Name.Map.t
val typedecls : t -> Canonical.Typedecl.t list

val typedecl_payloads :
  Canonical.Typedecl.t ->
  Typed.Type.t Typed.Variable.t list
  * (Data.Name.t * Typed.Type.t list) list
