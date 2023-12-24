(* data Type
   = TLambda Type Type
   | TVar Name
   | TType ModuleName.Canonical Name [Type]
   | TRecord (Map.Map Name FieldType) (Maybe Name)
   | TUnit
   | TTuple Type Type (Maybe Type)
   | TAlias ModuleName.Canonical Name [(Name, Type)] AliasType
   deriving (Eq) *)

type t = { parameters : t list; body : kind } [@@deriving show]
and kind = T_concrete of string

(* and concrete =  *)

let of_ast (arg : Frontend.Impl.t) = { parameters = []; body = T_concrete "" }
