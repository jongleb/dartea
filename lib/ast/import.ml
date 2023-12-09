type privacy = Public of Data.Located.region | Private
type upper = { name : string Data.Located.t; privacy : privacy }
type operator = { name : string; region : Data.Located.region }

type exposed =
  | Lower of string Data.Located.t
  | Upper of upper
  | Operator of operator

type exposing = Open | Explicit of exposed list

type t = {
  name : string Data.Located.t;
  alias : string option;
  exposing : exposing;
}
