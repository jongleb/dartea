type item = Value of string | Type of { name : string; ctors_exposed : bool }
[@@deriving show]

type t = All | Only of item list [@@deriving show]

let of_frontend (exposing : Frontend.Exposing.t) =
  match exposing with
  | Frontend.Exposing.Open -> All
  | Frontend.Exposing.Explicit items ->
      Only
        (List.map
           (function
             | Frontend.Exposing.Lower name -> Value (Data.Located.unwrap name)
             | Frontend.Exposing.Upper { name; privacy } ->
                 let ctors_exposed =
                   match privacy with
                   | Frontend.Exposing.Public _ -> true
                   | Frontend.Exposing.Private -> false
                 in
                 Type { name = Data.Located.unwrap name; ctors_exposed })
           items)
