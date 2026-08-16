type t = Exactly of int | At_least of int

let of_type (typ : Optimized.Type.t) : t =
  let rec through arrows (typ : Optimized.Type.t) =
    match typ with
    | Optimized.Type.TFun (_, result) -> through (arrows + 1) result
    | Optimized.Type.TVar _ -> At_least arrows
    | Optimized.Type.TInt | Optimized.Type.TBool | Optimized.Type.TStr
    | Optimized.Type.TUnit | Optimized.Type.TTup _ | Optimized.Type.TCustom _
    | Optimized.Type.TRecord _ | Optimized.Type.TRowExtend _
    | Optimized.Type.TRowEmpty ->
        Exactly arrows
  in
  through 0 typ
