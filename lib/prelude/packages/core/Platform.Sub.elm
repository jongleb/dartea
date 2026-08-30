module Platform.Sub exposing (Sub, batch, map, none)

import List


type alias Watch msg =
    (msg -> ()) -> () -> ()


type Sub msg
    = None
    | Batch (List (Sub msg))
    | Listen String (Watch msg)


none : Sub msg
none =
    None


batch : List (Sub msg) -> Sub msg
batch =
    Batch


map : (a -> msg) -> Sub a -> Sub msg
map tagger subscription =
    case subscription of
        None ->
            None

        Batch subscriptions ->
            Batch (List.map (map tagger) subscriptions)

        Listen key watch ->
            Listen key
                (\dispatch -> watch (\value -> dispatch (tagger value)))
