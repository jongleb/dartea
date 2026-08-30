module Platform.Cmd exposing (Cmd, batch, map, none)

import List


type alias Effect msg =
    (msg -> ()) -> ()


type Cmd msg
    = None
    | Batch (List (Cmd msg))
    | Perform (Effect msg)


none : Cmd msg
none =
    None


batch : List (Cmd msg) -> Cmd msg
batch =
    Batch


map : (a -> msg) -> Cmd a -> Cmd msg
map tagger command =
    case command of
        None ->
            None

        Batch commands ->
            Batch (List.map (map tagger) commands)

        Perform run ->
            Perform (\dispatch -> run (\value -> dispatch (tagger value)))
