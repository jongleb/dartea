module Browser.Dom exposing (Error(..), focus)

import Task exposing (Task)


type Error
    = NotFound String


focus : String -> Task Error ()
focus =
    Elm.Kernel.Dom.focus
