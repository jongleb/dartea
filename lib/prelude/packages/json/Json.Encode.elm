module Json.Encode exposing
    ( Value, bool, encode, float, int, list, null, object, string )

import Basics exposing (..)
import List
import Tuple


type Value
    = Value


string : String -> Value
string =
    Elm.Kernel.Json.identity


int : Int -> Value
int =
    Elm.Kernel.Json.identity


float : Float -> Value
float =
    Elm.Kernel.Json.identity


bool : Bool -> Value
bool =
    Elm.Kernel.Json.identity


null : Value
null =
    Elm.Kernel.Json.unsafeParse "null"


list : (a -> Value) -> List a -> Value
list encoder items =
    List.foldl (\item built -> Elm.Kernel.Json.pushed (encoder item) built)
        Elm.Kernel.Json.emptyArray
        items


object : List ( String, Value ) -> Value
object pairs =
    List.foldl
        (\pair built ->
            Elm.Kernel.Json.withField (Tuple.first pair) (Tuple.second pair) built
        )
        Elm.Kernel.Json.emptyObject
        pairs


encode : Int -> Value -> String
encode =
    Elm.Kernel.Json.stringify
