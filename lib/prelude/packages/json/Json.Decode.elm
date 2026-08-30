{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/json -- https://github.com/elm/json
   Copyright 2014-present Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Json.Decode exposing
    ( Decoder, Error(..), Value
    , andThen, at, bool, decodeString, decodeValue, errorToString, fail, field
    , float, index, int, lazy, list, map, map2, map3, map4, map5, map6, map7
    , map8, maybe, null, nullable, oneOf, oneOrMore, string, succeed, value
    )

import Basics exposing (..)
import Json.Encode
import List
import Maybe exposing (Maybe(..))
import Result exposing (Result(..))
import String


type alias Value =
    Json.Encode.Value


type Decoder a
    = Decoder (Value -> Result Error a)


type Error
    = Field String Error
    | Index Int Error
    | OneOf (List Error)
    | Failure String Value


isString : Value -> Bool
isString =
    Elm.Kernel.Json.isString


isBool : Value -> Bool
isBool =
    Elm.Kernel.Json.isBool


isNumber : Value -> Bool
isNumber =
    Elm.Kernel.Json.isNumber


isWhole : Value -> Bool
isWhole =
    Elm.Kernel.Json.isInt


isNull : Value -> Bool
isNull =
    Elm.Kernel.Json.isNull


isArray : Value -> Bool
isArray =
    Elm.Kernel.Json.isArray


isObject : Value -> Bool
isObject =
    Elm.Kernel.Json.isObject


hasField : String -> Value -> Bool
hasField =
    Elm.Kernel.Json.hasField


rawField : String -> Value -> Value
rawField =
    Elm.Kernel.Json.unsafeField


rawLength : Value -> Int
rawLength =
    Elm.Kernel.Json.length


rawIndex : Int -> Value -> Value
rawIndex =
    Elm.Kernel.Json.unsafeIndex


asString : Value -> String
asString =
    Elm.Kernel.Json.identity


asBool : Value -> Bool
asBool =
    Elm.Kernel.Json.identity


asInt : Value -> Int
asInt =
    Elm.Kernel.Json.identity


asFloat : Value -> Float
asFloat =
    Elm.Kernel.Json.identity


shown : Value -> String
shown value =
    Json.Encode.encode 0 value


isValidJson : String -> Bool
isValidJson =
    Elm.Kernel.Json.isValid


parsed : String -> Value
parsed =
    Elm.Kernel.Json.unsafeParse


run : Decoder a -> Value -> Result Error a
run decoder input =
    case decoder of
        Decoder decode ->
            decode input


decodeValue : Decoder a -> Value -> Result Error a
decodeValue =
    run


decodeString : Decoder a -> String -> Result Error a
decodeString decoder text =
    if isValidJson text then
        run decoder (parsed text)

    else
        Err (Failure "This is not valid JSON!" (parsed "null"))


succeed : a -> Decoder a
succeed given =
    Decoder (\_ -> Ok given)


fail : String -> Decoder a
fail message =
    Decoder (\input -> Err (Failure message input))


value : Decoder Value
value =
    Decoder (\input -> Ok input)


primitive : (Value -> Bool) -> (Value -> a) -> String -> Decoder a
primitive fits take expected =
    Decoder
        (\input ->
            if fits input then
                Ok (take input)

            else
                Err (Failure ("Expecting " ++ expected) input)
        )


string : Decoder String
string =
    primitive isString asString "a STRING"


bool : Decoder Bool
bool =
    primitive isBool asBool "a BOOL"


int : Decoder Int
int =
    primitive isWhole asInt "an INT"


float : Decoder Float
float =
    primitive isNumber asFloat "a FLOAT"


null : a -> Decoder a
null given =
    Decoder
        (\input ->
            if isNull input then
                Ok given

            else
                Err (Failure "Expecting null" input)
        )


map : (a -> b) -> Decoder a -> Decoder b
map change decoder =
    Decoder (\input -> Result.map change (run decoder input))


map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 combine first second =
    Decoder
        (\input ->
            case run first input of
                Ok one ->
                    Result.map (combine one) (run second input)

                Err error ->
                    Err error
        )


map3 : (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c -> Decoder d
map3 combine first second third =
    map2 (\apply given -> apply given) (map2 combine first second) third


andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen next decoder =
    Decoder
        (\input ->
            case run decoder input of
                Ok given ->
                    run (next given) input

                Err error ->
                    Err error
        )


field : String -> Decoder a -> Decoder a
field name decoder =
    Decoder
        (\input ->
            if isObject input && hasField name input then
                case run decoder (rawField name input) of
                    Ok given ->
                        Ok given

                    Err error ->
                        Err (Field name error)

            else
                Err (Failure ("Expecting an OBJECT with a field named `" ++ name ++ "`") input)
        )


at : List String -> Decoder a -> Decoder a
at names decoder =
    List.foldr field decoder names


index : Int -> Decoder a -> Decoder a
index wanted decoder =
    Decoder
        (\input ->
            if isArray input && wanted < rawLength input then
                case run decoder (rawIndex wanted input) of
                    Ok given ->
                        Ok given

                    Err error ->
                        Err (Index wanted error)

            else
                Err (Failure "Expecting a longer ARRAY" input)
        )


items : Decoder a -> Value -> Int -> List a -> Result Error (List a)
items decoder input at_ collected =
    if at_ < 0 then
        Ok collected

    else
        case run decoder (rawIndex at_ input) of
            Ok given ->
                items decoder input (at_ - 1) (given :: collected)

            Err error ->
                Err (Index at_ error)


list : Decoder a -> Decoder (List a)
list decoder =
    Decoder
        (\input ->
            if isArray input then
                items decoder input (rawLength input - 1) []

            else
                Err (Failure "Expecting a LIST" input)
        )


tried : List (Decoder a) -> Value -> List Error -> Result Error a
tried decoders input failures =
    case decoders of
        [] ->
            Err (OneOf (List.reverse failures))

        decoder :: rest ->
            case run decoder input of
                Ok given ->
                    Ok given

                Err error ->
                    tried rest input (error :: failures)


oneOf : List (Decoder a) -> Decoder a
oneOf decoders =
    Decoder (\input -> tried decoders input [])


maybe : Decoder a -> Decoder (Maybe a)
maybe decoder =
    Decoder
        (\input ->
            case run decoder input of
                Ok given ->
                    Ok (Just given)

                Err _ ->
                    Ok Nothing
        )


nullable : Decoder a -> Decoder (Maybe a)
nullable decoder =
    oneOf [ null Nothing, map Just decoder ]


indented : String -> String
indented text =
    "    " ++ text


errorToString : Error -> String
errorToString error =
    case error of
        Field name inner ->
            "At field `" ++ name ++ "`:\n" ++ indented (errorToString inner)

        Index spot inner ->
            "At index " ++ String.fromInt spot ++ ":\n" ++ indented (errorToString inner)

        OneOf failures ->
            case failures of
                [] ->
                    "Ran into a Json.Decode.oneOf with no possibilities!"

                _ ->
                    "I ran into the following problems:\n"
                        ++ String.join "\n" (List.map errorToString failures)

        Failure message given ->
            message ++ ", but instead got: " ++ shown given


map4 :
    (a -> b -> c -> d -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder value
map4 func one other third fourth =
    andThen (\a -> map3 (func a) other third fourth) one


map5 :
    (a -> b -> c -> d -> e -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder value
map5 func one other third fourth fifth =
    andThen (\a -> map4 (func a) other third fourth fifth) one


map6 :
    (a -> b -> c -> d -> e -> f -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
    -> Decoder value
map6 func one other third fourth fifth sixth =
    andThen (\a -> map5 (func a) other third fourth fifth sixth) one


map7 :
    (a -> b -> c -> d -> e -> f -> g -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
    -> Decoder g
    -> Decoder value
map7 func one other third fourth fifth sixth seventh =
    andThen (\a -> map6 (func a) other third fourth fifth sixth seventh) one


map8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
    -> Decoder g
    -> Decoder h
    -> Decoder value
map8 func one other third fourth fifth sixth seventh eighth =
    andThen (\a -> map7 (func a) other third fourth fifth sixth seventh eighth)
        one


lazy : (() -> Decoder a) -> Decoder a
lazy thunk =
    andThen thunk (succeed ())


oneOrMore : (a -> List a -> value) -> Decoder a -> Decoder value
oneOrMore func decoder =
    andThen (oneOrMoreHelp func) (list decoder)


oneOrMoreHelp : (a -> List a -> value) -> List a -> Decoder value
oneOrMoreHelp func values =
    case values of
        [] ->
            fail "a ARRAY with at least ONE element"

        first :: rest ->
            succeed (func first rest)
