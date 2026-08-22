module Result exposing
    ( Result(..)
    , withDefault, map, map2, map3, map4, map5
    , andThen, toMaybe, fromMaybe, mapError
    )

import Basics exposing (..)
import Maybe exposing (Maybe(..))


type Result error value
    = Ok value
    | Err error


withDefault : a -> Result x a -> a
withDefault def result =
    case result of
        Ok a ->
            a

        Err _ ->
            def


map : (a -> value) -> Result x a -> Result x value
map func ra =
    case ra of
        Ok a ->
            Ok (func a)

        Err e ->
            Err e


map2 : (a -> b -> value) -> Result x a -> Result x b -> Result x value
map2 func ra rb =
    case ra of
        Err x ->
            Err x

        Ok a ->
            case rb of
                Err x ->
                    Err x

                Ok b ->
                    Ok (func a b)


map3 :
    (a -> b -> c -> value)
    -> Result x a
    -> Result x b
    -> Result x c
    -> Result x value
map3 func ra rb rc =
    case ra of
        Err x ->
            Err x

        Ok a ->
            case rb of
                Err x ->
                    Err x

                Ok b ->
                    case rc of
                        Err x ->
                            Err x

                        Ok c ->
                            Ok (func a b c)


map4 :
    (a -> b -> c -> d -> value)
    -> Result x a
    -> Result x b
    -> Result x c
    -> Result x d
    -> Result x value
map4 func ra rb rc rd =
    map2 (\g d -> g d) (map3 func ra rb rc) rd


map5 :
    (a -> b -> c -> d -> e -> value)
    -> Result x a
    -> Result x b
    -> Result x c
    -> Result x d
    -> Result x e
    -> Result x value
map5 func ra rb rc rd re =
    map2 (\g e -> g e) (map4 func ra rb rc rd) re


andThen : (a -> Result x b) -> Result x a -> Result x b
andThen callback result =
    case result of
        Ok value ->
            callback value

        Err msg ->
            Err msg


mapError : (x -> y) -> Result x a -> Result y a
mapError f result =
    case result of
        Ok v ->
            Ok v

        Err e ->
            Err (f e)


toMaybe : Result x a -> Maybe a
toMaybe result =
    case result of
        Ok v ->
            Just v

        Err _ ->
            Nothing


fromMaybe : x -> Maybe a -> Result x a
fromMaybe err maybe =
    case maybe of
        Just v ->
            Ok v

        Nothing ->
            Err err
