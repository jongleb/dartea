module Task exposing
    ( Task, andThen, attempt, fail, map, map2, map3, map4, map5, mapError
    , onError, perform, sequence, succeed
    )

import Basics exposing (..)
import List
import Platform.Cmd exposing (Cmd)
import Result exposing (Result)


type Task x a
    = Task


succeed : a -> Task x a
succeed =
    Elm.Kernel.Task.succeed


fail : x -> Task x a
fail =
    Elm.Kernel.Task.fail


andThen : (a -> Task x b) -> Task x a -> Task x b
andThen =
    Elm.Kernel.Task.andThen


onError : (x -> Task y a) -> Task x a -> Task y a
onError =
    Elm.Kernel.Task.onError


map : (a -> b) -> Task x a -> Task x b
map func task =
    andThen (\a -> succeed (func a)) task


map2 : (a -> b -> result) -> Task x a -> Task x b -> Task x result
map2 func one other =
    andThen (\a -> andThen (\b -> succeed (func a b)) other) one


map3 :
    (a -> b -> c -> result)
    -> Task x a
    -> Task x b
    -> Task x c
    -> Task x result
map3 func one other third =
    andThen (\a -> map2 (func a) other third) one


map4 :
    (a -> b -> c -> d -> result)
    -> Task x a
    -> Task x b
    -> Task x c
    -> Task x d
    -> Task x result
map4 func one other third fourth =
    andThen (\a -> map3 (func a) other third fourth) one


map5 :
    (a -> b -> c -> d -> e -> result)
    -> Task x a
    -> Task x b
    -> Task x c
    -> Task x d
    -> Task x e
    -> Task x result
map5 func one other third fourth fifth =
    andThen (\a -> map4 (func a) other third fourth fifth) one


mapError : (x -> y) -> Task x a -> Task y a
mapError convert task =
    onError (\reason -> fail (convert reason)) task


sequence : List (Task x a) -> Task x (List a)
sequence tasks =
    List.foldr (map2 (\value gathered -> value :: gathered)) (succeed []) tasks


perform : (a -> msg) -> Task Never a -> Cmd msg
perform =
    Elm.Kernel.Task.perform


attempt : (Result x a -> msg) -> Task x a -> Cmd msg
attempt =
    Elm.Kernel.Task.attempt
