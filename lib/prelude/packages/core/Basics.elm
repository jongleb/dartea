{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/core -- https://github.com/elm/core
   Copyright 2014-present Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Basics exposing
    ( identity, always
    , toFloat, round, floor, ceiling, truncate
    , compare, Order(..), min, max
    , not, xor
    , modBy, remainderBy, negate, abs, clamp, sqrt, logBase, e
    , pi, cos, sin, tan, acos, asin, atan, atan2
    , degrees, radians, turns
    , isNaN, isInfinite
    , composeL, composeR
    , apL, apR
    , Never, never
    )


type Order
    = LT
    | EQ
    | GT


comparisonOf : comparable -> comparable -> Int
comparisonOf =
    Elm.Kernel.Utils.compare


compare : comparable -> comparable -> Order
compare x y =
    let
        ordering =
            comparisonOf x y
    in
    if ordering < 0 then
        LT

    else if ordering == 0 then
        EQ

    else
        GT


min : comparable -> comparable -> comparable
min x y =
    if x < y then
        x

    else
        y


max : comparable -> comparable -> comparable
max x y =
    if x > y then
        x

    else
        y


identity : a -> a
identity x =
    x


always : a -> b -> a
always a b =
    a


toFloat : Int -> Float
toFloat =
    Elm.Kernel.Basics.toFloat


round : Float -> Int
round =
    Elm.Kernel.Basics.round


floor : Float -> Int
floor =
    Elm.Kernel.Basics.floor


ceiling : Float -> Int
ceiling =
    Elm.Kernel.Basics.ceiling


truncate : Float -> Int
truncate =
    Elm.Kernel.Basics.truncate


not : Bool -> Bool
not =
    Elm.Kernel.Basics.not


xor : Bool -> Bool -> Bool
xor =
    Elm.Kernel.Basics.xor


modBy : Int -> Int -> Int
modBy =
    Elm.Kernel.Basics.modBy


remainderBy : Int -> Int -> Int
remainderBy =
    Elm.Kernel.Basics.remainderBy


negate : number -> number
negate n =
    0 - n


abs : number -> number
abs n =
    if n < 0 then
        0 - n

    else
        n


clamp : number -> number -> number -> number
clamp low high number =
    if number < low then
        low

    else if number > high then
        high

    else
        number


sqrt : Float -> Float
sqrt =
    Elm.Kernel.Basics.sqrt


logBase : Float -> Float -> Float
logBase base number =
    Elm.Kernel.Basics.log number / Elm.Kernel.Basics.log base


e : Float
e =
    Elm.Kernel.Basics.e


isNaN : Float -> Bool
isNaN =
    Elm.Kernel.Basics.isNaN


isInfinite : Float -> Bool
isInfinite =
    Elm.Kernel.Basics.isInfinite


radians : Float -> Float
radians angleInRadians =
    angleInRadians


degrees : Float -> Float
degrees angleInDegrees =
    angleInDegrees * pi / 180


turns : Float -> Float
turns angleInTurns =
    2 * pi * angleInTurns


pi : Float
pi =
    Elm.Kernel.Basics.pi


cos : Float -> Float
cos =
    Elm.Kernel.Basics.cos


sin : Float -> Float
sin =
    Elm.Kernel.Basics.sin


tan : Float -> Float
tan =
    Elm.Kernel.Basics.tan


acos : Float -> Float
acos =
    Elm.Kernel.Basics.acos


asin : Float -> Float
asin =
    Elm.Kernel.Basics.asin


atan : Float -> Float
atan =
    Elm.Kernel.Basics.atan


atan2 : Float -> Float -> Float
atan2 =
    Elm.Kernel.Basics.atan2


composeL : (b -> c) -> (a -> b) -> (a -> c)
composeL g f x =
    g (f x)


composeR : (a -> b) -> (b -> c) -> (a -> c)
composeR f g x =
    g (f x)


apR : a -> (a -> b) -> b
apR x f =
    f x


apL : (a -> b) -> a -> b
apL f x =
    f x


type Never
    = JustOneMore Never


never : Never -> a
never nvr =
    case nvr of
        JustOneMore inner ->
            never inner
