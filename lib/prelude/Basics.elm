module Basics exposing
    ( identity, always
    , negate
    , composeL, composeR
    , apL, apR
    )


identity : a -> a
identity x =
    x


always : a -> b -> a
always a b =
    a


negate : Int -> Int
negate n =
    0 - n


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
