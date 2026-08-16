module Basics exposing
    ( identity, always
    , negate
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
