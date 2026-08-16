module Currying exposing (report)


add : Int -> Int -> Int
add a b =
    a + b


mul : Int -> Int -> Int
mul a b =
    a * b


saturated : Int
saturated =
    add 1 2


partial : Int
partial =
    let
        inc =
            add 1
    in
    inc 5 + inc 6


twice : (Int -> Int) -> Int -> Int
twice f n =
    f (f n)


concreteHigherOrder : Int
concreteHigherOrder =
    twice (add 1) 5


bodyIsALambda : Int -> Int -> Int
bodyIsALambda a =
    \b -> a - b


sharesWorkThenReturns : Int -> Int -> Int
sharesWorkThenReturns n =
    let
        doubled =
            n + n
    in
    \m -> doubled + m


callsAtTwo : (Int -> Int -> Int) -> Int
callsAtTwo f =
    f 3 4


etaExpanded : Int
etaExpanded =
    callsAtTwo bodyIsALambda + callsAtTwo sharesWorkThenReturns


computedCallee : Bool -> (Int -> Int) -> (Int -> Int) -> Int -> Int
computedCallee c f g n =
    (if c then f else g) n


fromARecord : { go : Int -> Int } -> Int -> Int
fromARecord handlers n =
    handlers.go n


notAnIdentifier : Int
notAnIdentifier =
    computedCallee True (add 1) (mul 2) 5 + fromARecord { go = add 7 } 5


overApplied : Int
overApplied =
    identity add 3 4


nestedLambda : Int
nestedLambda =
    callsAtTwo (\a -> \b -> a + b)


apply : (a -> b) -> a -> b
apply f x =
    f x


polymorphicParameter : Int
polymorphicParameter =
    apply (add 3) 4 + apply add 3 4


type Boxed
    = Boxed (Int -> Int -> Int)


unbox : Boxed -> Int
unbox b =
    case b of
        Boxed f ->
            f 1 2


keepGeneric : a -> Maybe a
keepGeneric g =
    Just g


throughAGenericSlot : Int
throughAGenericSlot =
    case keepGeneric add of
        Just f ->
            f 1 2 + unbox (Boxed mul)

        Nothing ->
            0


line : String -> Int -> String
line name value =
    name ++ "=" ++ String.fromInt value


report : String
report =
    line "saturated" saturated
        ++ "; "
        ++ line "partial" partial
        ++ "; "
        ++ line "concreteHigherOrder" concreteHigherOrder
        ++ "; "
        ++ line "etaExpanded" etaExpanded
        ++ "; "
        ++ line "notAnIdentifier" notAnIdentifier
        ++ "; "
        ++ line "overApplied" overApplied
        ++ "; "
        ++ line "nestedLambda" nestedLambda
        ++ "; "
        ++ line "polymorphicParameter" polymorphicParameter
        ++ "; "
        ++ line "throughAGenericSlot" throughAGenericSlot
