module Fib exposing (fib, fibUpTo, main)


fib : Int -> Int
fib n =
    let
        go i a b =
            if i == 0 then
                a

            else
                go (i - 1) b (a + b)
    in
    go n 0 1


fibUpTo : Int -> String
fibUpTo n =
    let
        go i acc =
            if i > n then
                acc

            else if acc == "" then
                go (i + 1) (String.fromInt (fib i))

            else
                go (i + 1) (acc ++ " " ++ String.fromInt (fib i))
    in
    go 0 ""


main : String
main =
    fibUpTo 15 ++ " | fib 20 = " ++ String.fromInt (fib 20)
