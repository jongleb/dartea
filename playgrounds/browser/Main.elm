module Main exposing (main)


greeting : String
greeting =
    "compiled by dartea"


main : String
main =
    String.fromInt (2 + 3) ++ " — " ++ greeting
