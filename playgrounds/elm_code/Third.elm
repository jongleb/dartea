module Playground exposing (..)

flip f = \a b -> f b a
compose f g = \x -> f (g x)
twice f = compose f f
thrice f = compose f (twice f)
on f g = \a b -> f (g a) (g b)
apply f x = f x


neg x = 0 - x
abs x = if x < 0 then neg x else x
sign x = if x < 0 then neg 1 else if x == 0 then 0 else 1

min a b = if a < b then a else b
max a b = if a > b then a else b
clamp lo hi x = min hi (max lo x)

quot a b = a / b
rem a b = a - quot a b * b
divides d n = rem n d == 0
even n = divides 2 n
odd n = even n == False

gcd a b = if b == 0 then abs a else gcd b (rem a b)
lcm a b = quot (abs (a * b)) (gcd a b)

powFast base e =
    if e == 0 then
        1
    else
        let
            half = powFast base (quot e 2)
            sq = half * half
        in
        if even e then sq else sq * base
