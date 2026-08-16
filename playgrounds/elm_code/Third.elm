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


fib n =
    let
        go i a b = if i == 0 then a else go (i - 1) b (a + b)
    in
    go n 0 1

fact n = if n <= 1 then 1 else n * fact (n - 1)

sumDigits n = if n < 10 then n else rem n 10 + sumDigits (quot n 10)

digitCount n = if n < 10 then 1 else 1 + digitCount (quot n 10)

reverseInt n =
    let
        go m acc = if m == 0 then acc else go (quot m 10) (acc * 10 + rem m 10)
    in
    go n 0

isPalindromeInt n = reverseInt n == n

isPrime n =
    let
        go d =
            if d * d > n then True
            else if divides d n then False
            else go (d + 1)
    in
    if n < 2 then False else go 2

collatzSteps n =
    let
        go m acc =
            if m <= 1 then acc
            else if even m then go (quot m 2) (acc + 1)
            else go (3 * m + 1) (acc + 1)
    in
    go n 0

ack m n =
    if m == 0 then n + 1
    else if n == 0 then ack (m - 1) 1
    else ack (m - 1) (ack m (n - 1))

foldRange lo hi step acc =
    if lo > hi then acc else foldRange (lo + 1) hi step (step lo acc)

sumTo n = foldRange 1 n (\i acc -> i + acc) 0
countPrimes n = foldRange 2 n (\i acc -> if isPrime i then acc + 1 else acc) 0


mapMaybe f m =
    case m of
        Just v -> Just (f v)
        Nothing -> Nothing

andThen f m =
    case m of
        Just v -> f v
        Nothing -> Nothing

withDefault d m =
    case m of
        Just v -> v
        Nothing -> d

orElse fallback m =
    case m of
        Just v -> Just v
        Nothing -> fallback

isJust m =
    case m of
        Just _ -> True
        Nothing -> False

map2 f a b = andThen (\x -> mapMaybe (f x) b) a

keepIf p m = andThen (\v -> if p v then Just v else Nothing) m

safeDiv a b = if b == 0 then Nothing else Just (quot a b)

addStrings a b = map2 (\x y -> x + y) (String.toInt a) (String.toInt b)


type Expr
    = Lit Int
    | Neg Expr
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr

eval e =
    case e of
        Lit n -> Just n
        Neg a -> mapMaybe neg (eval a)
        Add a b -> map2 (\x y -> x + y) (eval a) (eval b)
        Sub a b -> map2 (\x y -> x - y) (eval a) (eval b)
        Mul a b -> map2 (\x y -> x * y) (eval a) (eval b)
        Div a b -> andThen (\y -> andThen (\x -> safeDiv x y) (eval a)) (eval b)

binop l op r = "(" ++ l ++ " " ++ op ++ " " ++ r ++ ")"

show e =
    case e of
        Lit n -> String.fromInt n
        Neg a -> "(0 - " ++ show a ++ ")"
        Add a b -> binop (show a) "+" (show b)
        Sub a b -> binop (show a) "-" (show b)
        Mul a b -> binop (show a) "*" (show b)
        Div a b -> binop (show a) "/" (show b)

size e =
    case e of
        Lit _ -> 1
        Neg a -> 1 + size a
        Add a b -> 1 + size a + size b
        Sub a b -> 1 + size a + size b
        Mul a b -> 1 + size a + size b
        Div a b -> 1 + size a + size b

depth e =
    case e of
        Lit _ -> 1
        Neg a -> 1 + depth a
        Add a b -> 1 + max (depth a) (depth b)
        Sub a b -> 1 + max (depth a) (depth b)
        Mul a b -> 1 + max (depth a) (depth b)
        Div a b -> 1 + max (depth a) (depth b)

simplify e =
    case e of
        Add a b ->
            case ( simplify a, simplify b ) of
                ( Lit 0, sb ) -> sb
                ( sa, Lit 0 ) -> sa
                ( Lit x, Lit y ) -> Lit (x + y)
                ( sa, sb ) -> Add sa sb

        Mul a b ->
            case ( simplify a, simplify b ) of
                ( Lit 0, _ ) -> Lit 0
                ( _, Lit 0 ) -> Lit 0
                ( Lit 1, sb ) -> sb
                ( sa, Lit 1 ) -> sa
                ( Lit x, Lit y ) -> Lit (x * y)
                ( sa, sb ) -> Mul sa sb

        Sub a b ->
            case ( simplify a, simplify b ) of
                ( sa, Lit 0 ) -> sa
                ( Lit x, Lit y ) -> Lit (x - y)
                ( sa, sb ) -> Sub sa sb

        Neg a ->
            case simplify a of
                Lit x -> Lit (neg x)
                sa -> Neg sa

        Div a b -> Div (simplify a) (simplify b)
        Lit n -> Lit n

expr1 = Add (Mul (Lit 1) (Lit 21)) (Sub (Lit 21) (Lit 0))
expr2 = Div (Lit 10) (Sub (Lit 3) (Lit 3))
expr3 = Mul (Add (Lit 2) (Lit 3)) (Neg (Lit 4))


type Res e a
    = Ok a
    | Err e

mapRes f r =
    case r of
        Ok a -> Ok (f a)
        Err e -> Err e

andThenRes f r =
    case r of
        Ok a -> f a
        Err e -> Err e

resToMaybe r =
    case r of
        Ok a -> Just a
        Err _ -> Nothing

maybeToRes e m =
    case m of
        Just a -> Ok a
        Nothing -> Err e

evalR e =
    case e of
        Lit n -> Ok n
        Neg a -> mapRes neg (evalR a)
        Add a b -> andThenRes (\x -> mapRes (\y -> x + y) (evalR b)) (evalR a)
        Sub a b -> andThenRes (\x -> mapRes (\y -> x - y) (evalR b)) (evalR a)
        Mul a b -> andThenRes (\x -> mapRes (\y -> x * y) (evalR b)) (evalR a)
        Div a b ->
            andThenRes
                (\y ->
                    if y == 0 then
                        Err ("div by zero in " ++ show e)
                    else
                        mapRes (\x -> quot x y) (evalR a)
                )
                (evalR b)

showRes r =
    case r of
        Ok n -> "ok " ++ String.fromInt n
        Err msg -> "err " ++ msg


vec x y = Tuple.pair x y
vx v = Tuple.first v
vy v = Tuple.second v

vadd a b = vec (vx a + vx b) (vy a + vy b)
vsub a b = vec (vx a - vx b) (vy a - vy b)
vscale k v = vec (k * vx v) (k * vy v)
vdot a b = vx a * vx b + vy a * vy b
vlen2 v = vdot v v
manhattan a b = abs (vx a - vx b) + abs (vy a - vy b)
vshow v = "(" ++ String.fromInt (vx v) ++ ", " ++ String.fromInt (vy v) ++ ")"

swap t = Tuple.pair (Tuple.second t) (Tuple.first t)


nil = \_ z -> z
cons x xs = \f z -> f x (xs f z)
foldr f z xs = xs f z

lmap f xs = \g z -> xs (\x acc -> g (f x) acc) z
lfilter p xs = \g z -> xs (\x acc -> if p x then g x acc else acc) z

lsum xs = foldr (\x acc -> x + acc) 0 xs
llen xs = foldr (\_ acc -> acc + 1) 0 xs
lall p xs = foldr (\x acc -> p x && acc) True xs
lany p xs = foldr (\x acc -> p x || acc) False xs

lrange lo hi = if lo > hi then nil else cons lo (lrange (lo + 1) hi)

ljoin sep xs =
    foldr (\x acc -> if acc == "" then x else x ++ sep ++ acc) "" xs

lshow xs = "[" ++ ljoin ", " (lmap String.fromInt xs) ++ "]"

primesTo n = lfilter isPrime (lrange 2 n)
squares n = lmap (\x -> x * x) (lrange 1 n)


fizzbuzz n =
    if divides 15 n then "FizzBuzz"
    else if divides 3 n then "Fizz"
    else if divides 5 n then "Buzz"
    else String.fromInt n

fizzbuzzTo n = ljoin " " (lmap fizzbuzz (lrange 1 n))


polyTest =
    let
        i = identity
        k = always
    in
    Tuple.pair (Tuple.pair (i 1) (i "str")) (Tuple.pair (k True "x") (k "y" 42))


line label value = label ++ " = " ++ value

report =
    ljoin
        "; "
        (cons (line "fib 25" (String.fromInt (fib 25)))
            (cons (line "fact 10" (String.fromInt (fact 10)))
                (cons (line "gcd 462 1071" (String.fromInt (gcd 462 1071)))
                    (cons (line "2^16" (String.fromInt (powFast 2 16)))
                        (cons (line "collatz 27" (String.fromInt (collatzSteps 27)))
                            (cons (line "primes<100" (String.fromInt (countPrimes 100)))
                                (cons (line "ack 2 3" (String.fromInt (ack 2 3)))
                                    (cons (line "sumDigits 987654" (String.fromInt (sumDigits 987654)))
                                        (cons (line "palindrome 12321" (String.fromInt (reverseInt 12321)))
                                            nil
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )

demoExpr1 = show (simplify expr1) ++ " -> " ++ (withDefault 0 (eval expr1) |> String.fromInt)
demoExpr2 = showRes (evalR expr2)
demoExpr3 = show (simplify expr3)

demoVec = vshow (vadd (vec 3 4) (vscale 2 (vec 1 1)))
demoList = lshow (squares 10) ++ " sum=" ++ String.fromInt (lsum (squares 10))
demoPrimes = lshow (primesTo 50)
demoFizz = fizzbuzzTo 20
demoMaybe = withDefault 0 (addStrings "40" "2")
demoChain = "7" |> String.toInt |> mapMaybe (\x -> x * 6) |> withDefault 0 |> String.fromInt