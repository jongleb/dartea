{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/random -- https://github.com/elm/random
   Copyright (c) 2018-present, Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Random exposing
  ( Generator, Seed
  , int, float, uniform, weighted, constant
  , list, pair
  , map, map2, map3, map4, map5
  , andThen, lazy
  , minInt, maxInt
  , generate
  , step, initialSeed, independentSeed
  )

import Basics exposing (..)
import Bitwise
import List
import Maybe exposing (Maybe(..))
import Platform.Cmd exposing (Cmd)
import Task
import Tuple


-- PRIMITIVE GENERATORS


int : Int -> Int -> Generator Int
int a b =
    Generator
        (\seed0 ->
            let
                ( lo, hi ) =
                    if a < b then
                        ( a, b )
                    else
                        ( b, a )

                range =
                    hi - lo + 1
            in
                -- fast path for power of 2
                if (Bitwise.and (range - 1) range) == 0 then
                    ( (Bitwise.shiftRightZfBy 0 (Bitwise.and (range - 1) (peel seed0))) + lo, next seed0 )
                else
                    let
                        threshhold =
                            -- essentially: period % max
                            Bitwise.shiftRightZfBy 0 (remainderBy range (Bitwise.shiftRightZfBy 0 -range))

                        accountForBias : Seed -> ( Int, Seed )
                        accountForBias seed =
                            let
                                x =
                                    peel seed

                                seedN =
                                    next seed
                            in
                                if x < threshhold then
                                    -- in practice this recurses almost never
                                    accountForBias seedN
                                else
                                    ( remainderBy range x + lo, seedN )
                    in
                        accountForBias seed0
        )


maxInt : Int
maxInt =
  2147483647


minInt : Int
minInt =
  -2147483648


float : Float -> Float -> Generator Float
float a b =
    Generator (\seed0 ->
        let
            -- Get 64 bits of randomness
            seed1 =
                next seed0

            n0 =
                peel seed0

            n1 =
                peel seed1

            -- Get a uniformly distributed IEEE-754 double between 0.0 and 1.0
            hi =
                toFloat (Bitwise.and 0x03FFFFFF n0) * 1.0

            lo =
                toFloat (Bitwise.and 0x07FFFFFF n1) * 1.0

            val =
                -- These magic constants are 2^27 and 2^53
                ((hi * 134217728.0) + lo) / 9007199254740992.0

            -- Scale it into our range
            range =
                abs (b - a)

            scaled =
                val * range + a
        in
            ( scaled, next seed1 )
        )


constant : a -> Generator a
constant value =
  Generator (\seed -> (value, seed))


-- DATA STRUCTURES


pair : Generator a -> Generator b -> Generator (a,b)
pair genA genB =
  map2 (\a b -> (a,b)) genA genB


list : Int -> Generator a -> Generator (List a)
list n (Generator gen) =
  Generator (\seed ->
    listHelp [] n gen seed
  )


listHelp : List a -> Int -> (Seed -> (a,Seed)) -> Seed -> (List a, Seed)
listHelp revList n gen seed =
  if n < 1 then
    (revList, seed)

  else
    let
      (value, newSeed) =
        gen seed
    in
      listHelp (value :: revList) (n-1) gen newSeed


-- ENUMERATIONS


uniform : a -> List a -> Generator a
uniform value valueList =
  weighted (addOne value) (List.map addOne valueList)


addOne : a -> (Float, a)
addOne value =
  ( 1, value )


weighted : (Float, a) -> List (Float, a) -> Generator a
weighted first others =
  let
    normalize (weight, _) =
      abs weight

    total =
      normalize first + List.sum (List.map normalize others)
  in
  map (getByWeight first others) (float 0 total)


getByWeight : (Float, a) -> List (Float, a) -> Float -> a
getByWeight (weight, value) others countdown =
  case others of
    [] ->
      value

    second :: otherOthers ->
      if countdown <= abs weight then
        value
      else
        getByWeight second otherOthers (countdown - abs weight)


-- CUSTOM GENERATORS


map : (a -> b) -> Generator a -> Generator b
map func (Generator genA) =
  Generator (\seed0 ->
    let
      (a, seed1) = genA seed0
    in
      (func a, seed1)
  )


map2 : (a -> b -> c) -> Generator a -> Generator b -> Generator c
map2 func (Generator genA) (Generator genB) =
  Generator (\seed0 ->
    let
      (a, seed1) = genA seed0
      (b, seed2) = genB seed1
    in
      (func a b, seed2)
  )


map3 : (a -> b -> c -> d) -> Generator a -> Generator b -> Generator c -> Generator d
map3 func (Generator genA) (Generator genB) (Generator genC) =
  Generator (\seed0 ->
    let
      (a, seed1) = genA seed0
      (b, seed2) = genB seed1
      (c, seed3) = genC seed2
    in
      (func a b c, seed3)
  )


map4 : (a -> b -> c -> d -> e) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e
map4 func (Generator genA) (Generator genB) (Generator genC) (Generator genD) =
  Generator (\seed0 ->
    let
      (a, seed1) = genA seed0
      (b, seed2) = genB seed1
      (c, seed3) = genC seed2
      (d, seed4) = genD seed3
    in
      (func a b c d, seed4)
  )


map5 : (a -> b -> c -> d -> e -> f) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator f
map5 func (Generator genA) (Generator genB) (Generator genC) (Generator genD) (Generator genE) =
  Generator (\seed0 ->
    let
      (a, seed1) = genA seed0
      (b, seed2) = genB seed1
      (c, seed3) = genC seed2
      (d, seed4) = genD seed3
      (e, seed5) = genE seed4
    in
      (func a b c d e, seed5)
  )


andThen : (a -> Generator b) -> Generator a -> Generator b
andThen callback (Generator genA) =
  Generator (\seed ->
    let
      (result, newSeed) = genA seed
      (Generator genB) = callback result
    in
      genB newSeed
  )


lazy : (() -> Generator a) -> Generator a
lazy callback =
  Generator (\seed ->
    let
      (Generator gen) = callback ()
    in
      gen seed
  )


-- IMPLEMENTATION

{- Explanation of the PCG algorithm

    This is a special variation (dubbed RXS-M-SH) that produces 32
    bits of output by keeping 32 bits of state. There is one function
    (next) to derive the following state and another (peel) to obtain 32
    psuedo-random bits from the current state.

    Getting the next state is easy: multiply by a magic factor, modulus by 2^32,
    and then add an increment. If two seeds have different increments,
    their random numbers from the two seeds will never match up; they are
    completely independent. This is very helpful for isolated components or
    multithreading, and elm-test relies on this feature.

    Transforming a seed into 32 random bits is more complicated, but
    essentially you use the "most random" bits to pick some way of scrambling
    the remaining bits. Beyond that, see section 6.3.4 of the [paper].

    [paper](http://www.pcg-random.org/paper.html)

    Once we have 32 random bits, we have to turn it into a number. For integers,
    we first check if the range is a power of two. If it is, we can mask part of
    the value and be done. If not, we need to account for bias.

    Let's say you want a random number between 1 and 7 but I can only generate
    random numbers between 1 and 32. If I modulus by result by 7, I'm biased,
    because there are more random numbers that lead to 1 than 7. So instead, I
    check to see if my random number exceeds 28 (the largest multiple of 7 less
    than 32). If it does, I reroll, otherwise I mod by seven. This sounds
    wateful, except that instead of 32 it's 2^32, so in practice it's hard to
    notice. So that's how we get random ints. There's another process from
    floats, but I don't understand it very well.
-}


type Seed
    = Seed Int Int
    -- the first Int is the state of the RNG and stepped with each random generation
    -- the second state is the increment which corresponds to an independent RNG


-- step the RNG to produce the next seed
-- this is incredibly simple: multiply the state by a constant factor, modulus it
-- by 2^32, and add a magic addend. The addend can be varied to produce independent
-- RNGs, so it is stored as part of the seed. It is given to the new seed unchanged.
next : Seed -> Seed
next (Seed state0 incr) =
    -- The magic constant is from Numerical Recipes and is inlined for perf.
    Seed (Bitwise.shiftRightZfBy 0 ((state0 * 1664525) + incr)) incr


-- obtain a psuedorandom 32-bit integer from a seed
peel : Seed -> Int
peel (Seed state _) =
    -- This is the RXS-M-SH version of PCG, see section 6.3.4 of the paper
    -- and line 184 of pcg_variants.h in the 0.94 (non-minimal) C implementation,
    -- the latter of which is the source of the magic constant.
    let
        word =
            (Bitwise.xor state (Bitwise.shiftRightZfBy ((Bitwise.shiftRightZfBy 28 state) + 4) state)) * 277803737
    in
        Bitwise.shiftRightZfBy 0 (Bitwise.xor (Bitwise.shiftRightZfBy 22 word) word)


type Generator a =
    Generator (Seed -> (a, Seed))


step : Generator a -> Seed -> (a, Seed)
step (Generator generator) seed =
  generator seed


initialSeed : Int -> Seed
initialSeed x =
    let
        -- the default increment magic constant is taken from Numerical Recipes
        (Seed state1 incr) =
            next (Seed 0 1013904223)

        state2 =
            Bitwise.shiftRightZfBy 0 (state1 + x)
    in
        next (Seed state2 incr)


independentSeed : Generator Seed
independentSeed =
    Generator <|
        \seed0 ->
            let
                gen =
                    int 0 0xFFFFFFFF

                {--
                Although it probably doesn't hold water theoretically, xor two
                random numbers to make an increment less likely to be
                pathological. Then make sure that it's odd, which is required.
                Next make sure it is positive. Finally step it once before use.
                --}
                makeIndependentSeed state b c =
                    next <| Seed state <|
                        Bitwise.shiftRightZfBy 0 (Bitwise.or 1 (Bitwise.xor b c))
            in
            step (map3 makeIndependentSeed gen gen gen) seed0


-- MANAGER


generate : (a -> msg) -> Generator a -> Cmd msg
generate tagger (Generator generator) =
  Task.perform tagger (Elm.Kernel.Random.generate initialSeed generator)
