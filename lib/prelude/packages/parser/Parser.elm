{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/parser -- https://github.com/elm/parser
   Copyright (c) 2017-present, Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Parser exposing
  ( Parser, run
  , int, float, number, symbol, keyword, variable, end
  , succeed, (|=), (|.), lazy, andThen, problem
  , oneOf, map, backtrackable, commit, token
  , sequence, Trailing(..), loop, Step(..)
  , spaces, lineComment, multiComment, Nestable(..)
  , getChompedString, chompIf, chompWhile, chompUntil, chompUntilEndOr, mapChompedString
  , DeadEnd, Problem(..), deadEndsToString
  , withIndent, getIndent
  , getPosition, getRow, getCol, getOffset, getSource
  )

import Basics exposing (..)
import List
import Maybe exposing (Maybe(..))
import Result exposing (Result(..))
import String
import Tuple
import Char
import Parser.Advanced as A exposing ((|=), (|.))
import Set


-- INFIX OPERATORS - see Parser.Advanced for why 5 and 6 were chosen


infix left 5 (|=) = keeper
infix left 6 (|.) = ignorer


-- PARSERS


type alias Parser a =
  A.Parser Never Problem a


-- RUN


run : Parser a -> String -> Result (List DeadEnd) a
run parser source =
  case A.run parser source of
    Ok a ->
      Ok a

    Err problems ->
      Err (List.map problemToDeadEnd problems)


problemToDeadEnd : A.DeadEnd Never Problem -> DeadEnd
problemToDeadEnd p =
  DeadEnd p.row p.col p.problem


-- PROBLEMS


type alias DeadEnd =
  { row : Int
  , col : Int
  , problem : Problem
  }


type Problem
  = Expecting String
  | ExpectingInt
  | ExpectingHex
  | ExpectingOctal
  | ExpectingBinary
  | ExpectingFloat
  | ExpectingNumber
  | ExpectingVariable
  | ExpectingSymbol String
  | ExpectingKeyword String
  | ExpectingEnd
  | UnexpectedChar
  | Problem String
  | BadRepeat


deadEndsToString : List DeadEnd -> String
deadEndsToString deadEnds =
  "TODO deadEndsToString"


-- PIPELINES


succeed : a -> Parser a
succeed =
  A.succeed


keeper : Parser (a -> b) -> Parser a -> Parser b
keeper =
  (|=)


ignorer : Parser keep -> Parser ignore -> Parser keep
ignorer =
  (|.)


lazy : (() -> Parser a) -> Parser a
lazy =
  A.lazy


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen =
  A.andThen


problem : String -> Parser a
problem msg =
  A.problem (Problem msg)


-- BACKTRACKING


oneOf : List (Parser a) -> Parser a
oneOf =
  A.oneOf


map : (a -> b) -> Parser a -> Parser b
map =
  A.map


backtrackable : Parser a -> Parser a
backtrackable =
  A.backtrackable


commit : a -> Parser a
commit =
  A.commit


-- TOKEN


token : String -> Parser ()
token str =
  A.token (toToken str)


toToken : String -> A.Token Problem
toToken str =
  A.Token str (Expecting str)


-- LOOPS


loop : state -> (state -> Parser (Step state a)) -> Parser a
loop state callback =
  A.loop state (\s -> map toAdvancedStep (callback s))


type Step state a
  = Loop state
  | Done a


toAdvancedStep : Step s a -> A.Step s a
toAdvancedStep step =
  case step of
    Loop s -> A.Loop s
    Done a -> A.Done a


-- NUMBERS


int : Parser Int
int =
  A.int ExpectingInt ExpectingInt


float : Parser Float
float =
  A.float ExpectingFloat ExpectingFloat


-- NUMBER


number
  : { int : Maybe (Int -> a)
    , hex : Maybe (Int -> a)
    , octal : Maybe (Int -> a)
    , binary : Maybe (Int -> a)
    , float : Maybe (Float -> a)
    }
  -> Parser a
number i =
  A.number
    { int = Result.fromMaybe ExpectingInt i.int
    , hex = Result.fromMaybe ExpectingHex i.hex
    , octal = Result.fromMaybe ExpectingOctal i.octal
    , binary = Result.fromMaybe ExpectingBinary i.binary
    , float = Result.fromMaybe ExpectingFloat i.float
    , invalid = ExpectingNumber
    , expecting = ExpectingNumber
    }


-- SYMBOL


symbol : String -> Parser ()
symbol str =
  A.symbol (A.Token str (ExpectingSymbol str))


-- KEYWORD


keyword : String -> Parser ()
keyword kwd =
  A.keyword (A.Token kwd (ExpectingKeyword kwd))


-- END


end : Parser ()
end =
  A.end ExpectingEnd


-- CHOMPED STRINGS


getChompedString : Parser a -> Parser String
getChompedString =
  A.getChompedString


mapChompedString : (String -> a -> b) -> Parser a -> Parser b
mapChompedString =
  A.mapChompedString


chompIf : (Char -> Bool) -> Parser ()
chompIf isGood =
  A.chompIf isGood UnexpectedChar


chompWhile : (Char -> Bool) -> Parser ()
chompWhile =
  A.chompWhile


chompUntil : String -> Parser ()
chompUntil str =
  A.chompUntil (toToken str)


chompUntilEndOr : String -> Parser ()
chompUntilEndOr =
  A.chompUntilEndOr


-- INDENTATION


withIndent : Int -> Parser a -> Parser a
withIndent =
  A.withIndent


getIndent : Parser Int
getIndent =
  A.getIndent


-- POSITION


getPosition : Parser (Int, Int)
getPosition =
  A.getPosition


getRow : Parser Int
getRow =
  A.getRow


getCol : Parser Int
getCol =
  A.getCol


getOffset : Parser Int
getOffset =
  A.getOffset


getSource : Parser String
getSource =
  A.getSource


-- VARIABLES


variable :
  { start : Char -> Bool
  , inner : Char -> Bool
  , reserved : Set.Set String
  }
  -> Parser String
variable i =
  A.variable
    { start = i.start
    , inner = i.inner
    , reserved = i.reserved
    , expecting = ExpectingVariable
    }


-- SEQUENCES


sequence
  : { start : String
    , separator : String
    , end : String
    , spaces : Parser ()
    , item : Parser a
    , trailing : Trailing
    }
  -> Parser (List a)
sequence i =
  A.sequence
    { start = toToken i.start
    , separator = toToken i.separator
    , end = toToken i.end
    , spaces = i.spaces
    , item = i.item
    , trailing = toAdvancedTrailing i.trailing
    }


type Trailing = Forbidden | Optional | Mandatory


toAdvancedTrailing : Trailing -> A.Trailing
toAdvancedTrailing trailing =
  case trailing of
    Forbidden -> A.Forbidden
    Optional -> A.Optional
    Mandatory -> A.Mandatory


-- WHITESPACE


spaces : Parser ()
spaces =
  A.spaces


lineComment : String -> Parser ()
lineComment str =
  A.lineComment (toToken str)


multiComment : String -> String -> Nestable -> Parser ()
multiComment open close nestable =
  A.multiComment (toToken open) (toToken close) (toAdvancedNestable nestable)


type Nestable = NotNestable | Nestable


toAdvancedNestable : Nestable -> A.Nestable
toAdvancedNestable nestable =
  case nestable of
    NotNestable -> A.NotNestable
    Nestable -> A.Nestable
