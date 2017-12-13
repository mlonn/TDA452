module Common exposing (..)
{-| This seems stupid
@docs Pos
@docs Direction
@docs Color
@docs colors
@docs posGenerator
@docs constant
@docs flattenList
@docs getAt
@docs pos
@docs posShrink
@docs direction
@docs color
@docs unique
@docs uniqueHelp
@docs colorGenerator
@docs intToColor
-}
import Fuzz exposing (..)
import Random.Pcg as Random exposing (Generator, pair, int, map2)
import Shrink
import Set exposing (Set)

{-| Direction -}
type Direction = N | S | W | E
{-| Pos -}
type alias Pos = (Int,Int)

{-| Fuzzer for positions -}
pos : Int -> Fuzzer Pos
pos i =
    Fuzz.custom
        (posGenerator i)
        posShrink

{-| Shrinker to be able to fuzz positions-}
posShrink : Shrink.Shrinker Pos
posShrink = Shrink.tuple ((Shrink.int),  (Shrink.int))

{-| Fuzzer for directions -}
direction : Fuzzer Direction
direction = frequency
    [ (1, Fuzz.constant N),
      (1, Fuzz.constant W),
      (1, Fuzz.constant S),
      (1, Fuzz.constant E)
    ]

{-| Color -}
type Color = Red | Blue | Silver | Yellow | Green

{-| Fuzzer for colors -}
color : Fuzzer Color
color = Fuzz.oneOf (List.map Fuzz.constant colors)

{-| Generator for colors -}
colorGenerator : Generator Color
colorGenerator = Random.map intToColor (Random.int 1 4)

{-| Transforms an int to a color -}
intToColor : Int -> Color
intToColor i = case i of
                1 -> Red
                2 -> Blue
                3 -> Yellow
                _ -> Green

{-| List of all possible colors -}
colors : List Color
colors = [Silver, Red, Blue, Yellow, Green]

{-| Generator for random positions -}
posGenerator : Int -> Generator Pos
posGenerator s = pair (Random.int 1 s) (Random.int 1 s)

{-| Given a type, create a generator for it-}
constant : a -> Generator a
constant value = Random.map (\_ -> value) Random.bool

{-| Transforms a list of generators to a generators of that list.-}
flattenList : List (Generator a) -> Generator (List a)
flattenList generators =
  case generators of
      [] -> constant []
      g :: gs -> Random.map2 (::) g (flattenList gs)

{-| returns the value of a given index in a list if possible -}
getAt : Int -> List a -> a
getAt i l = case List.head <| List.drop i l of
              Just a -> a
              Nothing ->
                  Debug.crash "No such index"

{-| Remove all duplicates from a list and return a list of distinct elements.
-}
unique : List comparable -> List comparable
unique list =
  uniqueHelp Set.empty list

{-| Helper for unique -}
uniqueHelp : Set comparable -> List comparable -> List comparable
uniqueHelp existing remaining =
  case remaining of
    [] ->
      []

    first :: rest ->
      if Set.member first existing then
        uniqueHelp existing rest
      else
        first :: uniqueHelp (Set.insert first existing) rest
