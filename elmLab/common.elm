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
-}
import Fuzz exposing (..)
import Random.Pcg as Random exposing (Generator, pair, int, map2)
import Shrink

{-| Direction -}
type Direction = N | S | W | E
{-| Pos -}
type alias Pos = (Int,Int)

{-| -}
pos : Int -> Fuzzer Pos
pos i =
    Fuzz.custom
        (posGenerator i)
        posShrink

{-| -}
posShrink : Shrink.Shrinker Pos
posShrink = Shrink.tuple ((Shrink.int),  (Shrink.int))

{-| -}
direction : Fuzzer Direction
direction = frequency
    [ (1, Fuzz.constant N),
      (1, Fuzz.constant W),
      (1, Fuzz.constant S),
      (1, Fuzz.constant E)
    ]

{-| Color -}
type Color = Red | Blue | Silver | Yellow | Green
{-| -}
color : Fuzzer Color
color = Fuzz.oneOf (List.map Fuzz.constant colors)

{-| colors -}
colors : List Color
colors = [Silver, Red, Blue, Yellow, Green]

{-| -}
posGenerator : Int -> Generator Pos
posGenerator s = pair (Random.int 1 s) (Random.int 1 s)

{-| -}
constant : a -> Generator a
constant value = Random.map (\_ -> value) Random.bool

{-| -}
flattenList : List (Generator a) -> Generator (List a)
flattenList generators =
  case generators of
      [] -> constant []
      g :: gs -> Random.map2 (::) g (flattenList gs)

{-| -}
getAt : Int -> List a -> a
getAt i l = case List.head <| List.drop i l of
              Just a -> a
              Nothing ->
                  Debug.crash "No such index"
