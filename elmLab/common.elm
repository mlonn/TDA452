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
-}
import Random exposing (Generator, pair, int, map2)


{-| Direction -}
type Direction = N | S | W | E
{-| Pos -}
type alias Pos = (Int,Int)
{-| Color -}
type Color = Red | Blue | Silver | Yellow | Green

{-| colors -}
colors : List Color
colors = [Silver, Red, Blue, Yellow, Green]

{-| -}
posGenerator : Int -> Generator Pos
posGenerator s = pair (int 1 s) (int 1 s)

{-| -}
constant : a -> Generator a
constant value = Random.map (\_ -> value) Random.bool

{-| -}
flattenList : List (Generator a) -> Generator (List a)
flattenList generators =
  case generators of
      [] -> constant []
      g :: gs -> map2 (::) g (flattenList gs)

{-| -}
getAt : Int -> List a -> a
getAt i l = case List.head <| List.drop i l of
              Just a -> a
              Nothing ->
                  Debug.crash "TODO"
