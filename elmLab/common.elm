module Common exposing (..)
{-| This seems stupid
@docs Pos
@docs Direction
@docs Color
@docs colors
@docs posGenerator
@docs constant
@docs flattenList
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
colors = [Red, Blue, Silver, Yellow, Green]

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
