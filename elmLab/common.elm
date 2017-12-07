module Common exposing (..)
{-| This seems stupid
@docs Pos
@docs Direction
@docs Color
@docs colors
@docs posGenerator
-}
import Random exposing (Generator, pair, int)


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
