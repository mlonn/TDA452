module Common exposing (..)
{-| This seems stupid
@docs Pos
@docs Direction
@docs Color
@docs colors
-}

{-| Direction -}
type Direction = N | S | W | E
{-| Pos -}
type alias Pos = (Int,Int)
{-| Color -}
type Color = Red | Blue | Silver | Yellow | Green

{-| colors -}
colors : List Color
colors = [Red, Blue, Silver, Yellow, Green]
