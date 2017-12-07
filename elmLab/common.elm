module Common exposing (..)
import Random exposing (Generator, pair, int)

type Direction = N | S | W | E
type alias Pos = (Int,Int)
type Color = Red | Blue | Silver | Yellow | Green

colors : List Color
colors = [Red, Blue, Silver, Yellow, Green]

posGenerator : Int -> Generator Pos
posGenerator s = pair (int 1 s) (int 1 s)
