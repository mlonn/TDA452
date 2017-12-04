module Marker exposing (..)
import Common exposing (..)

type alias Marker = {c: Color, s: Symbol, p: Pos}
type Symbol = Moon | Planet | Star | Gear

generateMarkers: Int -> List (Generator Marker)
generateMarkers limit = 
