module Marker exposing (..)
import Common exposing (..)
import Random exposing (..)

type alias Marker = {c: Color, s: Symbol, i: Int}
type Symbol = Moon | Planet | Star | Gear
symbols : List Symbol
symbols = [Moon, Planet, Star, Gear]

markerGenerator : Int -> Generator (List Marker)
markerGenerator limit = map (\x -> List.map2 (mkMarker x) colors symbols) (int 0 limit)

mkMarker : Int -> Color -> Symbol -> Marker
mkMarker i c s  =   {c = c, s = s, i = i}
