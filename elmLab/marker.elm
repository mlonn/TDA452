module Marker exposing (Marker, Symbol, symbols, markersGenerator)
{-| Markers
@docs Marker
@docs Symbol
@docs symbols
@docs markersGenerator
-}

import Common exposing (..)
import Random exposing (..)
{-| -}
type alias Marker = {c: Color, s: Symbol, i: Int}
{-| -}
type Symbol = Moon | Planet | Star | Gear
{-| -}
symbols : List Symbol
symbols = [Moon, Planet, Star, Gear]
{-| -}
markersGenerator : Int -> Generator (List Marker)
markersGenerator limit = map (\x -> List.map2 (mkMarker x) colors symbols) (int 0 limit)

mkMarker : Int -> Color -> Symbol -> Marker
mkMarker i c s  =   {c = c, s = s, i = i}

markerCombinations : List ( Symbol, Color )
markerCombinations =  List.concat <| List.map2 (\s cs -> List.map (\c -> (s, c)) cs) symbols <| List.map (\x -> colors) colors
