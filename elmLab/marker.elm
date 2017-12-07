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
markersGenerator i = flattenList <| List.concat <| List.map (\s -> (List.map2 (\c -> markerGenerator i c) colors) s )symbols

markerGenerator : Int -> Color -> Symbol -> Generator Marker
markerGenerator i c s = map (mkMarker c s) (int 0 i)
mkMarker : Color -> Symbol -> Int -> Marker
mkMarker c s i =   {c = c, s = s, i = i}
