module Marker exposing (..)
{-| Markers
@docs Marker
@docs Symbol
@docs symbols
@docs markersGenerator
@docs markerGenerator
@docs symbolGenerator
@docs intToSymbol
@docs markerCombinations
@docs markerGeneratorIndex
@docs mkMarker
@docs symbol
-}

import Common exposing (..)
import Random.Pcg as Random exposing (..)
import Fuzz exposing (Fuzzer)
{-| -}
type alias Marker = {c: Color, s: Symbol, i: Int, r: Int}
{-| -}
type Symbol = Moon | Planet | Star | Gear
{-| -}
symbols : List Symbol
symbols = [Moon, Planet, Star, Gear]
{-| -}
symbol : Fuzzer Symbol
symbol = Fuzz.oneOf (List.map Fuzz.constant [Moon, Planet, Star, Gear])

{-| -}
symbolGenerator : Generator Symbol
symbolGenerator = Random.map intToSymbol (Random.int 1 4)
{-| -}
intToSymbol: Int -> Symbol
intToSymbol i = case i of
                1 -> Moon
                2 -> Planet
                3 -> Star
                _ -> Gear
{-| -}
markerCombinations : List ( Symbol, Color )
markerCombinations =  let
                        mColors = List.drop 1 colors
                      in
                        List.concat <| List.map2 (\s cs -> List.map (\c -> (s, c)) cs) symbols <| List.map (\x -> mColors) mColors
{-| -}
markersGenerator : Int -> Generator (List Marker)
markersGenerator i = flattenList <| List.map (markerGeneratorIndex i) markerCombinations
{-| -}
markerGeneratorIndex : Int -> (Symbol, Color) -> Generator Marker
markerGeneratorIndex i (s, c) = Random.map2 (mkMarker c s) (Random.int 0 (i-1)) (Random.int 0 1)
{-| -}
markerGenerator : Generator Marker
markerGenerator = Random.map4 Marker colorGenerator symbolGenerator (Random.int 0 0) (Random.int 0 1)
{-| -}
mkMarker : Color -> Symbol -> Int -> Int -> Marker
mkMarker c s i r =   {c = c, s = s, i = i, r = r}
