module Marker exposing (..)
{-| Markers
@docs Marker
@docs Symbol
@docs generateMarkers
-}

import Common exposing (..)
import Random exposing (Generator, pair, int)


{-| -}
type alias Marker = {c: Color, s: Symbol, p: Pos}
{-| -}
type Symbol = Moon | Planet | Star | Gear
{-| -}
generateMarkers: Int -> Int
generateMarkers limit = 5
