module Styles exposing (put, wallStyle, wallBorderStyle, robotWrapper, markerWrapper, markerStyle,boardWrapper, wallWrapper, robotImage, markerImage)
{-| css stylings
@docs wallStyle
@docs wallBorderStyle
@docs robotWrapper
@docs boardWrapper
@docs markerWrapper
@docs markerImage
@docs markerStyle
@docs wallWrapper
@docs robotImage
@docs put
-}

import Tuple exposing (first , second)
import Common exposing (..)
import String exposing (concat)
import Board exposing (..)
import Marker exposing (..)
import Html exposing (Attribute, node, Html)
import Html.Attributes exposing (attribute, style)

{-| -}
put : Int -> Int -> List (String, String)
put x y = [("grid-column", toString (x+1)), ("grid-row", toString (y+1))]

{-| -}
markerStyle : Pos -> Attribute msg
markerStyle p = style <| ("width","100%") :: put (first p) (second p)

{-| -}
wallStyle : ((Pos, Pos) -> Pos) -> Wall -> Attribute msg
wallStyle f w = style ([
    wallBorderStyle f w] ++ (put (first (f w)) (second (f w))))
{-| -}
wallBorderStyle : ((Pos, Pos) -> Pos) -> Wall -> (String, String)
wallBorderStyle f w =
  let ws = "solid 2px #212d35" in
  if f w == first w
  then ( if checkH w then ("border-right", ws) else ("border-bottom", ws) )
  else ( if checkH w then ("border-left", ws) else ("border-top", ws) )

checkH : Wall -> Bool
checkH w = second (first w) == second (second w)
{-| -}
boardWrapper : Int -> Attribute msg
boardWrapper s = style (wrapper s)
{-| -}
robotWrapper : Int -> Attribute msg
robotWrapper s = style (("z-index","20") :: wrapper s)
{-| -}
markerWrapper : Int -> Attribute msg
markerWrapper s = style (("z-index","15") :: wrapper s)
{-| -}
wallWrapper : Int -> Attribute msg
wallWrapper s = style (("z-index", "10") :: wrapper s)

wrapper : Int -> List (String, String)
wrapper s = let screen = 900 in
  [("display", "grid"),
  ("grid-template-columns", concat ["repeat(", toString (s+2) , ", ", toString (screen//(s+2)), "px)"]),
  ("grid-auto-rows", concat [toString (screen//(s+2)), "px"]),
  ("position","absolute")
  ]



{-| -}
robotImage : Color -> String
robotImage c = concat ["media/", (toString c), "/Robot.svg"]

{-| -}
markerImage : Color -> Symbol -> String
markerImage c s = concat ["media/", (toString c), "/", (toString s), ".svg"]
