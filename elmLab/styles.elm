module Styles exposing (winMessageStyle, winContentContainer, winButtonStyle, winStyle, controlStyle, baseCell, robotCellStyle, robotStyle, wallStyle, wallBorderStyle, robotWrapper, markerWrapper, markerStyle,boardWrapper, wallWrapper, buttonStyle, robotImage, markerImage)
{-| css stylings
@docs baseCell
@docs robotCellStyle
@docs robotStyle
@docs wallStyle
@docs wallBorderStyle
@docs robotWrapper
@docs boardWrapper
@docs markerWrapper
@docs markerImage
@docs markerStyle
@docs wallWrapper
@docs buttonStyle
@docs robotImage
@docs controlStyle
@docs winStyle
@docs winButtonStyle
@docs winContentContainer
@docs winMessageStyle
-}

import Tuple exposing (first , second)
import Robot exposing (..)
import Common exposing (..)
import String exposing (concat)
import Board exposing (..)
import Marker exposing (..)
import Html exposing (Attribute, node, Html)
import Html.Attributes exposing (attribute, style)

put : Int -> Int -> List (String, String)
put x y = [("grid-column", toString (x+1)), ("grid-row", toString (y+1))]
{-| -}
baseCell : Int -> Int -> Attribute msg
baseCell x y = style ([("border-style", "solid"), ("border-width", "1px"), ("border-color", "#757575"), ("background", "repeating-linear-gradient(45deg, #bcaaa4, #bcaaa4 2px, #a1887f 2px, #a1887f 4px)")] ++ put x y)

{-| -}
robotCellStyle : Robot -> Attribute msg
robotCellStyle r = style (
  [
    ("font-size", "small"),
    ("display", "grid"),
    ("grid-template-rows", "10% 80% 10%"),
    ("grid-template-columns", "15% 70% 15%"),
    ("text-align", "center")
  ] ++ put (first r.p) (second r.p))
{-| -}
markerStyle : Pos -> Attribute msg
markerStyle p = style <| ("width","100%") :: put (first p) (second p)
{-| -}
robotStyle : Attribute msg
robotStyle = style <| ("width","100%") :: (put 1 1)
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
buttonStyle : Direction -> Attribute msg
buttonStyle d = style (("width","100%") ::
                (case d of
                  N -> (put 1 0)
                  S -> (put 1 2)
                  E -> (put 2 1)
                  W -> (put 0 1)))

{-| -}
controlStyle : Attribute msg
controlStyle = style [  ("z-index","30"),
                        ("border-radius" , " 10px"),
                        ("font-family" , " Courier New"),
                        ("color" , " #ffffff"),
                        ("font-size" , " 25px"),
                        ("width" , " 100%"),
                        ("background" , " #09cdda"),
                        ("padding" , " 5px 10px 5px 10px"),
                        ("margin" , " 5px 10px 5px 10px"),
                        ("text-decoration" , " none") ]
{-| -}
robotImage : Color -> String
robotImage c = concat ["media/", (toString c), "/Robot.svg"]

{-| -}
markerImage : Color -> Symbol -> String
markerImage c s = concat ["media/", (toString c), "/", (toString s), ".svg"]

{-| -}
winStyle : Attribute msg
winStyle = style [("position","absolute"),
                  ("z-index","50"),
                  ("width", "100%"),
                  ("height", "100%"),
                  ("background-color", "rgba(12, 12, 12, 0.6)"),
                  ("display","grid"),
                  ("grid-template-columns", "100%"),
                  ("grid-template-rows", "50% 50%")]
{-| -}
winButtonStyle : Attribute msg
winButtonStyle = style [("border-radius" , " 10px"),
                        ("font-family" , " Courier New"),
                        ("color" , " #ffffff"),
                        ("font-size" , " 50px"),
                        ("width" , " 20%"),
                        ("background" , " #09cdda"),
                        ("padding" , " 10px 20px 10px 20px"),
                        ("margin" , " 10px 20px 10px 20px"),
                        ("text-decoration" , " none"),
                        ("grid-column", "1"), ("grid-row", "2") ]
{-| -}
winContentContainer : Attribute msg
winContentContainer = style [("display","inline-flex"),
                            ("flex-direction","row"),
                            ("align-items", "center"),
                            ("justify-content", "center")]
{-| -}
winMessageStyle: Attribute msg
winMessageStyle = style [("font-size","100px"),
                              ("font-family" ," Courier New"),
                              ("text-decoration" , "none"),
                              ("font-weight","bold"),
                              ("-webkit-text-stroke","2px white")
                              ]
