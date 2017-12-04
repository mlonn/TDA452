module Styles exposing (..)

import Tuple exposing (first , second)
import Robot exposing (..)
import Common exposing (..)
import String exposing (concat)
import Wall exposing (..)
import Html exposing (Attribute, node, Html)
import Html.Attributes exposing (attribute, style)
import Svg.Attributes exposing (class)
import InlineSvg exposing (..)

{icon} =
  inline
          { right = "./media/Right.svg",
            left  = "./media/Left.svg",
            up    = "./media/Up.svg",
            down  = "./media/Down.svg",
            robot = "./media/Robot.svg"
          }


put : Int -> Int -> List (String, String)
put x y = [("grid-column", toString (x+1)), ("grid-row", toString (y+1))]

baseCell : Int -> Int -> Attribute msg
baseCell x y = style (("border-style", "solid") :: put x y)

robotCellStyle : Robot -> Attribute msg
robotCellStyle r = style (
  [
    ("font-size", "small"),
    ("color", toString r.c),
    ("display", "grid"),
    ("grid-template-rows", "10% 80% 10%"),
    ("grid-template-columns", "15% 70% 15%"),
    ("text-align", "center")
  ] ++ put (first r.p) (second r.p))

robotStyle : Color -> Attribute msg
robotStyle c = style <| [("width","100%"), ("fill",toString c)] ++ (put 1 1)

wallStyle : ((Pos, Pos) -> Pos) -> Wall -> Attribute msg
wallStyle f w = style ([
    wallBorderStyle f w] ++ (put (first (f w)) (second (f w))))

wallBorderStyle : ((Pos, Pos) -> Pos) -> Wall -> (String, String)
wallBorderStyle f w =
  if f w == first w
  then ( if checkH w then ("border-right","solid red") else ("border-bottom","solid red") )
  else ( if checkH w then ("border-left","solid red") else ("border-top","solid red") )

checkH : Wall -> Bool
checkH w = second (first w) == second (second w)

boardWrapper : Int -> Attribute msg
boardWrapper s = style (wrapper s)

robotWrapper : Int -> Attribute msg
robotWrapper s = style (("z-index","20") :: wrapper s)

wallWrapper : Int -> Attribute msg
wallWrapper s = style (("z-index", "10") :: wrapper s)

wrapper : Int -> List (String, String)
wrapper s = let screen = 900 in
  [("display", "grid"),
  ("grid-template-columns", concat ["repeat(", toString (s+2) , ", ", toString (screen//(s+2)), "px)"]),
  ("grid-auto-rows", concat [toString (screen//(s+2)), "px"]),
  ("position","absolute")
  ]

buttonStyle : Direction -> Attribute msg
buttonStyle d = style (("width","100%") ::
                (case d of
                  N -> (put 1 0)
                  S -> (put 1 2)
                  E -> (put 2 1)
                  W -> (put 0 1)))

robotSvg : Html msg
robotSvg = icon .robot [class "icon"]

svg : Direction -> Html msg
svg d = case d of
          N -> icon .up [class "icon"]
          S -> icon .down [class "icon"]
          E -> icon .right [class "icon"]
          W -> icon .left [class "icon"]
