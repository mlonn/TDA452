module Styles exposing (..)

import Tuple exposing (first , second)
import Robot exposing (..)
import Common exposing (..)
import String exposing (concat)
import Wall exposing (..)
import Html exposing (Attribute, node)
import Html.Attributes exposing (attribute, style)

put : Int -> Int -> List (String, String)
put x y = [("grid-column", toString (x+1)), ("grid-row", toString (y+1))]

baseCell : Int -> Int -> Attribute msg
baseCell x y = style (("border-style", "solid") :: put x y)

robotStyle : Robot -> Attribute msg
robotStyle r = style (
  [
    ("font-size", "small"),
    ("color", toString r.c),
    ("display", "grid"),
    ("grid-auto-rows", "33%"),
    ("grid-template-columns", concat ["repeat(3, 33%)"]),
    ("text-align", "center")
  ] ++ put (first r.p) (second r.p))


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
buttonStyle d = case d of
  N -> style (put 1 0)
  S -> style (put 1 2)
  E -> style (put 2 1)
  W -> style (put 0 1)
