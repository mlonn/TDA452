module Robot exposing (moveRobot, Robot, robotsGenerator)
{-| Robot package
@docs moveRobot
@docs robotsGenerator
@docs Robot 
-}
import Common exposing (..)
import Tuple exposing (first, second)
import Random exposing (..)
{-|Robot -}
type alias Robot = {c: Color, p: Pos }

{-|moves a robot -}
moveRobot : Robot -> Direction -> Robot
moveRobot r d = case d of
  N -> {c = r.c, p = (first r.p, (second r.p)-1)}
  S -> {c = r.c, p = (first r.p, (second r.p) + 1)}
  E -> {c = r.c, p = ((first r.p)+1, second r.p)}
  W -> {c = r.c, p = ((first r.p)-1, second r.p)}

{-| -}
robotsGenerator : Int -> Generator (List Robot)
robotsGenerator i = flattenList <| List.map (robotGenerator i) colors

robotGenerator : Int -> Color -> Generator Robot
robotGenerator i c = map (mkRobot c) (posGenerator i)

mkRobot : Color -> Pos -> Robot
mkRobot c p = {c = c, p = p}
