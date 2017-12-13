module Robot exposing (moveRobot, Robot, robotsGenerator, robot)
{-| Robot package
@docs moveRobot
@docs robotsGenerator
@docs Robot 
@docs robot
-}
import Common exposing (..)
import Tuple exposing (first, second)
import Random.Pcg as Random exposing (..)
import Fuzz exposing (..)

{-| Robot -}
type alias Robot = {c: Color, p: Pos }

{-| Fuzzer for a robot -}
robot : Int -> Fuzzer Robot
robot i = Fuzz.map2 Robot color (pos i)

{-| Moves a robot until it reaches a wall or another robot. -}
moveRobot : Robot -> Direction -> Robot
moveRobot r d = case d of
  N -> {c = r.c, p = (first r.p, (second r.p)-1)}
  S -> {c = r.c, p = (first r.p, (second r.p) + 1)}
  E -> {c = r.c, p = ((first r.p)+1, second r.p)}
  W -> {c = r.c, p = ((first r.p)-1, second r.p)}

{-| Makes a random robot generator -}
robotsGenerator : Int -> Generator (List Robot)
robotsGenerator i = flattenList <| List.map (robotGenerator i) colors

robotGenerator : Int -> Color -> Generator Robot
robotGenerator i c = Random.map (mkRobot c) (posGenerator i)

mkRobot : Color -> Pos -> Robot
mkRobot c p = {c = c, p = p}
