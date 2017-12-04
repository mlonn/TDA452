module Robot exposing (..)
import Common exposing (..)
import Tuple exposing (first, second)
import Random exposing (..)

type alias Robot = {c: Color, p: Pos }

moveRobot : Robot -> Direction -> Robot
moveRobot r d = case d of
  N -> {c = r.c, p = (first r.p, (second r.p)-1)}
  S -> {c = r.c, p = (first r.p, (second r.p) + 1)}
  E -> {c = r.c, p = ((first r.p)+1, second r.p)}
  W -> {c = r.c, p = ((first r.p)-1, second r.p)}


robotsGenerator : Int -> List (Generator Robot)
robotsGenerator s = map (generateRobot s) colors

generateRobot : Int -> Color -> Generator (Robot)
generateRobot s c =
  let
    p = ((int 1 s), (int 1 s))
  in
    { c = c, p = p }
