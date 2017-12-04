module Robot exposing (..)
import Common exposing (..)
import Tuple exposing (first, second)

type alias Robot = {c: Color, p: Pos }

moveRobot : Robot -> Direction -> Robot
moveRobot r d = case d of
  N -> {c = r.c, p = (first r.p, (second r.p)-1)}
  S -> {c = r.c, p = (first r.p, (second r.p) + 1)}
  E -> {c = r.c, p = ((first r.p)+1, second r.p)}
  W -> {c = r.c, p = ((first r.p)-1, second r.p)}
