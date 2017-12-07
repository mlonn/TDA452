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

{-|generates robots -}
robotsGenerator : Int -> Generator (List Robot)
robotsGenerator s = map (\x -> List.map (mkRobot x) colors) (generatePos s)

mkRobot : Pos -> Color -> Robot
mkRobot p c = {c=c, p=p}

generatePos : Int -> Generator Pos
generatePos s = pair (int 1 s) (int 1 s)
