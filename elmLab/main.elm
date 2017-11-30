import Tuple exposing (first)
import List exposing (member)

type alias Pos = (Int, Int)

type Color = Red | Blue | Silver | Yellow | Green

type Symbol = Moon | Planet | Star | Gear

type alias Robot = { color : Color, pos : Pos }

type alias Marker = {color : Color, symbol : Symbol}

type Board = Board { vertical : List Wall, horizontal : List Wall }

type Direction = N | S | W | E

type alias Wall = (Pos, Pos)

moveRobot : Robot -> Direction -> Robot
moveRobot r d = case d of
    N -> {color = r.color, pos = (first r.pos, (first r.pos) + 1)}
    W -> {color = r.color, pos = ((first r.pos) + 1, first r.pos)}
    S -> {color = r.color, pos = (first r.pos, (first r.pos) - 1)}
    E -> {color = r.color, pos = ((first r.pos) - 1, first r.pos)}

moveUntilWall : Robot -> Direction -> Board -> Robot
moveUntilWall r N b = if (member (moveRobot r N).pos ( map fst b.vertical ))
  then



-- prop_moveRobot : Robot -> Board -> Direction -> Bool
