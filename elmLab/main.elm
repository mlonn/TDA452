import Tuple exposing (first, second)
import List exposing (member, map, unzip)
import Random exposing (generate, int, pair)
-- import Svg
import Html exposing (beginnerProgram, Html, div, text, Attribute, button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import String exposing (concat)


type alias Pos = (Int,Int)

type Color = Red | Blue | Silver | Yellow | Green

type Symbol = Moon | Planet | Star | Gear

type alias Robot = {c: Color, p: Pos }

type alias Marker = {c: Color, s: Symbol}

type alias Board = { v : List Wall, h : List Wall, s : Int}

type Direction = N | S | W | E


type alias Wall = (Pos, Pos)

type alias Elem = (Robot, Direction)

type alias Model = {b:Board, r:List Robot, m:List Marker}

emptyBoard : Int -> Board
emptyBoard s = {v = generateVWalls s, h = generateHWalls s, s = s}

generateHWalls : Int -> List Wall
generateHWalls s = case s of
  0 -> []
  _ -> makeHOutline s s

makeHOutline : Int -> Int -> List Wall
makeHOutline s i = case i of
  0 -> []
  _ -> [((0,i), (1,i)), ((s,i),(s+1,i))] ++ makeHOutline s (i-1)


generateVWalls : Int -> List Wall
generateVWalls s = case s of
  0 -> []
  _ -> makeVOutline s s

makeVOutline : Int -> Int -> List Wall
makeVOutline s i = case i of
  0 -> []
  _ -> [((i,0), (i,1)), ((i,s),(i,s+1))] ++ makeVOutline s (i-1)


moveRobot : Robot -> Direction -> Robot
moveRobot r d = case d of
  N -> {c = r.c, p = ((first r.p) - 1, second r.p)}
  S -> {c = r.c, p = ((first r.p) + 1, second r.p)}
  E -> {c = r.c, p = (first r.p, (second r.p) + 1)}
  W -> {c = r.c, p = (first r.p, (second r.p) - 1)}

move : Elem -> Model -> Robot
move (r, d) m = if isWall r.p d m.b || isRobot m.r (moveRobot r d)
                      then r
                      else move ((moveRobot r d ), d) m

isRobot : List Robot -> Robot -> Bool
isRobot lr r = member r.p (map (\x -> x.p) lr)

isWall : Pos -> Direction -> Board -> Bool
isWall p d b = case d of
  N -> (member p (map second b.h))
  S -> (member p (map first b.h))
  W -> (member p (map second b.v))
  E -> (member p (map first b.v))

showBoard : Int -> Html Elem
showBoard i = div [boardWrapper i] (makeCells (i) (i))

makeCells : Int -> Int -> List (Html Elem)
makeCells s i = case i of
  0 -> []
  _ -> makeRow s i ++ makeCells s (i-1)

makeRow : Int -> Int -> List (Html Elem)
makeRow s i =
  case s of
    0 -> []
    _ -> makeCell s i :: makeRow (s-1) i

makeCell : Int -> Int -> Html Elem
makeCell s i = div [baseCell s i] []


baseCell : Int -> Int -> Attribute Elem
baseCell x y = style (("border-style", "solid") :: put x y)

showRobots : List Robot -> Int -> Html Elem
showRobots lr s = div [robotWrapper s] (map showRobot lr)

showRobot : Robot -> Html Elem
showRobot r = div [robotStyle r]
  [ button [ onClick (r, N), style (put 1 0)] [text "N"],
    button [ onClick (r, W), style (put 0 1)] [text "W"],
    button [ onClick (r, E), style (put 2 1)] [text "E"],
    button [ onClick (r, S), style (put 1 2)] [text "S"],
    div [style (put 1 1)] [text "R"]
  ]

put : Int -> Int -> List (String, String)
put x y = [("grid-column", toString (x+1)), ("grid-row", toString (y+1))]

robotStyle : Robot -> Attribute Elem
robotStyle r = style (
  [
    ("font-size", "small"),
    ("color", toString r.c),
    ("display", "grid"),
    ("grid-auto-rows", "33%"),
    ("grid-template-columns", concat ["repeat(3, 33%)"]),
    ("text-align", "center")
  ] ++ put (second r.p) (first r.p))

showWalls : List Wall -> Int -> Html Elem
showWalls lw s = div [wallWrapper s] (List.concat (map showWall lw))

showWall : Wall -> List (Html Elem)
showWall w = [div [wallStyle first w] [], div [wallStyle second w] []]

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

boardWrapper : Int -> Attribute Elem
boardWrapper s = style (wrapper s)

robotWrapper : Int -> Attribute Elem
robotWrapper s = style (("z-index","20") :: wrapper s)

wallWrapper : Int -> Attribute Elem
wallWrapper s = style (("z-index", "10") :: wrapper s)

wrapper : Int -> List (String, String)
wrapper s = let screen = 720 in
  [("display", "grid"),
  ("grid-template-columns", concat ["repeat(", toString (s+2) , ", ", toString (screen//(s+2)), "px)"]),
  ("grid-auto-rows", concat [toString (screen//(s+2)), "px"]),
  ("position","absolute")
  ]

view : Model -> Html Elem
view model = div [style [("display","inline-flex")]] [
  showBoard model.b.s,
  showRobots model.r model.b.s,
  showWalls (model.b.v ++ model.b.h) model.b.s
  ]

game : Model
game = {b = emptyBoard 10, r = [{c=Red, p=(4,3)}, {c=Silver, p=(7,1)}], m= [{c=Red, s=Moon}]}

update : Elem -> Model -> Model
update elem m = let rl = removeRobot m.r (first elem) in
    {b = m.b, r = (move elem m) :: rl, m = m.m}

removeRobot : List Robot -> Robot -> List Robot
removeRobot lr r =case lr of
  (x :: xs) -> if x.c == r.c
               then xs
               else x :: removeRobot xs r
  [] -> []

main : Program Never Model ( Robot, Direction )
main = beginnerProgram {model = game, view = view, update = update}

-- prop_moveRobot : Robot -> Board -> Direction -> Bool
