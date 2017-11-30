import Tuple exposing (first, second)
import List exposing (member, map, unzip)
import Random exposing (map3, Generator, generate, int, pair, list)
-- import Svg
import Html exposing (program, Html, div, text, Attribute, button)
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

type alias Move = (Robot, Direction)

type alias Model = {b:Board, r:List Robot, m:List Marker}

type Msg
  = Move Move
  | NewGame
  | NewBoard Board

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

move : Move -> Model -> Robot
move (r , d) m = if isWall r.p d m.b || isRobot m.r (moveRobot r d)
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

showBoard : Int -> Html Msg
showBoard i = div [boardWrapper i] (makeCells (i) (i))

makeCells : Int -> Int -> List (Html Msg)
makeCells s i = case i of
  0 -> []
  _ -> makeRow s i ++ makeCells s (i-1)

makeRow : Int -> Int -> List (Html Msg)
makeRow s i =
  case s of
    0 -> []
    _ -> makeCell s i :: makeRow (s-1) i

makeCell : Int -> Int -> Html Msg
makeCell s i = div [baseCell s i] []


baseCell : Int -> Int -> Attribute Msg
baseCell x y = style (("border-style", "solid") :: put x y)

showRobots : List Robot -> Int -> Html Msg
showRobots lr s = div [robotWrapper s] (map showRobot lr)

showRobot : Robot -> Html Msg
showRobot r = div [robotStyle r]
  [ button [ onClick (Move (r, N)), style (put 1 0)] [text "N"],
    button [ onClick (Move (r, W)), style (put 0 1)] [text "W"],
    button [ onClick (Move (r, E)), style (put 2 1)] [text "E"],
    button [ onClick (Move (r, S)), style (put 1 2)] [text "S"],
    div [style (put 1 1)] [text "R"]
  ]

put : Int -> Int -> List (String, String)
put x y = [("grid-column", toString (x+1)), ("grid-row", toString (y+1))]

robotStyle : Robot -> Attribute Msg
robotStyle r = style (
  [
    ("font-size", "small"),
    ("color", toString r.c),
    ("display", "grid"),
    ("grid-auto-rows", "33%"),
    ("grid-template-columns", concat ["repeat(3, 33%)"]),
    ("text-align", "center")
  ] ++ put (second r.p) (first r.p))

showWalls : List Wall -> Int -> Html Msg
showWalls lw s = div [wallWrapper s] (List.concat (map showWall lw))

showWall : Wall -> List (Html Msg)
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

boardWrapper : Int -> Attribute Msg
boardWrapper s = style (wrapper s)

robotWrapper : Int -> Attribute Msg
robotWrapper s = style (("z-index","20") :: wrapper s)

wallWrapper : Int -> Attribute Msg
wallWrapper s = style (("z-index", "10") :: wrapper s)

wrapper : Int -> List (String, String)
wrapper s = let screen = 720 in
  [("display", "grid"),
  ("grid-template-columns", concat ["repeat(", toString (s+2) , ", ", toString (screen//(s+2)), "px)"]),
  ("grid-auto-rows", concat [toString (screen//(s+2)), "px"]),
  ("position","absolute")
  ]

view : Model -> Html Msg
view model = div [style [("display","inline-flex")]] [
  showBoard model.b.s,
  showRobots model.r model.b.s,
  showWalls (model.b.v ++ model.b.h) model.b.s,
  button [ onClick (NewGame), style [("z-index","30")]] [text "start game"]
  ]

game : Model
game = {b = emptyBoard 10, r = [{c=Red, p=(4,3)}, {c=Silver, p=(7,1)}], m= [{c=Red, s=Moon}]}

update : Msg -> Model -> (Model, Cmd Msg)
update msg m = case msg of
              Move mv -> let rl = removeRobot m.r (first mv) in
                              ({b = m.b, r = (move (mv) m) :: rl, m = m.m}, Cmd.none)
              NewGame -> (m, generate NewBoard (boardGenerator 10 5))
              NewBoard n -> ({b= (mergeBoards (emptyBoard n.s) n), r = m.r, m = m.m}, Cmd.none)

vWallsGenerator : Int -> Int -> Generator (List Wall)
vWallsGenerator limit n = list n (pair (pair (int 0 limit) (int 0 limit)) (pair (int 0 limit) (int 0 limit)))

hWallsGenerator : Int -> Int -> Generator (List Wall)
hWallsGenerator limit n = list n (pair (pair (int 0 limit) (int 0 limit)) (pair (int 0 limit) (int 0 limit)))

boardGenerator : Int -> Int -> Generator Board
boardGenerator s w = map3 Board (vWallsGenerator s w) (hWallsGenerator s w) (int s s)

mergeBoards : Board -> Board -> Board
mergeBoards b1 b2 = if (b1.s == b2.s) then
                                      {v = b1.v++b2.v, h= b1.h++b2.h, s=b1.s}
                                    else {v = b1.v, h=b1.h, s=b1.s}

removeRobot : List Robot -> Robot -> List Robot
removeRobot lr r =case lr of
  (x :: xs) -> if x.c == r.c
               then xs
               else x :: removeRobot xs r
  [] -> []

updateRobot : List Robot -> Robot -> List Robot
updateRobot lr r =case lr of
  (x :: xs) -> if x.c == r.c
               then r :: xs
               else x :: removeRobot xs r
  [] -> []

init : (Model, Cmd Msg)
init = (game, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

main : Program Never Model Msg
main = program {init = init,
                view = view,
                update = update,
                subscriptions = subscriptions}

-- prop_moveRobot : Robot -> Board -> Direction -> Bool
