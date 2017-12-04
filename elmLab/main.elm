import Tuple exposing (first, second)
import List exposing (member, map, unzip)
import Random exposing (map3, Generator, generate, int, pair, list, step, initialSeed, Seed)
import Html exposing (programWithFlags, Html, div, text, Attribute, button, img)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, class, src)
import Common exposing (..)
import Wall exposing (..)
import Robot exposing (..)
import Board exposing (..)
import Marker exposing (..)
import Styles exposing (..)

type alias Move = (Robot, Direction)

type alias Model = {b:Board, r:List Robot, m:List Marker, s: Seed}

type Msg
  = Move Move
  | NewGame
  | NewBoard Board

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

showRobots : List Robot -> Int -> Html Msg
showRobots lr s = div [robotWrapper s] (map showRobot lr)

showRobot : Robot -> Html Msg
showRobot r = div [robotCellStyle r]
  [ div [ onClick (Move (r, N)), buttonStyle N] [svg N],
    div [ onClick (Move (r, W)), buttonStyle W] [svg W],
    div [ onClick (Move (r, E)), buttonStyle E] [svg E],
    div [ onClick (Move (r, S)), buttonStyle S] [svg S],
    div [robotStyle r.c] [robotSvg]
  ]

showWalls : List Wall -> Int -> Html Msg
showWalls lw s = div [wallWrapper s] (List.concat (map showWall lw))

showWall : Wall -> List (Html Msg)
showWall w = [div [wallStyle first w] [], div [wallStyle second w] []]


view : Model -> Html Msg
view model = div [style [("display","inline-flex")]] [
  showBoard model.b.s,
  showRobots model.r model.b.s,
  showWalls (model.b.v ++ model.b.h) model.b.s,
  button [ onClick (NewGame), style [("z-index","30")]] [text "start game"]
  ]

baseGame : Model
baseGame = {b = emptyBoard 10, r = [{c=Red, p=(4,3)}, {c=Silver, p=(1,1)}], m= [{c=Red, s=Moon}], s = initialSeed 0}

update : Msg -> Model -> (Model, Cmd Msg)
update msg m = case msg of
              Move mv -> let rl = removeRobot m.r (first mv) in
                              ({m | r = (move (mv) m) :: rl}, Cmd.none)
              NewGame -> (m, generate NewBoard (boardGenerator 10 5))
              NewBoard board -> ({m | b = (mergeBoards (emptyBoard board.s) board)}, Cmd.none)

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

init : {startTime : Float} -> (Model, Cmd Msg)
init {startTime} = ({baseGame| s = initialSeed <| round startTime}, generate NewBoard (boardGenerator 5 5))

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


main : Program { startTime : Float } Model Msg
main = programWithFlags {init = init,
                view = view,
                update = update,
                subscriptions = subscriptions}

-- prop_moveRobot : Robot -> Board -> Direction -> Bool
