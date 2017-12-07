import Tuple exposing (first, second)
import List exposing (member, map, unzip, drop)
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
import InlineSvg exposing (..)
import Svg.Attributes

type alias Move = (Robot, Direction)

type alias Model = {b:Board, r:List Robot, m:List Marker}


type Msg
  = Move Move
  | NewGame Model
  | Start

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

showMarkers :  List Wall -> List Marker -> Int -> Html Msg
showMarkers lw lm s = div[markerWrapper s] (map (showMarker lw) lm)

showMarker : List Wall -> Marker -> Html Msg
showMarker lw m = let
                    w = getAt m.i lw
                    fw = first w
                  in
                    img [src <| markerImage m.c m.s, markerStyle <| if first fw * second fw == 0 then second w else first w] []

showRobots : List Robot -> Int -> Html Msg
showRobots lr s = div [robotWrapper s] (map showRobot lr)

showRobot : Robot -> Html Msg
showRobot r = div [robotCellStyle r]
  [ img [src "media/Up.svg", onClick (Move (r, N)), buttonStyle N] [],
    img [src "media/Left.svg", onClick (Move (r, W)), buttonStyle W] [],
    img [src "media/Right.svg", onClick (Move (r, E)), buttonStyle E] [],
    img [src "media/Down.svg", onClick (Move (r, S)), buttonStyle S] [],
    img [src <| robotImage r.c, robotStyle] []
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
  showMarkers ((drop model.b.s model.b.v) ++ (drop model.b.s model.b.h)) model.m model.b.s,
  button [ onClick (Start), style [("z-index","30")]] [text "start game"]
  ]

baseGame : Model
baseGame = {b = emptyBoard 10, r = [{c=Red, p=(4,3)}, {c=Silver, p=(1,1)}], m= []}

gameGenerator : Int -> Int -> Generator Model
gameGenerator s w = map3 Model (boardGenerator s w) (robotsGenerator s) (markersGenerator w)

update : Msg -> Model -> (Model, Cmd Msg)
update msg m = case msg of
              Move mv -> let rl = removeRobot m.r (first mv) in
                              ({m | r = (move (mv) m) :: rl}, Cmd.none)
              Start -> (m, generate NewGame (gameGenerator 10 5))
              NewGame game -> ({game | b = (mergeBoards (emptyBoard game.b.s) game.b)}, Cmd.none)

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
init {startTime} = (baseGame, generate NewGame (gameGenerator 10 20))

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


main : Program { startTime : Float } Model Msg
main = programWithFlags {init = init,
                view = view,
                update = update,
                subscriptions = subscriptions}

-- prop_moveRobot : Robot -> Board -> Direction -> Bool
