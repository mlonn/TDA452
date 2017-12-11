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

type alias Original = {b:Board, r:List Robot, m:List Marker}

type alias Model = {b:Board, r:List Robot, m:List Marker, c:Int, og:Original}


type Msg
  = Move Move
  | NewGame Original
  | Start
  | Reset

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
showMarker lw m = img [src <| markerImage m.c m.s, markerStyle <| first (getAt m.i lw)] []

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
view model =  let
                internalWalls = (drop (model.b.s*2) model.b.v) ++ (drop (model.b.s*2) model.b.h)
              in
                div [style [("display","grid"),
                            ("grid-template-columns","60% 40%"),
                            ("align-items","center"),
                            ("justify-content","center")
                            ]]
                  [
                  div [style [("height", "900px")]] [
                    showBoard model.b.s,
                    showRobots model.r model.b.s,
                    showWalls (model.b.v ++ model.b.h) model.b.s,
                    showMarkers internalWalls model.m model.b.s
                  ],
                  div [style [("display","inline-flex"), ("align-items", "center"), ("justify-content", "center"),("width", "100%")]] [
                    button [ onClick (Start), controlStyle ] [text "start game"],
                    button [ onClick (Reset), controlStyle ] [text "Reset"],
                    div [] [text <| String.concat ["Number of moves: ",(toString model.c)]]
                  ]
                ]

baseGame : Original
baseGame = {b = emptyBoard 10, r = [], m= []}

gameGenerator : Int -> Int -> Generator Original
gameGenerator s w = map3 Original (boardGenerator s w) (robotsGenerator s) (markersGenerator (w*2))

update : Msg -> Model -> (Model, Cmd Msg)
update msg m = case msg of
              Move mv -> let rl = removeRobot m.r (first mv) in
                              ({m | r = (move (mv) m) :: rl, c = m.c+1}, Cmd.none)
              Start -> (m, newGameCommand)
              NewGame game -> (originalToModel game, Cmd.none)
              Reset -> (originalToModel m.og, Cmd.none)

--fixCollision : List Wall -> List Marker -> List Wall
--fixCollision :

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

newGameCommand : Cmd Msg
newGameCommand = generate NewGame (gameGenerator 16 25)

init : {startTime : Float} -> (Model, Cmd Msg)
init {startTime} = (originalToModel baseGame, newGameCommand)

originalToModel : Original -> Model
originalToModel og = {b = (mergeBoards (emptyBoard og.b.s) og.b),
                  r = og.r,
                  m = og.m,
                  c = 0,
                  og = og
                  }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


main : Program { startTime : Float } Model Msg
main = programWithFlags {init = init,
                view = view,
                update = update,
                subscriptions = subscriptions}

-- prop_moveRobot : Robot -> Board -> Direction -> Bool
