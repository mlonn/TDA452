module Main exposing (prop_robot)
{-| The main file
@docs prop_robot
-}
import Tuple exposing (first, second)
import List exposing (member, map, unzip, drop)
import Random.Pcg as Random exposing (map3, Generator, generate, int, pair, list, step, initialSeed, Seed)
import Html exposing (programWithFlags, Html, div, text, Attribute, button, img)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, class, src)
import Common exposing (..)
import Robot exposing (..)
import Board exposing (..)
import Marker exposing (..)
import Styles exposing (..)
import Test exposing (Test, describe, test, fuzz, fuzz2)
import Fuzz exposing (..)
import Expect

type alias Move = (Robot, Direction)

type alias Original = {b:Board, r:List Robot, m:List Marker, gm: Marker}

type alias Model = {b:Board, r:List Robot, m:List Marker, c:Int,og:Original}


type Msg
  = Move Move
  | NewGame Original
  | Start
  | Reset
  | NewMarker Marker
  | NextMarker

move : Move -> Model -> Robot
move (r , d) m = if isWall r.p d m.b || isRobot m.r (moveRobot r d) then
                  r
                else
                  move ((moveRobot r d ), d) m

{-|-}
prop_robot : Test
prop_robot = describe "Robot tests"
              [ fuzz2 (robot 20) direction "Should move until wall"
                (\r d ->
                  let
                    am = move (r, d) {b= emptyBoard 20,r= [r],m= [],c= 0,og={b=emptyBoard 20, r=[r], m=[], gm = {c = Red, s = Moon, i = 0}} }
                  in
                    case d of
                      N -> second am.p |> Expect.equal 1
                      W -> first am.p |> Expect.equal 1
                      S -> second am.p |> Expect.equal 20
                      E -> first am.p |> Expect.equal 20
                  ),
                fuzz (Fuzz.list (robot 20)) "Testing remove"
                  (\lr ->
                    let fr = List.head lr in
                    case fr of
                     Just r -> member r (removeRobot (List.filter (\x -> x.c /= r.c && r.p /= x.p) lr) r) |> Expect.equal False
                     Nothing -> Expect.pass
                  ),
                fuzz (Fuzz.list (robot 20)) "Testing size remove"
                  (\lr ->
                    let fr = List.head lr in
                    case fr of
                     Just r -> List.length (removeRobot lr r) |> Expect.equal ((List.length lr) - 1)
                     Nothing -> Expect.pass
                  )

              ]

isRobot : List Robot -> Robot -> Bool
isRobot lr r = member r.p (List.map (\x -> x.p) lr)

isWall : Pos -> Direction -> Board -> Bool
isWall p d b = case d of
  N -> (member p (List.map second b.h))
  S -> (member p (List.map first b.h))
  W -> (member p (List.map second b.v))
  E -> (member p (List.map first b.v))

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

showGoalMarker : Marker -> Html Msg
showGoalMarker m = img [src <| markerImage m.c m.s] []

showMarkers :  List Wall -> List Marker -> Int -> Html Msg
showMarkers lw lm s = div[markerWrapper s] (List.map (showMarker lw) lm)

showMarker : List Wall -> Marker -> Html Msg
showMarker lw m = img [src <| markerImage m.c m.s, markerStyle <| first (getAt m.i lw)] []

showRobots : List Robot -> Int -> Html Msg
showRobots lr s = div [robotWrapper s] (List.map showRobot lr)

showRobot : Robot -> Html Msg
showRobot r = div [robotCellStyle r]
  [ img [src "media/Up.svg", onClick (Move (r, N)), buttonStyle N] [],
    img [src "media/Left.svg", onClick (Move (r, W)), buttonStyle W] [],
    img [src "media/Right.svg", onClick (Move (r, E)), buttonStyle E] [],
    img [src "media/Down.svg", onClick (Move (r, S)), buttonStyle S] [],
    img [src <| robotImage r.c, robotStyle] []
  ]

showWalls : List Wall -> Int -> Html Msg
showWalls lw s = div [wallWrapper s] (List.concat (List.map showWall lw))

showWall : Wall -> List (Html Msg)
showWall w = [div [wallStyle first w] [], div [wallStyle second w] []]


view : Model -> Html Msg
view model =  let
                internalWalls = (drop (model.b.s*2) model.b.v) ++ (drop (model.b.s*2) model.b.h)
              in
                div [style [("display","grid"),
                            ("grid-template-columns","900px 200px"),
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
                  div [style [("display","inline-flex"), ("flex-direction","column"),("align-items", "center"), ("justify-content", "center"),("width", "100%")]] [
                    showGoalMarker model.og.gm,
                    button [ onClick (Start), controlStyle ] [text "New game"],
                    button [ onClick (NextMarker), controlStyle ] [text "Next marker"],
                    button [ onClick (Reset), controlStyle ] [text "Reset"],
                    div [style [("font-size" , " 20px")]] [text <| String.concat ["Number of moves: ",(toString model.c)]]
                  ]
                ]

baseGame : Original
baseGame = {b = emptyBoard 10, r = [], m= [], gm = {c = Red, s = Moon, i = 0}}

gameGenerator : Int -> Int -> Generator Original
gameGenerator s w = Random.map4 Original (boardGenerator s w) (robotsGenerator s) (markersGenerator (w*2)) (markerGenerator)

update : Msg -> Model -> (Model, Cmd Msg)
update msg m = case msg of
              Move mv -> let rl = removeRobot m.r (first mv) in
                              ({m | r = (move (mv) m) :: rl, c = m.c+1}, Cmd.none)
              Start -> (m, newGameCommand)
              NewGame game -> (originalToModel game, Cmd.none)
              Reset -> (originalToModel m.og, Cmd.none)
              NewMarker marker -> ({m | og = {b = m.og.b, r = m.og.r, m = m.og.m, gm = marker}}, Cmd.none)
              NextMarker  -> (m, generate NewMarker markerGenerator )

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
