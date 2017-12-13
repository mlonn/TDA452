module Main exposing (prop_robot,
                      prop_win)
{-| The main file
@dohasWon prop_robot
@dohasWon prop_win
-}
import Tuple exposing (first, second)
import List exposing (member, map, unzip, drop)
import Random.Pcg as Random exposing (Generator, generate)
import Html exposing (program, Html, div, text, Attribute, button, img)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, class, src, classList)
import Common exposing (..)
import Robot exposing (..)
import Board exposing (..)
import Marker exposing (..)
import Styles exposing (..)
import Test exposing (Test, describe, test, fuzz, fuzz2)
import Fuzz exposing (..)
import Expect

type alias Move = (Robot, Direction)

type alias Original = {b: Board, r: List Robot, m: List Marker, gm: Marker}

type alias Model = {r: List Robot, c: Int, og: Original}


type Msg
  = Move Move
  | NewGame Original
  | Start
  | Restart
  | NewMarker Marker
  | NextMarker

{-| Moves a robot until it reaches a wall or another robot -}
move : Move -> Model -> Robot
move (r , d) m = if isWall r.p d m.og.b || isRobot m.r (moveRobot r d) then
                  r
                else
                  move ((moveRobot r d ), d) m

{-|
Test for robots,
    Tests that robots stops at walls,
    Removing a robot from robots such that that robot is removed and the list
    of robots is one shorter.
-}
prop_robot : Test
prop_robot = describe "Robot tests"
  [ fuzz2 (robot 20) direction "Should move until wall"
    (\r d ->
      let
        am = move (r, d)
                  {r= [r],c= 0,og={b=emptyBoard 20, r=[r], m=[],
                      gm = {c = Red, s = Moon, i = 0, r=0}}}
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
         Just r -> member r (removeRobot (List.filter
                                      (\x -> x.c /= r.c && r.p /= x.p) lr) r)
                                      |> Expect.equal False
         Nothing -> Expect.pass
      ),
    fuzz (Fuzz.list (robot 20)) "Testing size remove"
      (\lr ->
        let fr = List.head lr in
        case fr of
         Just r -> List.length (removeRobot lr r)
                                      |> Expect.equal ((List.length lr) - 1)
         Nothing -> Expect.pass
      )

  ]
{-| Returns true if a robot of correct color is on the goal marker (gm)-}
hasWon : Model -> Bool
hasWon m =
  case List.head (List.filter (\x -> x.c == m.og.gm.c) m.r) of
    Just r -> case List.head
                    (List.filter
                      (\x -> x.c == m.og.gm.c && x.s == m.og.gm.s) m.og.m) of
                Just mark -> let w =
                              (getAt mark.i (internalWalls m.og))
                             in
                              r.p ==
                                (if mark.r == 0 then
                                  first
                                else
                                  second) w
                Nothing -> False
    Nothing -> False

{-| test the hasWon function with hard coded moves that should or should
    not win with random robots-}
prop_win : Test
prop_win = describe "tests for winning"
  [
    fuzz2 (robot 20) symbol "Should win" (
    \r s -> let
              mark = {c = r.c, s = s, i = 0, r= 0}
              m = {r=[r],
                  c=0,
                  og={b= mergeBoards (emptyBoard 20) {v=lw, h=lw, s=20},
                  r = [r],
                  m= [mark],
                  gm = mark}}
              lw =[((r.p),(r.p))]
            in
              hasWon m |> Expect.equal True
    ),
    fuzz2 (robot 20) symbol "Should not win" (
    \r s -> let
              mark = {c = r.c, s = s, i = 0, r= 0}
              m = {r=[r],
                    c=0,
                    og={b= mergeBoards (emptyBoard 20) {v=lw, h=lw, s=20},
                    r = [r], m= [mark],
                    gm = mark}}
              notRpos = (1 + first r.p, second r.p)
              lw =[(notRpos, notRpos)]
            in
              hasWon m |> Expect.equal False
    )
  ]

{-| Checks is a robot shares posotion with another robot-}
isRobot : List Robot -> Robot -> Bool
isRobot lr r = member r.p (List.map (\x -> x.p) lr)

{-|Checiks if a there is a wall in a given direction from a position-}
isWall : Pos -> Direction -> Board -> Bool
isWall p d b = case d of
  N -> (member p (List.map second b.h))
  S -> (member p (List.map first b.h))
  W -> (member p (List.map second b.v))
  E -> (member p (List.map first b.v))

{-| Generates html code to show a board-}
showBoard : Int -> Html Msg
showBoard i = div [boardWrapper i] (makeCells (i) (i))

{-| Generates html code for all cells given a dimension-}
makeCells : Int -> Int -> List (Html Msg)
makeCells s i = case i of
  0 -> []
  _ -> makeRow s i ++ makeCells s (i-1)

{-| Generates html code for a row of cells-}
makeRow : Int -> Int -> List (Html Msg)
makeRow s i =
  case s of
    0 -> []
    _ -> makeCell s i :: makeRow (s-1) i

{-| Generates html code for a single cell-}
makeCell : Int -> Int -> Html Msg
makeCell s i = div [class "base-cell", style <| put s i] []

{-| Generate html code for the goal marker -}
showGoalMarker : Marker -> Html Msg
showGoalMarker m = img [src <| markerImage m.c m.s, class "goal-marker"] []

{-| Generates html code for all markers-}
showMarkers :  List Wall -> List Marker -> Int -> Html Msg
showMarkers lw lm s = div[markerWrapper s] (List.map (showMarker lw) lm)

{-| Generates html code for a marker-}
showMarker : List Wall -> Marker -> Html Msg
showMarker lw m =
  let
    p = getAt m.i lw
  in
    if m.r == 0 then
        img [src <| markerImage m.c m.s,
              class "marker",
              style <| put (first (first p)) (second (first p))] []
      else
        img [src <| markerImage m.c m.s,
              class "marker",
              style <| put (first (second p)) (second (second p))] []



{-| Translates Robots to html -}
showRobots : List Robot -> Int -> Html Msg
showRobots lr s = div [robotWrapper s] (List.map showRobot lr)

{-| Translates a robot to html -}
showRobot : Robot -> Html Msg
showRobot r = div [class "robot-cell",style <| put (first r.p) (second r.p)]
  [ img [
          src "media/Up.svg",
          onClick (Move (r, N)),
          classList [
                      ("btn-dir", True),
                      ("btn-dir-north", True)
                    ]
        ] [],
    img [
          src "media/Left.svg",
          onClick (Move (r, W)),
          classList [
                      ("btn-dir", True),
                      ("btn-dir-west", True)
                    ]
        ] [],
    img [
          src "media/Right.svg",
          onClick (Move (r, E)),
          classList [
                      ("btn-dir", True),
                      ("btn-dir-east", True)
                    ]
        ] [],
    img [
          src "media/Down.svg",
          onClick (Move (r, S)),
          classList [
                      ("btn-dir", True),
                      ("btn-dir-south", True)
                    ]
        ] [],
    img [
          src <| robotImage r.c,
          class "robot"
        ] []
  ]

{-| Translates walls to html -}
showWalls : List Wall -> Int -> Html Msg
showWalls lw s = div [wallWrapper s] (List.concat (List.map showWall lw))

{-| Translates a single wall to html -}
showWall : Wall -> List (Html Msg)
showWall w = [div [wallStyle first w] [], div [wallStyle second w] []]

{-| Removes all outerwalls from an original -}
internalWalls : Original -> List Wall
internalWalls og = (drop (og.b.s*2) og.b.v) ++ (drop (og.b.s*2) og.b.h)

{-| The output, all html code is made here. Also checks
if we've won or not to know if the overlay when you've won should be there
or not. -}
view : Model -> Html Msg
view model =
  let
    iw = internalWalls model.og
  in
    div [ class "container"] [
          div [ style [("height", "900px")]] [
                showBoard model.og.b.s,
                showRobots model.r model.og.b.s,
                showWalls (model.og.b.v ++ model.og.b.h) model.og.b.s,
                showMarkers iw model.og.m model.og.b.s
              ],
                div [ class "control-container"] [
                      showGoalMarker model.og.gm,
                      button  [onClick (Start),
                                classList [("btn", True), ("btn-sm", True)]] [
                                  text "New game"
                              ],
                      button  [ onClick (NextMarker),
                                classList[("btn", True),("btn-sm", True)]] [
                                  text "Next marker"
                              ],
                      button [  onClick (Restart),
                                classList[("btn", True),("btn-sm", True)]] [
                                  text "Restart"
                              ],
                      div [] [
                                text <| String.concat ["Number of moves: ",
                                                        (toString model.c)]
                              ]
                    ],
                    if hasWon model then
                      div [ class "win"]  [
                            div [ class "win-content",
                                  style [("grid-row","2")]] [
                                    div [] [
                                              text <|
                                              String.concat["You won in ",
                                              toString model.c, " moves!"]
                                            ]
                            ],
                            div [ class "win-content",
                                  style [("grid-row","3")]] [
                                  button [onClick (Start),
                                          classList[("btn", True),
                                                    ("btn-lg", True)]] [
                                                      text "New game"
                                          ],
                                  button [onClick (NextMarker),
                                          classList[("btn", True),
                                                    ("btn-lg", True)]] [
                                                      text "Next marker"
                                          ],
                                  button [onClick (Restart),
                                          classList[("btn", True),
                                                    ("btn-lg", True)]] [
                                                      text "Restart"
                                          ]
                            ]
                      ]
                      else text ""
    ]

{-| A base version of the game that is a placeholder
until a game has been generated -}
baseGame : Original
baseGame = {b = emptyBoard 10,
            r = [],
            m= [],
            gm = {
                    c = Red,
                    s = Moon,
                    i = 0,
                    r = 0}
                  }

{-| Returns a generator for originals -}
gameGenerator : Int -> Int -> Generator Original
gameGenerator s w = Random.map4 Original  (boardGenerator s w)
                                          (robotsGenerator s)
                                          (markersGenerator (w*2))
                                          (markerGenerator)

{-| Does different things depending on what msg was called.
Mostly referes to other methods -}
update : Msg -> Model -> (Model, Cmd Msg)
update msg m = case msg of
              Move mv -> let
                            rl = removeRobot m.r (first mv)
                            mr = (move (mv) m)
                            am = {m | r = (move (mv) m) :: rl, c = m.c+1}
                         in (am, Cmd.none)
              Start -> (m, newGameCommand)
              NewGame og -> if checkOverlap og.m (og.b.v ++ og.b.h) then
                              ({m|c= m.c + 1}, newGameCommand)
                            else
                              (originalToModel
                                {og | b = mergeBoards
                                          (emptyBoard og.b.s)
                                          og.b
                                }, Cmd.none)
              Restart -> (originalToModel m.og, Cmd.none)
              NewMarker marker -> ({m | og = {b = m.og.b,
                                              r = m.r,
                                              m = m.og.m,
                                              gm = marker},
                                              c = 0}, Cmd.none)
              NextMarker  -> (m, generate NewMarker markerGenerator )

{-| Checks if any markers are on the same position -}
checkOverlap : List Marker -> List Wall -> Bool
checkOverlap lm lw =
  isOverlapping <| List.map
                    (\m -> (if m.r == 0 then first else second )
                    (getAt m.i lw))
                    lm

{-| Checks if the any positions overlap -}
isOverlapping : List Pos -> Bool
isOverlapping pos = case pos of
  (p::ps) -> List.member p ps || isOverlapping ps
  [] -> False

{-| removes a robot from a list of robots -}
removeRobot : List Robot -> Robot -> List Robot
removeRobot lr r =case lr of
  (x :: xs) -> if x.c == r.c
               then xs
               else x :: removeRobot xs r
  [] -> []

{-| command that generates all things needed for a game -}
newGameCommand : Cmd Msg
newGameCommand = generate NewGame (gameGenerator 16 25)

{-| sets up a base for a game -}
init : (Model, Cmd Msg)
init = (originalToModel baseGame, newGameCommand)

{-| makes a model from an original -}
originalToModel : Original -> Model
originalToModel og = {
                  r = og.r,
                  c = 0,
                  og = og
                  }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

{-| Starts the program-}
main : Program Never Model Msg
main = program {init = init,
                view = view,
                update = update,
                subscriptions = subscriptions}

-- prop_moveRobot : Robot -> Board -> Direction -> Bool
