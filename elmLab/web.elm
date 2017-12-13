module Web exposing (..)
{-| css stylings
@docs wallStyle
@docs wallBorderStyle
@docs robotWrapper
@docs boardWrapper
@docs markerWrapper
@docs markerImage
@docs markerStyle
@docs wallWrapper
@docs robotImage
@docs put
-}

import Tuple exposing (first , second)
import Common exposing (..)
import String exposing (concat)
import Board exposing (..)
import Marker exposing (..)
import Robot exposing (..)
import Html exposing (Html, div, text, Attribute, button, img)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, class, src, classList)

type Msg
  = Move Move
  | NewGame Original
  | Start
  | Restart
  | NewMarker Marker
  | NextMarker

type alias Move = (Robot, Direction)

type alias Original = {b: Board, r: List Robot, m: List Marker, gm: Marker}

{-| Adds styling to put a element in a grid -}
put : Int -> Int -> List (String, String)
put x y = [("grid-column", toString (x+1)), ("grid-row", toString (y+1))]

{-| the style for markers -}
markerStyle : Pos -> Attribute msg
markerStyle p = style <| ("width","100%") :: put (first p) (second p)

{-| Find where to place the wall -}
wallStyle : ((Pos, Pos) -> Pos) -> Wall -> Attribute msg
wallStyle f w = style ([
    wallBorderStyle f w] ++ (put (first (f w)) (second (f w))))

{-| the style for walls -}
wallBorderStyle : ((Pos, Pos) -> Pos) -> Wall -> (String, String)
wallBorderStyle f w =
  let ws = "solid 2px #212d35" in
  if f w == first w
  then ( if checkH w then ("border-right", ws) else ("border-bottom", ws) )
  else ( if checkH w then ("border-left", ws) else ("border-top", ws) )

checkH : Wall -> Bool
checkH w = second (first w) == second (second w)

{-| Styling for bottom layer -}
boardWrapper : Int -> Attribute msg
boardWrapper s = style (wrapper s)

{-| style for robot layer -}
robotWrapper : Int -> Attribute msg
robotWrapper s = style (("z-index","20") :: wrapper s)

{-| style for marker layer -}
markerWrapper : Int -> Attribute msg
markerWrapper s = style (("z-index","15") :: wrapper s)

{-| style for wall layer -}
wallWrapper : Int -> Attribute msg
wallWrapper s = style (("z-index", "10") :: wrapper s)

{-| basic layer stylings -}
wrapper : Int -> List (String, String)
wrapper s = let screen = 900 in
  [("display", "grid"),
  ("grid-template-columns", concat ["repeat(", toString (s+2) , ", ",
                                          toString (screen//(s+2)), "px)"]),
  ("grid-auto-rows", concat [toString (screen//(s+2)), "px"]),
  ("position","absolute")
  ]

{-| Given a color returns the correct image -}
robotImage : Color -> String
robotImage c = concat ["media/", (toString c), "/Robot.svg"]

{-| Given a color returns the correct image -}
markerImage : Color -> Symbol -> String
markerImage c s = concat ["media/", (toString c), "/", (toString s), ".svg"]

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

showControls : Marker -> Int-> Html Msg
showControls gm c=
  div [ class "control-container"] [
        showGoalMarker gm,
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
                                          (toString c)]
                ]
      ]
showWinScreen : Int -> Html Msg
showWinScreen c =
  div [ class "win"]  [
        div [ class "win-content",
              style [("grid-row","2")]] [
                div [] [
                          text <|
                          String.concat["You won in ", toString c, " moves!"]
                        ]
        ],
        div [ class "win-content",
              style [("grid-row","3")]] [
              button [onClick (Start),
                      classList[("btn", True),("btn-lg", True)]] [
                                  text "New game"
                      ],
              button [onClick (NextMarker),
                      classList[("btn", True),("btn-lg", True)]] [
                                  text "Next marker"
                      ],
              button [onClick (Restart),
                      classList[("btn", True),("btn-lg", True)]] [
                                  text "Restart"
                      ]
        ]
  ]
