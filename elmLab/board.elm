module Board exposing (Board,
                        emptyBoard,
                        boardGenerator,
                        prop_emptyBoard,
                        Wall)
{-|  Board and its functions
@docs Board
@docs boardGenerator
@docs emptyBoard
@docs prop_emptyBoard
@docs Wall
-}
import Random.Pcg as Random exposing (..)
import Test exposing (Test, fuzz, describe)
import Fuzz
import Expect
import Common exposing (Pos)
import List exposing (length)
{-| -}
type alias Wall = (Pos, Pos)
{-| -}
type alias Board = { v : List Wall, h : List Wall, s : Int}
{-| -}
emptyBoard : Int -> Board
emptyBoard s = {v = generateVWalls s, h = generateHWalls s, s = s}
{-|-}
prop_emptyBoard : Test
prop_emptyBoard = describe "checking empty generation"
                      [ fuzz (Fuzz.intRange 1 100) "Checking size"
                        (\i -> let b = emptyBoard i in (length b.v) +
                          (length b.h) |> Expect.equal (i*4) )
                      ]

generateHWalls : Int -> List Wall
generateHWalls s = makeHOutline s s

makeHOutline : Int -> Int -> List Wall
makeHOutline s i = case i of
  0 -> []
  _ -> [((i,0), (i,1)), ((i,s),(i,s+1))] ++ makeHOutline s (i-1)


generateVWalls : Int -> List Wall
generateVWalls s = makeVOutline s s

makeVOutline : Int -> Int -> List Wall
makeVOutline s i = case i of
  0 -> []
  _ -> [((0,i), (1,i)), ((s,i),(s+1,i))] ++ makeVOutline s (i-1)

{-| -}
boardGenerator : Int -> Int -> Generator Board
boardGenerator s w = map3 Board
                            (vWallsGenerator s w)
                            (hWallsGenerator s w)
                            (int s s)

mergeBoards : Board -> Board -> Board
mergeBoards b1 b2 = if (b1.s == b2.s) then
                      {v = b1.v++b2.v, h= b1.h++b2.h, s=b1.s}
                    else {v = b1.v, h=b1.h, s=b1.s}

vWallsGenerator : Int -> Int -> Generator (List Wall)
vWallsGenerator limit n = list n (vWallGenerator limit)

vWallGenerator: Int -> Generator Wall
vWallGenerator limit = let
                         leftPos = pair (int 1 (limit-1)) (int 1 (limit))
                       in
                         Random.map (\(x,y) -> ((x,y), (x+1,y))) leftPos


hWallsGenerator : Int -> Int -> Generator (List Wall)
hWallsGenerator limit n = list n (hWallGenerator limit)

hWallGenerator: Int -> Generator Wall
hWallGenerator limit = let
                         topPos = pair (int 1 limit) (int 1 (limit-1))
                       in
                         Random.map (\(x,y) -> ((x,y), (x,y+1))) topPos
