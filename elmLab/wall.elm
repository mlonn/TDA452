module Wall exposing (Wall, vWallsGenerator, hWallsGenerator)
{-|Wall
@docs Wall
@docs vWallsGenerator
@docs hWallsGenerator
-}
import Common exposing (..)
import Tuple exposing (first, second)
import List exposing (length)
import Random.Pcg as Random exposing (pair, Generator, int, list, map)

{-| -}
type alias Wall = (Pos, Pos)

{-| -}
vWallsGenerator : List Wall -> Generator (List Wall)
vWallsGenerator l = flattenList <| List.map vWallGenerator l

vWallGenerator: Wall -> Generator Wall
vWallGenerator wall = let
                          topPos = first wall
                          topX = first topPos
                          topY = second topPos
                          bottomPos = second wall
                          bottomX = first topPos
                          bottomY = second topPos
                        in
                          pair (pair (int topX topX) (int topY topY)) (pair (int (topX + 1) (topX + 1)) (int topY topY))
{-| -}
hWallsGenerator : Int -> Int -> Generator (List Wall)
hWallsGenerator limit n = list n (hWallGenerator limit)

hWallGenerator: Int -> Generator Wall
hWallGenerator limit = let
                          topPos = pair (int 1 limit) (int 1 (limit-1))
                        in
                          map (\(x,y) -> ((x,y), (x,y+1))) topPos
