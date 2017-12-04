module Wall exposing (..)
import Common exposing (..)
import Random exposing (pair, Generator, int, list, map)

type alias Wall = (Pos, Pos)

vWallsGenerator : Int -> Int -> Generator (List Wall)
vWallsGenerator limit n = list n (vWallGenerator limit)

vWallGenerator: Int -> Generator Wall
vWallGenerator limit = let
                          leftPos = pair (int 1 (limit-1)) (int 1 (limit))
                        in
                          map (\(x,y) -> ((x,y), (x+1,y))) leftPos


hWallsGenerator : Int -> Int -> Generator (List Wall)
hWallsGenerator limit n = list n (hWallGenerator limit)

hWallGenerator: Int -> Generator Wall
hWallGenerator limit = let
                          topPos = pair (int 1 limit) (int 1 (limit-1))
                        in
                          map (\(x,y) -> ((x,y), (x,y+1))) topPos
