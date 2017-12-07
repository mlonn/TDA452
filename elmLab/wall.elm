module Wall exposing (Wall, vWallsGenerator, hWallsGenerator)
{-|Wall
@docs Wall
@docs vWallsGenerator
@docs hWallsGenerator
-}
import Common exposing (..)
import Random exposing (pair, Generator, int, list, map)

{-| -}
type alias Wall = (Pos, Pos)

{-| -}
vWallsGenerator : List Wall -> Generator (List Wall)
vWallsGenerator l = map (vWallGenerator l (int 1 4))

vWallGenerator: Wall -> Int -> Generator Wall
vWallGenerator wall i = let
                          topPos = first wall
                          bottomPos = second wall
                        in
                          case i of
                          0 -> map (\(x,y) -> ((x,y), (x,y+1))) topPos
                          1 -> map (\(x,y) -> ((x,y), (x,y+1))) topPos
                          2 -> map (\(x,y) -> ((x,y), (x,y+1))) topPos
                          _ -> map (\(x,y) -> ((x,y), (x,y+1))) topPos
{-| -}
hWallsGenerator : Int -> Int -> Generator (List Wall)
hWallsGenerator limit n = list n (hWallGenerator limit)

hWallGenerator: Int -> Generator Wall
hWallGenerator limit = let
                          topPos = pair (int 1 limit) (int 1 (limit-1))
                        in
                          map (\(x,y) -> ((x,y), (x,y+1))) topPos
