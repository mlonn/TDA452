module Board exposing (..)
import Wall exposing (..)

type alias Board = { v : List Wall, h : List Wall, s : Int}

emptyBoard : Int -> Board
emptyBoard s = {v = generateVWalls s, h = generateHWalls s, s = s}

generateHWalls : Int -> List Wall
generateHWalls s = case s of
  0 -> []
  _ -> makeHOutline s s

makeHOutline : Int -> Int -> List Wall
makeHOutline s i = case i of
  0 -> []
  _ -> [((i,0), (i,1)), ((i,s),(i,s+1))] ++ makeHOutline s (i-1)


generateVWalls : Int -> List Wall
generateVWalls s = case s of
  0 -> []
  _ -> makeVOutline s s

makeVOutline : Int -> Int -> List Wall
makeVOutline s i = case i of
  0 -> []
  _ -> [((0,i), (1,i)), ((s,i),(s+1,i))] ++ makeVOutline s (i-1)
