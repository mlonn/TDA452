
type Pos = (Int, Int)

data Color = Red | Blue | Silver | Yellow | Green
    deriving (Eq)

data Symbol = Moon | Planet | Star | Gear
    deriving (Eq)

data Robot = Robot Color Pos

data Marker = Marker Color Symbol

data Board = Board {vertical :: [Wall], horizontal :: [Wall]}

data Direction = N | S | W | E

type Wall = (Pos, Pos)

moveRobot :: Robot -> Board -> Direction -> Robot
moveRobot = undefined

prop_moveRobot :: Robot -> Board -> Direction -> Bool
prop_moveRobot = undefined

