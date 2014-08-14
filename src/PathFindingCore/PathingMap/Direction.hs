module PathFindingCore.PathingMap.Direction where

  data Direction
    = North
    | East
    | South
    | West deriving (Eq, Show)

  directions :: [Direction]
  directions = [North, East, South, West]
