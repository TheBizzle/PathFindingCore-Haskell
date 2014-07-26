module PathFindingCore.PathingMap.Direction where

  data Direction
    = North
    | East
    | South
    | West

  toList :: [Direction]
  toList = [North, East, South, West]
