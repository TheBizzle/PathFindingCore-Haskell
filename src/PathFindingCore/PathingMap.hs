{-# LANGUAGE FlexibleInstances #-}
module PathFindingCore.PathingMap(findDirection, findNeighborCoord, getTerrain, insertPath, markAsGoal, neighborsOf, PrintablePathingGrid(..), step) where

  import Control.Arrow
  import Data.Array.IArray
  import Data.List.Split
  import Text.Printf

  import PathFindingCore.PathingMap.Coordinate
  import PathFindingCore.PathingMap.Direction
  import PathFindingCore.PathingMap.Interpreter
  import PathFindingCore.PathingMap.Terrain

  data PrintablePathingGrid
    = PPG {
        pathingGrid :: PathingGrid
      }

  a |> f = f a

  getTerrain :: PathingGrid -> Coordinate -> Terrain
  getTerrain grid (Coord x y) = if isInBounds then grid ! (x, y) else Invalid
    where
      ((x1, y1), (x2, y2)) = bounds grid
      isInBounds           = (x >= x1) && (x <= x2) && (y >= y1) && (y <= y2)

  neighborsOf :: PathingGrid -> Coordinate -> [Direction]
  neighborsOf grid coordinate = filter canTravelTo directions
    where
      canTravelTo = (findNeighborCoord coordinate) >>> (getTerrain grid) >>> isPassable

  step :: PathingGrid -> Coordinate -> Coordinate -> PathingGrid
  step grid (Coord x1 y1) (Coord x2 y2) = grid // [((x1, y1), Query), ((x2, y2), Self)]

  markAsGoal :: PathingGrid -> Coordinate -> PathingGrid
  markAsGoal grid (Coord x y) = grid // [((x, y), Goal)]

  insertPath :: PathingGrid -> [Coordinate] -> PathingGrid
  insertPath grid coords = grid // (fmap f coords)
    where
      f (Coord x y) = ((x, y), Path)

  findNeighborCoord :: Coordinate -> Direction -> Coordinate
  findNeighborCoord (Coord x y) dir = case dir of
    North -> Coord x       (y + 1)
    South -> Coord x       (y - 1)
    East  -> Coord (x + 1) y
    West  -> Coord (x - 1) y

  findDirection :: Coordinate -> Coordinate -> Direction
  findDirection startCoord@(Coord x1 y1) endCoord@(Coord x2 y2)
      | y2 == y1 + 1 = North
      | y2 == y1 - 1 = South
      | x2 == x1 + 1 = East
      | x2 == x1 - 1 = West
      | otherwise    = error (printf "Cannot find direction to non-adjacent coordinates (start: %s, end: %s)" (show startCoord) (show endCoord))

  instance Show PrintablePathingGrid where
    show (PPG grid) = foldr (++) [] lines
      where
        maxX   = grid |> (bounds >>> snd >>> fst >>> (+1))
        str    = grid |> (elems >>> (fmap terrainToChar))
        chunks = chunksOf maxX str
        lines  = chunks |> (rotateCounterClockwise >>> (makeLinesPretty maxX))

  makeLinesPretty :: Int -> [String] -> [String]
  makeLinesPretty maxX lines = concat [[topB], linesB, [botB]]
    where
      linesB = fmap (\x -> "|" ++ x ++ "|\n") lines
      border = replicate maxX '-'
      topB   = concat ["+", border, "+", "\n"]
      botB   = concat ["+", border, "+"]

  rotateCounterClockwise :: [[t]] -> [[t]]
  rotateCounterClockwise ts = reverse $ helper ts []
    where
      helper xs acc | isUseless xs = acc
      helper xs acc                = xs |> ((fmap unsnap) >>> unzip >>> (recurse acc))
      isUseless                    = concat >>> null
      unsnap []                    = error "Impossible condition achieved"
      unsnap xs                    = (last xs, init xs)
      recurse acc (row, remainder) = helper remainder (row : acc)
