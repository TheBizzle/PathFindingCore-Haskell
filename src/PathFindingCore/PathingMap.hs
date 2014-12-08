{-# LANGUAGE FlexibleInstances #-}
module PathFindingCore.PathingMap(findDirection, getTerrain, insertPath, markAsGoal, neighborsOf, PrintablePathingGrid(..), step) where

  import Control.Arrow
  import Data.Array.IArray
  import Data.List
  import Data.List.Split
  import Data.Maybe
  import Data.Ord
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

  getTerrain :: Coordinate -> PathingGrid -> Maybe Terrain
  getTerrain coord@(Coord x y) grid = if isInBounds then Just $ grid ! coord else Nothing
    where
      (Coord x1 y1, Coord x2 y2) = bounds grid
      isInBounds                 = (x >= x1) && (x <= x2) && (y >= y1) && (y <= y2)

  neighborsOf :: Coordinate -> PathingGrid -> [Coordinate]
  neighborsOf coordinate grid = directions |> ((fmap $ findNeighborCoord coordinate) >>> (filter canTravelTo))
    where
      canTravelTo = (flip getTerrain) grid >>> (fmap isPassable) >>> (fromMaybe False)

  step :: Coordinate -> Coordinate -> PathingGrid -> PathingGrid
  step prev new grid = grid // [(prev, Query), (new, Self)]

  markAsGoal :: Coordinate -> PathingGrid -> PathingGrid
  markAsGoal coord grid = grid // [(coord, Goal)]

  insertPath :: [Coordinate] -> PathingGrid -> PathingGrid
  insertPath coords grid = grid // (fmap f coords)
    where
      f coord = (coord, Path)

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
      | otherwise    = error $ printf "Cannot find direction to non-adjacent coordinates (start: %s, end: %s)" (show startCoord) (show endCoord)

  instance Show PrintablePathingGrid where
    show (PPG grid) = foldr (++) [] lines
      where
        maxX   = grid |> (bounds >>> snd >>> x >>> (+1))
        str    = grid |> (assocs >>> (sortBy sillySort) >>> (fmap $ snd >>> terrainToChar))
        lines  = str  |> ((chunksOf maxX) >>> reverse >>> (makeLinesPretty maxX))

  makeLinesPretty :: Int -> [String] -> [String]
  makeLinesPretty maxX lines = concat [[topB], linesB, [botB]]
    where
      linesB = fmap (\x -> "|" ++ x ++ "|\n") lines
      border = replicate maxX '-'
      topB   = concat ["+", border, "+", "\n"]
      botB   = concat ["+", border, "+"]

  sillySort :: (Coordinate, Terrain) -> (Coordinate, Terrain) -> Ordering
  sillySort (Coord x1 y1, _) (Coord x2 y2, _) =
    if y1 < y2 then LT else if y1 > y2 then GT
       else if x1 < x2 then LT else if x1 > x2 then GT
         else EQ
