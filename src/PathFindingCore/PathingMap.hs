{-# LANGUAGE FlexibleInstances, TupleSections #-}
module PathFindingCore.PathingMap(findDirection, getTerrain, insertPath, markAsGoal, neighborsOf, PrintablePathingGrid(..), step) where

import Data.Array.IArray((!), (//), assocs, bounds)
import Data.List(filter, reverse, sortBy)
import Data.Text(chunksOf, replicate)

import Text.Printf(printf)

import PathFindingCore.PathingMap.Coordinate(Coordinate(Coord, x))
import PathFindingCore.PathingMap.Direction(Direction(East, North, South, West), directions)
import PathFindingCore.PathingMap.Interpreter(PathingGrid)
import PathFindingCore.PathingMap.Terrain(isPassable, Terrain(Goal, Path, Query, Self), terrainToChar)

newtype PrintablePathingGrid = PPG PathingGrid

getTerrain :: Coordinate -> PathingGrid -> Maybe Terrain
getTerrain coord@(Coord x y) grid = if isInBounds then Just $ grid ! coord else Nothing
  where
    (Coord x1 y1, Coord x2 y2) = bounds grid
    isInBounds                 = and [x >= x1, x <= x2, y >= y1, y <= y2]

neighborsOf :: Coordinate -> PathingGrid -> [Coordinate]
neighborsOf coordinate grid = directions |> ((fmap $ findNeighborCoord coordinate) >>> (filter canTravelTo))
  where
    canTravelTo = (flip getTerrain) grid >>> (fmap isPassable) >>> (fromMaybe False)

step :: Coordinate -> Coordinate -> PathingGrid -> PathingGrid
step prev new grid = grid // [(prev, Query), (new, Self)]

markAsGoal :: Coordinate -> PathingGrid -> PathingGrid
markAsGoal coord grid = grid // [(coord, Goal)]

insertPath :: [Coordinate] -> PathingGrid -> PathingGrid
insertPath coords grid = grid // (fmap (, Path) coords)

findNeighborCoord :: Coordinate -> Direction -> Coordinate
findNeighborCoord (Coord x y) North = Coord  x     (y + 1)
findNeighborCoord (Coord x y) South = Coord  x     (y - 1)
findNeighborCoord (Coord x y) East  = Coord (x + 1) y
findNeighborCoord (Coord x y) West  = Coord (x - 1) y

findDirection :: Coordinate -> Coordinate -> Direction
findDirection startCoord@(Coord x1 y1) endCoord@(Coord x2 y2)
    | y2 == y1 + 1 = North
    | y2 == y1 - 1 = South
    | x2 == x1 + 1 = East
    | x2 == x1 - 1 = West
    | otherwise    = error $ asText $ printf "Cannot find direction to non-adjacent coordinates (start: %s, end: %s)" (show startCoord) (show endCoord)

instance Show PrintablePathingGrid where
  show (PPG grid) = asString $ fold lines
    where
      maxX   = grid |> (bounds >>> snd >>> x >>> (+1))
      text   = grid |> (assocs >>> (sortBy sillySort) >>> (fmap $ snd >>> terrainToChar) >>> asText)
      lines  = text |> ((chunksOf maxX) >>> reverse >>> (makeLinesPretty maxX))

makeLinesPretty :: Int -> [Text] -> [Text]
makeLinesPretty maxX lines = concat [[topB], linesB, [botB]]
  where
    linesB = fmap (\x -> "|" <> x <> "|\n") lines
    border = replicate maxX "-"
    topB   = "+" <> border <> "+" <> "\n"
    botB   = "+" <> border <> "+"

sillySort :: (Coordinate, Terrain) -> (Coordinate, Terrain) -> Ordering
sillySort (Coord x1 y1, _) (Coord x2 y2, _) =
  if y1 < y2 then LT else if y1 > y2 then GT
     else if x1 < x2 then LT else if x1 > x2 then GT
       else EQ
