module PathFindingCore.PathingMap.Interpreter(fromMapString, PathingGrid, PathingMapString(..), PathingMapData(..)) where

  import Control.Arrow
  import Data.Array.IArray
  import Data.List.Split

  import PathFindingCore.PathingMap.Coordinate
  import PathFindingCore.PathingMap.Terrain

  type PathingGrid = Array (Int, Int) Terrain

  data PathingMapString
    = PathingMapString {
        str   :: String,
        delim :: String
      } deriving (Eq)

  data PathingMapData
    = PathingMapData {
        start :: Coordinate,
        goal  :: Coordinate,
        grid  :: PathingGrid
      } deriving (Eq, Show)

  a |> f = f a

  fromMapString :: PathingMapString -> PathingMapData
  fromMapString (PathingMapString str delim) = PathingMapData start goal grid
    where
      grid          = str |> ((splitOn delim) >>> strListToGrid)
      (start, goal) = findStartAndGoal grid

  strListToGrid :: [String] -> PathingGrid
  strListToGrid strList = listArray ((0, 0), endTuple) terrains
    where
      str      = foldr (++) [] (reverse strList) --Reverse the list to vertically flip the map so it prints out sensically
      terrains = fmap charToTerrain str
      length'  = length >>> (+(-1))
      xLength  = strList |> (last >>> length')
      yLength  = strList |> (length')
      endTuple = (xLength, yLength)

  findStartAndGoal :: PathingGrid -> (Coordinate, Coordinate)
  findStartAndGoal arr =
    let result = foldr findBest (BadCoord, BadCoord) (assocs arr)
    in case result of
      (BadCoord, _       ) -> error "No start in given grid."
      (_,        BadCoord) -> error "No goal in given grid."
      startAndGoal         -> startAndGoal
    where
      findBest ((x, y), Self) (_, g) = (Coord x y, g)
      findBest ((x, y), Goal) (s, _) = (s,         Coord x y)
      findBest _              (s, g) = (s,         g)
