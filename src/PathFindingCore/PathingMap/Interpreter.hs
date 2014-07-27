module PathFindingCore.PathingMap.Interpreter(fromMapString, PathingGrid, PathingMapString, PathingMapData) where

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
      }

  data PathingMapData
    = PathingMapData {
        start :: Coordinate,
        goal  :: Coordinate,
        grid  :: PathingGrid
      }

  a |> f = f a

  fromMapString :: PathingMapString -> PathingMapData
  fromMapString (PathingMapString str delim) =
    let grid          = str |> ((splitOn delim) >>> strListToGrid)
        (start, goal) = findStartAndGoal grid
    in (PathingMapData start goal grid)

  strListToGrid :: [String] -> PathingGrid
  strListToGrid strList =
    let str      = foldr (++) [] (reverse strList) --Reverse the list to vertically flip the map so it prints out sensically
        terrains = fmap charToTerrain str
        lastX    = strList |> (last >>> length)
    in listArray ((0, 0), ((length strList) - 1, lastX)) terrains
    where
      convertInner :: String -> Array Int Terrain
      convertInner str =
        let innerBounds = (0, (length str) - 1)
        in str |> ((fmap charToTerrain) >>> (listArray innerBounds))

  findStartAndGoal :: PathingGrid -> (Coordinate, Coordinate)
  findStartAndGoal arr =
    let pairs  = zip (indices arr) (elems arr)
        result = foldr findBest (BadCoord, BadCoord) $ pairs
    in case result of
      (BadCoord, _       ) -> error "No start in given grid."
      (_,        BadCoord) -> error "No goal in given grid."
      startAndGoal         -> startAndGoal
    where
      findBest ((x, y), Self) (_, g) = (Coord x y, g)
      findBest ((x, y), Goal) (s, g) = (s,         Coord x y)
      findBest _              (s, g) = (s,         g)
