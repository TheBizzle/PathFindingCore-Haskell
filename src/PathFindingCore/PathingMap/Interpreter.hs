module PathFindingCore.PathingMap.Interpreter(fromMapString, PathingGrid, PathingMapString(..), PathingMapData(..)) where

import Data.Array.IArray(Array, assocs, listArray)
import Data.List(drop, isSuffixOf, last, reverse, unzip)
import Data.List.Split(splitOn)

import PathFindingCore.PathingMap.Coordinate(Coordinate(Coord))
import PathFindingCore.PathingMap.Terrain(charToTerrain, Terrain(Goal, Self))

type PathingGrid = Array Coordinate Terrain

data PathingMapString
  = PathingMapString {
      str   :: Text,
      delim :: Text
    } deriving (Eq)

data PathingMapData
  = PathingMapData {
      start :: Coordinate,
      goal  :: Coordinate,
      grid  :: PathingGrid
    } deriving (Eq, Show)

fromMapString :: PathingMapString -> PathingMapData
fromMapString (PathingMapString ""  _)     = error "Cannot build map from empty string"
fromMapString (PathingMapString str delim) = PathingMapData start goal grid
  where
    sDelim        = asString delim
    grid          = str |> (asString >>> (dropDelim sDelim) >>> (splitOn sDelim) >>> strListToGrid)
    (start, goal) = findStartAndGoal grid
    dropDelim d s = if (isSuffixOf d s) then s |> (reverse >>> (drop $ length d) >>> reverse) else s

-- The need for `rotateClockwise` likely seems bizarre, at first glance.  To frame the reason for its necessity:
-- We start with rows of strings (strs[a][b] => a: 0 = top row, b: 0 = leftmost character) and we need to transform
-- it such that it follows normal Cartesian coordinate rules (strs[a][b] => a: 0 = leftmost character, b: 0 = bottom
-- row) --JAB (8/13/14)
strListToGrid :: [String] -> PathingGrid
strListToGrid strList = listArray (Coord 0 0, endCoord) terrains
  where
    str      = fold $ rotateClockwise strList
    terrains = fmap charToTerrain str
    length'  = length >>> (subtract 1)
    xLength  = strList |> (last >>> length')
    yLength  = strList |> (length')
    endCoord = Coord xLength yLength

findStartAndGoal :: PathingGrid -> (Coordinate, Coordinate)
findStartAndGoal arr = analyzeResult $ foldr findBest (Nothing, Nothing) (assocs arr)
  where

    findBest (coord, Self) (_, g) = (Just coord, g)
    findBest (coord, Goal) (s, _) = (s,          Just coord)
    findBest _             (s, g) = (s,          g)

    analyzeResult (Nothing,    _      )   = error "No start in given grid."
    analyzeResult (_,          Nothing)   = error "No goal in given grid."
    analyzeResult (Just start, Just goal) = (start, goal)

rotateClockwise :: [[t]] -> [[t]]
rotateClockwise ts = reverse (helper ts [])
  where
    helper xs acc | isUseless xs = acc
    helper xs acc                = xs |> ((fmap unsnap) >>> unzip >>> (recurse acc))
    isUseless                    = concat >>> null
    unsnap (x : xs)              = (x, xs)
    unsnap []                    = error "Impossible condition achieved"
    recurse acc (row, remainder) = helper remainder $ (reverse row) : acc
