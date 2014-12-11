module PathingMapTests where

import Test.Framework.Providers.API(Test, testGroup)
import Test.Framework.Providers.HUnit(testCase)
import Test.HUnit((@?=))

import Control.Arrow((>>>))

import Data.Array.IArray()

import PathFindingCore.PathingMap.Coordinate(Coordinate(Coord))
import PathFindingCore.PathingMap.Direction(Direction(East, North, South, West))
import PathFindingCore.PathingMap.Interpreter(fromMapString, grid, PathingGrid, PathingMapString(PathingMapString))
import PathFindingCore.PathingMap.Terrain(Terrain(Empty, Wall))
import PathFindingCore.PathingMap(findDirection, getTerrain, insertPath, markAsGoal, neighborsOf, step)

tests = testGroup "Test interpreter" [
   testInterpreter "getTerrain 1"  (getTerrain  $ Coord 9001 9001) Nothing
 , testInterpreter "getTerrain 2"  (getTerrain  $ Coord 1 1)       $ Just Wall
 , testInterpreter "getTerrain 3"  (getTerrain  $ Coord 3 0)       $ Just Empty
 , testInterpreter "neighborsOf 1" (neighborsOf $ Coord 9001 9001) []
 , testInterpreter "neighborsOf 2" (neighborsOf $ Coord 2 0)       [Coord 2 1, Coord 3 0]
 , testInterpreter "neighborsOf 3" (neighborsOf $ Coord 3 0)       [Coord 2 0]
 , testInterpreter "step"          (step         (Coord 2 0) (Coord 2 1)) $ gridFromString " DGD | DDD |%%%% |DD*%%|*D. %"
 , testInterpreter "markAsGoal 1"  (markAsGoal  $ Coord 2 0) $ gridFromString " DGD | DDD |%%%% |DD %%|*DG %"
 , testInterpreter "markAsGoal 2"  (markAsGoal  $ Coord 2 1) $ gridFromString " DGD | DDD |%%%% |DDG%%|*D  %"
 , testInterpreter "insertPath 1"  (insertPath  cPath1)      $ gridFromString " DGD | DDDx|%%%%x|DD %%|*D  %"
 , testInterpreter "insertPath 2"  (insertPath  cPath2)      $ gridFromString " DGD | DDD |%%%% |DDx%%|*Dxx%"
 , testInterpreter "insertPath 3"  (insertPath  [])          $ gridFromString " DGD | DDD |%%%% |DD %%|*D  %"
 , testInterpreter "findDirection 1" (\x -> findDirection (Coord 2 0) (Coord 2 1)) $ North
 , testInterpreter "findDirection 2" (\x -> findDirection (Coord 2 0) (Coord 3 0)) $ East
 , testInterpreter "findDirection 3" (\x -> findDirection (Coord 2 0) (Coord 1 0)) $ West
 ]
 where
   cPath1 = [(Coord 4 3), (Coord 4 2)]
   cPath2 = [(Coord 2 1), (Coord 2 0), (Coord 3 0)]

testInterpreter :: (Eq t, Show t) => String -> (PathingGrid -> t) -> t -> Test
testInterpreter desc genActual expected = testCase desc assertion
  where
    grid      = gridFromString " DGD | DDD |%%%% |DD %%|*D  %"
    assertion = (genActual grid) @?= expected

a |> f = f a

gridFromString :: String -> PathingGrid
gridFromString str = str |> ((\x -> PathingMapString x "|") >>> fromMapString >>> grid)
