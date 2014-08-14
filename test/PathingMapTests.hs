module PathingMapTests where

import Test.Framework.Providers.API as API
import Test.Framework.Providers.HUnit
import Test.HUnit

import Control.Arrow

import Data.Array.IArray

import PathFindingCore.PathingMap.Coordinate
import PathFindingCore.PathingMap.Direction
import PathFindingCore.PathingMap.Interpreter
import PathFindingCore.PathingMap.Terrain
import PathFindingCore.PathingMap

tests = testGroup "Test interpreter" [
   testInterpreter "getTerrain 1"  (\x -> getTerrain  x (Coord 9001 9001)) Invalid
 , testInterpreter "getTerrain 2"  (\x -> getTerrain  x (Coord 1 1))       Wall
 , testInterpreter "getTerrain 3"  (\x -> getTerrain  x (Coord 3 0))       Empty
 , testInterpreter "neighborsOf 1" (\x -> neighborsOf x (Coord 9001 9001)) []
 , testInterpreter "neighborsOf 2" (\x -> neighborsOf x (Coord 2 0))       [North, East]
 , testInterpreter "neighborsOf 3" (\x -> neighborsOf x (Coord 3 0))       [West]
 , testInterpreter "step"          (\x -> step        x (Coord 2 0) (Coord 2 1)) $ gridFromString " DGD | DDD |%%%% |DD*%%|*D. %"
 , testInterpreter "markAsGoal 1"  (\x -> markAsGoal  x (Coord 2 0)) $ gridFromString " DGD | DDD |%%%% |DD %%|*DG %"
 , testInterpreter "markAsGoal 2"  (\x -> markAsGoal  x (Coord 2 1)) $ gridFromString " DGD | DDD |%%%% |DDG%%|*D  %"
 , testInterpreter "insertPath 1"  (\x -> insertPath  x cPath1)      $ gridFromString " DGD | DDDx|%%%%x|DD %%|*D  %"
 , testInterpreter "insertPath 2"  (\x -> insertPath  x cPath2)      $ gridFromString " DGD | DDD |%%%% |DDx%%|*Dxx%"
 , testInterpreter "insertPath 3"  (\x -> insertPath  x [])          $ gridFromString " DGD | DDD |%%%% |DD %%|*D  %"
 , testInterpreter "fnCoord 1" (\_ -> findNeighborCoord (Coord 2 0) North) $ Coord 2 1
 , testInterpreter "fnCoord 2" (\_ -> findNeighborCoord (Coord 2 0) East)  $ Coord 3 0
 , testInterpreter "fnCoord 3" (\_ -> findNeighborCoord (Coord 2 0) West)  $ Coord 1 0
 , testInterpreter "findDirection 1" (\x -> findDirection (Coord 2 0) (Coord 2 1)) $ North
 , testInterpreter "findDirection 2" (\x -> findDirection (Coord 2 0) (Coord 3 0)) $ East
 , testInterpreter "findDirection 3" (\x -> findDirection (Coord 2 0) (Coord 1 0)) $ West
 ]
 where
   cPath1 = [(Coord 4 3), (Coord 4 2)]
   cPath2 = [(Coord 2 1), (Coord 2 0), (Coord 3 0)]

testInterpreter :: (Eq t, Show t) => String -> (PathingGrid -> t) -> t -> API.Test
testInterpreter desc genActual expected = testCase desc assertion
  where
    grid      = gridFromString " DGD | DDD |%%%% |DD %%|*D  %"
    assertion = (genActual grid) @?= expected

a |> f = f a

gridFromString :: String -> PathingGrid
gridFromString str = str |> ((\x -> PathingMapString x "|") >>> fromMapString >>> grid)
