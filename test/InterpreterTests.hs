module InterpreterTests where

import Test.Framework.Providers.API as API
import Test.Framework.Providers.HUnit
import Test.HUnit

import Control.Arrow

import Data.Array.IArray

import PathFindingCore.PathingMap.Coordinate
import PathFindingCore.PathingMap.Interpreter
import PathFindingCore.PathingMap.Terrain

tests = testGroup "Test interpreter" [
   testInterpreter "Simple grid"          "*G"        (0, 0) (1, 0) (1, 0) [Self,  Goal]
 , testInterpreter "One-line grid 1"      "*      G"  (0, 0) (7, 0) (7, 0) [Self,  Empty, Empty, Empty, Empty, Empty, Empty, Goal]
 , testInterpreter "One-line grid 2"      "G      *"  (7, 0) (0, 0) (7, 0) [Goal,  Empty, Empty, Empty, Empty, Empty, Empty, Self]
 , testInterpreter "One-line grid 3"      "G %D%  *"  (7, 0) (0, 0) (7, 0) [Goal,  Empty, Water, Wall,  Water, Empty, Empty, Self]
 , testInterpreter "Simple vertical grid" "*|G"       (0, 1) (0, 0) (0, 1) [Goal,  Self]
 , testInterpreter "One-line vert grid 1" "*| | | |G" (0, 4) (0, 0) (0, 4) [Goal,  Empty, Empty, Empty, Self]
 , testInterpreter "One-line vert grid 2" "G| | | |*" (0, 0) (0, 4) (0, 4) [Self,  Empty, Empty, Empty, Goal]
 , testInterpreter "One-line vert grid 3" "G| |%|D|*" (0, 0) (0, 4) (0, 4) [Self,  Wall,  Water, Empty, Goal]
 , testInterpreter "2x2 grid 1"           "G | *"     (1, 0) (0, 1) (1, 1) [Empty, Goal,  Self,  Empty]
 , testInterpreter "2x2 grid 2"           "G*|  "     (1, 1) (0, 1) (1, 1) [Empty, Goal,  Empty, Self]
 , testInterpreter "2x2 grid 3"           "G*|DD"     (1, 1) (0, 1) (1, 1) [Wall,  Goal,  Wall,  Self]
 , testInterpreter "2x2 grid 4"           "DD|*G"     (0, 0) (1, 0) (1, 1) [Self,  Wall,  Goal,  Wall]
 , testInterpreter "5x5 grid 1"           m5x5p1      (3, 1) (1, 3) (4, 4) [Wall,  Wall,  Wall,  Wall,  Wall,  Wall, Empty, Empty, Goal, Wall, Wall,  Empty, Empty, Empty, Wall, Wall,  Self,  Empty, Empty, Wall, Wall,  Wall,  Wall,  Wall,  Wall]
 , testInterpreter "5x5 grid 2"           m5x5p2      (0, 0) (2, 4) (4, 4) [Self,  Wall,  Water, Empty, Empty, Wall, Wall,  Water, Wall, Wall, Empty, Empty, Water, Wall,  Goal, Empty, Water, Water, Wall,  Wall, Water, Water, Empty, Empty, Empty]
 ]
 where
   m5x5p1 = "DDDDD|DG  D|D   D|D  *D|DDDDD"
   m5x5p2 = " DGD | DDD |%%%% |DD %%|*D  %"

testInterpreter :: String -> String -> (Int, Int) -> (Int, Int) -> (Int, Int) -> [Terrain] -> API.Test
testInterpreter desc strGrid (x1, y1) (x2, y2) upperBound arr = testCase desc assertion
  where
    pmStr     = PathingMapString strGrid "|"
    grid      = listArray ((0, 0), upperBound) arr
    pmData    = PathingMapData (Coord x1 y1) (Coord x2 y2) grid
    assertion = (fromMapString pmStr) @?= pmData
