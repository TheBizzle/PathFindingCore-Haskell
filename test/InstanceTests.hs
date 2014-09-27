module InstanceTests where

import Test.Framework.Providers.API as API
import Test.Framework.Providers.HUnit
import Test.HUnit

import Control.Arrow

import Data.Array.IArray

import PathFindingCore.PathingMap
import PathFindingCore.PathingMap.Interpreter

a |> f = f a

tests = testGroup "Test instances" [
   testPPGInstance "Show PPG 1" " "     expected1
 , testPPGInstance "Show PPG 2" "xG"    expected2
 , testPPGInstance "Show PPG 3" "x|G"   expected3
 , testPPGInstance "Show PPG 4" "x | G" expected4
 , testPPGInstance "Show PPG 5" actual5 expected5
 ]
  where
    actual5   = "   D D|\
                \ x    |\
                \  DDDD|\
                \D DG D|\
                \D DDDD"
    expected1 = "+-+\n\
                \| |\n\
                \+-+"
    expected2 = "+--+\n\
                \|xG|\n\
                \+--+"
    expected3 = "+-+\n\
                \|x|\n\
                \|G|\n\
                \+-+"
    expected4 = "+--+\n\
                \|x |\n\
                \| G|\n\
                \+--+"
    expected5 = "+------+\n\
                \|   D D|\n\
                \| x    |\n\
                \|  DDDD|\n\
                \|D DG D|\n\
                \|D DDDD|\n\
                \+------+"

testPPGInstance :: String -> String -> String -> API.Test
testPPGInstance desc strGrid expected = testCase desc assertion
  where
    actual    = strGrid |> ((flip PathingMapString "|") >>> fromMapString >>> grid >>> PPG >>> show)
    assertion = actual @?= expected
