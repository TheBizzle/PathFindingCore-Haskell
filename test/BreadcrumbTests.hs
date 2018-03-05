module BreadcrumbTests(tests) where

import Test.Tasty(testGroup, TestTree)
import Test.Tasty.HUnit((@?=), testCase)

import PathFindingCore.PathingMap.Coordinate(Breadcrumb(Crumb, Source), breadcrumbsToList, Coordinate(Coord))

tests = testGroup "Test breadcrumbs" [
   testIt "Simple source 1"  (Source $ Coord 0 0)                                         [Coord 0 0]
 , testIt "Simple source 2"  (Source $ Coord 1 1)                                         [Coord 1 1]
 , testIt "Simple source 3"  (Source $ Coord 3 8)                                         [Coord 3 8]
 , testIt "Two-item crumb"   (Crumb (Coord 0 0) $ Source $ Coord 3 8)                     [(Coord 3 8), (Coord 0 0)]
 , testIt "Three-item crumb" (Crumb (Coord 1 7) $ Crumb (Coord 0 0) $ Source $ Coord 3 8) [(Coord 3 8), (Coord 0 0), (Coord 1 7)]
 ]

testIt :: Text -> Breadcrumb -> [Coordinate] -> TestTree
testIt desc crumbs coords = testCase (asString desc) assertion
  where
    assertion = (breadcrumbsToList crumbs) @?= coords
