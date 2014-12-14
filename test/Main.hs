module Main where

import qualified BreadcrumbTests  as Breadcrumb
import qualified InstanceTests    as Instances
import qualified InterpreterTests as IT
import qualified PathingMapTests  as PMT

import Test.Tasty(defaultMain, testGroup)

import Data.Foldable(fold)

main = defaultMain $ testGroup "Pathfinding Tests" [IT.tests, PMT.tests, Breadcrumb.tests, Instances.tests]
