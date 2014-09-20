module Main where

import qualified BreadcrumbTests  as Breadcrumb
import qualified InterpreterTests as IT
import qualified PathingMapTests  as PMT

import Test.Framework.Runners.Console (defaultMain)

main = defaultMain $ [IT.tests, PMT.tests, Breadcrumb.tests]
