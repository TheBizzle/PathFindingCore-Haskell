module Main where

import InterpreterTests as IT
import PathingMapTests  as PMT
import Test.Framework.Runners.Console (defaultMain)

main = defaultMain $ [IT.tests, PMT.tests]
