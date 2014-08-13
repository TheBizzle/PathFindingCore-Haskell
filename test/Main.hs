module Main where

import InterpreterTests as IT
import Test.Framework.Runners.Console (defaultMain)

main = defaultMain $ [IT.tests]
