module Main where

import qualified Data.Text.IO as IO
import Test.HUnit

import TypesClasses
import Runs
import Hx
import PPrint
import TestSuite

-- Main function to execute the tests
main :: IO ()
main = do
  _ <- runTestTT testSuite
  putStrLn "Executing Program that Runs interpreter test:"
  mapM_ (print . unRuns) testPrograms
  putStrLn "Executing Haskell Representation test:"
  IO.putStrLn $ unHx $ add (int 3) (neg (int 5))
  putStrLn "Executing PPrint test:"
  IO.putStrLn $ unPPrint $ add (int 2) (neg (int 3))