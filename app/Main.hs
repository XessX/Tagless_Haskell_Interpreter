module Main where

import qualified Data.Text.IO as IO
import Test.HUnit

import TypesClasses
import R
import HxR
import PPrint
import TestSuite

-- Main function to execute the tests
main :: IO ()
main = do
  _ <- runTestTT testSuite
  putStrLn "Executing R interpreter test:"
  mapM_ (print . unR) testPrograms
  putStrLn "Executing Haskell Representation test:"
  IO.putStrLn $ unHxR $ add (int 3) (neg (int 5))
  putStrLn "Executing PPrint test:"
  IO.putStrLn $ unPPrint $ add (int 2) (neg (int 3))