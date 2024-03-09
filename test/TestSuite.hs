module TestSuite where

import Test.HUnit
import TypesClasses
import Runs

-- Define a test program using the expression type class
testPrograms :: (ExpSYM repr) => [repr Int]
testPrograms = [test0, test1, test2, test3, test4]
  where
    test0 = add (int 2) (neg (int 3))
    test1 = sub (mul (add (int 3) (neg (int 7))) (int 6)) (int 5)
    test2 = if_ (neq (int 3) (int 4)) (int 2) (int 8)
    test3 = app (app (lam (\y -> lam (\x -> mul x y))) (int 8)) (int 4)
    test4 = fst_ (pair (int 6) (int 3))

-- Define a test suite that covers all features and interpreters
testSuite :: Test
testSuite = TestList $ map TestCase
  [ assertEqual "Test 1" (-1) 
     (unRuns $ testPrograms !! 0)
  , assertEqual "Test 2" (-29) 
     (unRuns $ testPrograms !! 1)
  , assertEqual "Test 3" (2) 
     (unRuns $ testPrograms !! 2)
  , assertEqual "Test 4" (32)
     (unRuns $ testPrograms !! 3)
  , assertEqual "Test 5" (6)
     (unRuns $ testPrograms !! 4)
  ]

main :: IO ()
main = do
  _ <- runTestTT testSuite
  putStrLn "All tests completed."