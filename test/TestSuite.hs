module TestSuite where

import Test.HUnit
import TypesClasses
import R

-- Define a test program using the expression type class
testPrograms :: (ExpSYM repr) => [repr Int]
testPrograms = [test1, test2, test3, test4, test5]
  where
    test1 = add (int 1) (neg (int 2))
    test2 = sub (mul (add (int 3) (neg (int 7))) (int 6)) (int 5)
    test3 = if_ (neq (int 3) (int 4)) (int 2) (int 8)
    test4 = app (app (lam (\y -> lam (\x -> mul x y))) (int 8)) (int 4)
    test5 = fst_ (pair (int 6) (int 3))

-- Define a test suite that covers all features and interpreters
testSuite :: Test
testSuite = TestList $ map TestCase
  [ assertEqual "Test 1" (-1) 
     (unR $ testPrograms !! 0)
  , assertEqual "Test 2" (-29) 
     (unR $ testPrograms !! 1)
  , assertEqual "Test 3" (2) 
     (unR $ testPrograms !! 2)
  , assertEqual "Test 4" (32)
     (unR $ testPrograms !! 3)
  , assertEqual "Test 5" (6)
     (unR $ testPrograms !! 4)
  ]

main :: IO ()
main = do
  _ <- runTestTT testSuite
  putStrLn "All tests completed."