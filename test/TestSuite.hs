{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TestSuite where


import Language.Haskell.TH
import Test.HUnit
import Data.Text ()
import qualified Data.Text as T
import Control.Monad (void)
import Control.Monad.Writer hiding (fix)

import TypeClasses
import Eval
import CodeGen
import Pretty
import Trace
import Partial
import Abstract


-- Helper function to print test results
printTest :: (Show a) => String -> Abstract a -> IO ()
printTest desc abstract = putStrLn $ desc ++ ": " ++ show (getAbstract abstract)

testAbstract :: Abstract Int
testAbstract = add (int 5) (neg (int (-10)))


-- Assuming a dynamic value for demonstration
dynamicValue :: Partial Int
dynamicValue = Dynamic (traceStep "unknown dynamic value" 10)

-- An expression that uses dynamic values
exampleExpression :: Partial Int
exampleExpression = add (int 5) dynamicValue

-- Evaluating and tracing the example expression
evaluateTrace :: Trace Int
evaluateTrace = toTrace exampleExpression


-- Function to run and print the generated Haskell code from CodeGen
runAndPrintCodeGen :: CodeGen a -> IO ()
runAndPrintCodeGen codeGen = do
    generatedCode <- runQ $ pprint <$> unCodeGen codeGen
    putStrLn generatedCode


-- Define test cases for the CodeGen interpreter
codegenTests :: IO ()
codegenTests = do
    
    putStrLn "\n--- CodeGen Tests ---\n"

    putStrLn "1. CodeGen for Integer Addition:"
    runAndPrintCodeGen $ add (int 5) (int 10)

    putStrLn "\n2. CodeGen for Multiplication with Optimization:"
    runAndPrintCodeGen $ mult (int 1) (int 10)

    putStrLn "\n3. CodeGen for Conditional Expression:"
    runAndPrintCodeGen $ if_ (bool True) (int 1) (int 0)

    putStrLn "\n4. CodeGen for Lambda Expression:"
    runAndPrintCodeGen $ app (lam $ \x -> add x (int 1)) (int 5)

    putStrLn "\n5. CodeGen for Fixpoint (Factorial Function):"
    let factorialCodeGen = fix $ \f -> lam $ \n -> 
            if_ (eqInt n (int 0)) 
                (int 1) 
                (mult n (app f (sub n (int 1))))
    runAndPrintCodeGen $ app factorialCodeGen (int 5)


-- Utility function to generate Haskell code from a CodeGen expression and compare to expected output
codegenTestCase :: CodeGen a -> String -> Test
codegenTestCase codeGenExpr expectedOutput = TestCase $ do
  generatedCode <- runQ $ pprint <$> unCodeGen codeGenExpr
  let normalizedGenerated = filter (`notElem` (" \n\t" :: String)) generatedCode
      normalizedExpected = filter (`notElem` (" \n\t" :: String)) expectedOutput
  assertEqual "CodeGen Output" normalizedExpected normalizedGenerated

-- Define test cases
codegenTestCases :: Test
codegenTestCases = TestList
  [ codegenTestCase (add (int 5) (int 10)) "5 GHC.Num.+ 10"
  , codegenTestCase (mult (int 1) (int 10)) "10"
  , codegenTestCase (if_ (bool True) (int 1) (int 0)) "if GHC.Types.True then 1 else 0"
  , codegenTestCase (app (lam (\x -> add x (int 1))) (int 5)) "(\\ x_0 -> x_0 GHC.Num.+ 1) 5"
  , codegenTestCase (app factorial (int 5))
      "Main.fix (\\fixFun_0 -> fixFun_0 (\\x_1 -> if x_1 GHC.Classes.== 0 then 1 else x_1 GHC.Num.* fixFun_0 (x_1 GHC.Num.+ GHC.Num.negate 1))) 5"
  ]

-- This function runs a single test case for pretty printing
prettyPrintTest :: Pretty a -> String -> Test
prettyPrintTest expr expected = TestCase $ assertEqual "Pretty print test" expected (T.unpack $ unPretty expr)

-- This constructs a list of test cases
prettyPrintTests :: Test
prettyPrintTests = TestList [
  prettyPrintTest (bool True) "True",
  prettyPrintTest (not_ (bool False)) "not (False)",
  prettyPrintTest (and_ (bool True) (bool False)) "(True && False)",
  prettyPrintTest (if_ (bool True) (int 1) (int 0)) "if True\nthen 1\nelse 0",
  prettyPrintTest (add (int 5) (int 10)) "(5 + 10)",
  prettyPrintTest (mult (int 2) (int 3)) "(2 * 3)",
  prettyPrintTest (app (lam (\x -> add x (int 1))) (int 5)) "(\\x -> (x + 1)) (5)",
  prettyPrintTest (fix (\f -> lam (\n -> if_ (eqInt n (int 0)) (int 1) (mult n (app f (sub n (int 1))))))) "fix (\\f -> \\n -> if (n == 0) then 1 else n * f (n - 1))"
    ]


-- Runs the trace computation and prints the steps
runAndPrintTrace :: Show a => Trace a -> IO ()
runAndPrintTrace trace = do
    let (result, log) = runWriter (runTrace trace)
    mapM_ putStrLn log
    putStrLn ("Result: " ++ show result)


testExpression :: Partial Int
testExpression = add (int 5) (mult (int 2) (int 3))



myPair :: (Pairs repr, IntegerExp repr) => repr (Int, Int)
myPair = pair (int 3) (int 5)

myfirst :: (Pairs repr, IntegerExp repr) => repr Int
myfirst = first myPair

mysecond :: (Pairs repr, IntegerExp repr) => repr Int
mysecond = second myPair


myComparison :: (IntegerExp repr) => repr Bool
myComparison = leq (int 1) (int 2)

factorial :: (LambdaExp repr, FixExp repr, IntegerExp repr, Conditional repr, EqualityExp repr) => repr (Int -> Int)
factorial = fix $ \f -> lam $ \n ->
  if_ (eqInt n (int 0))
    (int 1)
    (mult n (app f (sub n (int 1))))

fibonacci :: (LambdaExp repr, FixExp repr, IntegerExp repr, Conditional repr, EqualityExp repr) => repr (Int -> Int)
fibonacci = fix $ \f -> lam $ \n ->
  if_ (eqInt n (int 0))
    (int 0)
    (if_ (eqInt n (int 1))
      (int 1)
      (add (app f (sub n (int 1))) (app f (sub n (int 2)))))

sumToN :: (FixExp repr, LambdaExp repr, IntegerExp repr, Conditional repr, EqualityExp repr) => repr (Int -> Int)
sumToN = fix $ \f -> lam $ \n ->
  if_ (eqInt n (int 0))
    (int 0)
    (add n (app f (sub n (int 1))))


evalFactorial :: Int -> Int
evalFactorial n = unEval $ app factorial (int n)

evalFibonacci :: Int -> Int
evalFibonacci n = unEval $ app fibonacci (int n)

evalSumToN :: Int -> Int
evalSumToN n = unEval $ app sumToN (int n)

-- Function to generate Haskell source code from the DSL's expression
genHaskell :: CodeGen a -> Q Exp
genHaskell = unCodeGen

prettySumToN :: Int -> T.Text
prettySumToN n = unPretty $ app sumToN (int n)


-- Define test cases
pairFirstTest :: Test
pairFirstTest = TestCase (assertEqual "first element of myPair" 3 (unEval myfirst))

pairSecondTest :: Test
pairSecondTest = TestCase (assertEqual "second element of myPair" 5 (unEval mysecond))

factorialTest :: Test
factorialTest = TestCase (assertEqual "factorial of 5" 120 (evalFactorial 5))

fibonacciTest :: Test
fibonacciTest = TestCase (assertEqual "fibonacci of 10" 55 (evalFibonacci 10))

sumToNTest :: Test
sumToNTest = TestCase (assertEqual "sum to 5" 15 (evalSumToN 5))

-- Aggregate all test cases
tests :: Test
tests = TestList [pairFirstTest, pairSecondTest, factorialTest, fibonacciTest, sumToNTest]



boolTest :: Test
boolTest = TestList [
  TestCase $ assertEqual "eval bool True" True (unEval $ bool True),
  TestCase $ assertEqual "pretty bool False" "False" (T.unpack $ unPretty $ bool False)
  ]

intTest :: Test
intTest = TestList [
  TestCase $ assertEqual "eval add 1+2" 3 (unEval $ add (int 1) (int 2)),
  TestCase $ assertEqual "pretty mult 3*4" "(3 * 4)" (T.unpack $ unPretty $ mult (int 3) (int 4))
  ]

conditionalTest :: Test
conditionalTest = TestCase $ assertEqual "eval if_ True 1 2" 1 (unEval $ if_ (bool True) (int 1) (int 2))

lambdaAndFixTest :: Test
lambdaAndFixTest = TestList [
  TestCase $ assertEqual "eval factorial 5" 120 (evalFactorial 5),
  TestCase $ assertEqual "eval fibonacci 10" 55 (evalFibonacci 10)
  ]

  

evaluateWithTrace :: Trace a -> IO ()
evaluateWithTrace (Trace action) = mapM_ putStrLn $ snd $ runWriter action

exampleExpr :: Trace Int
exampleExpr = add (int 1) (int 2)


-- Pair creation and manipulation expressions
examplePairCreation :: Trace (Int, Bool)
examplePairCreation = pair (int 42) (bool True)

examplePairFirst :: Trace Int
examplePairFirst = first examplePairCreation

examplePairSecond :: Trace Bool
examplePairSecond = second examplePairCreation

pairTests :: Test
pairTests = TestList [
    TestCase $ assertEqual "pair creation" ((42, True), ["int 42","bool True","pair (42, True)"]) (runWriter $ runTrace examplePairCreation),
    TestCase $ assertEqual "pair first" (42, ["int 42","bool True","pair (42, True)","first ((42,True)) => 42"]) (runWriter $ runTrace examplePairFirst),
    TestCase $ assertEqual "pair second" (True, ["int 42","bool True","pair (42, True)","second ((42,True)) => True"]) (runWriter $ runTrace examplePairSecond)
  ]

examplePairExpr :: Trace (Int, Int)
examplePairExpr = pair (int 1) (int 2)

exampleFirstExpr :: Trace Int
exampleFirstExpr = first $ pair (int 1) (int 2)

exampleSecondExpr :: Trace Int
exampleSecondExpr = second $ pair (int 3) (int 4)

testPairs :: Trace (Int, Int)
testPairs = pair (int 5) (int 10)

testFirst :: Trace Int
testFirst = first testPairs

testSecond :: Trace Int
testSecond = second testPairs

testEquality :: Trace Bool
testEquality = eqInt (int 5) (int 5)

testConditional :: Trace Int
testConditional = if_ (eqInt (int 5) (int 10)) (int 1) (int 2)


complexConditionalTest :: Test
complexConditionalTest = TestCase $ assertEqual "complex conditional" expectedValue (evalComplexExpr)
  where
    evalComplexExpr = unEval $ if_ (leq (int 10) (int 20)) (add (int 1) (int 2)) (mult (int 3) (int 4))
    expectedValue = 3  -- (1 + 2) because 10 <= 20

-- Convert a Trace computation into a testable form
runTraceTest :: Trace a -> ((a, [String]))
runTraceTest = runWriter . runTrace

pairPartialTest :: Test
pairPartialTest = TestList [
    TestCase $ assertEqual "pair creation dynamic" expectedDynamic (runTraceTest $ toTrace $ pair (toDynamic $ traceStep "int" 42) (toDynamic $ traceStep "bool" True)),
    TestCase $ assertEqual "first of pair dynamic" 42 (fst $ runTraceTest $ toTrace $ first $ pair (toDynamic $ traceStep "int" 42) (toDynamic $ traceStep "bool" True)),
    TestCase $ assertEqual "second of pair dynamic" True (fst $ runTraceTest $ toTrace $ second $ pair (toDynamic $ traceStep "int" 42) (toDynamic $ traceStep "bool" True))
  ]
  where
    expectedDynamic = ((42, True), ["int => 42", "bool => True", "pair => (42,True)"])  -- Adjusted expected trace text

