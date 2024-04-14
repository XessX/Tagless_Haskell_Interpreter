{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Language.Haskell.TH
import Test.HUnit
import qualified Data.Text as T
import Control.Monad (void)
import Control.Monad.Writer hiding (fix)

import TypeClasses
import Eval
import Pretty
import Trace
import Partial
import Abstract
import TestSuite


-- Main function to execute the tests
main :: IO ()
main = do

    putStrLn "\nTestCases 1:\n"
    results <- runTestTT tests
    print results
    putStrLn "\nTestCases 2:\n"
    -- void $ runTestTT $ TestList [boolTest, intTest, conditionalTest, lambdaAndFixTest, complexConditionalTest]
    results2 <- runTestTT $ TestList [boolTest, intTest, conditionalTest, lambdaAndFixTest, complexConditionalTest, codegenTestCases, prettyPrintTests]
    print results2

    putStrLn "\n--- Pairs Expression Demonstrations ---"

    -- Demonstrations
    putStrLn $ "\nThe first element of myPair is: " ++ show (unEval myfirst)
    putStrLn $ "\nThe second element of myPair is: " ++ show (unEval mysecond)

    putStrLn "\n--- Boolean Expression Demonstrations ---"
    putStrLn "\nEval bool True:"
    print $ unEval $ bool True
    putStrLn "\nPretty bool False:"
    putStrLn $ T.unpack $ unPretty $ bool False

    putStrLn "\n--- Integer Expression Demonstrations ---"
    putStrLn "\nEval add 1 + 2:"
    print $ unEval $ add (int 1) (int 2)
    putStrLn "\nPretty mult 3 * 4:"
    putStrLn $ T.unpack $ unPretty $ mult (int 3) (int 4)

    putStrLn "\n--- Conditional Expression Demonstrations ---"
    putStrLn "\nEval if True then 1 else 2:"
    print $ unEval $ if_ (bool True) (int 1) (int 2)
    putStrLn "\nPretty if False then 3 else 4:"
    putStrLn $ T.unpack $ unPretty $ if_ (bool False) (int 3) (int 4)

    putStrLn "\n--- Lambda Demonstrations ---"
    putStrLn . T.unpack . unPretty $ prettyLambda 0


  -- Recursion Examples: Factorial and Fibonacci
    putStrLn "\n--- Recursion Examples ---"
    putStrLn "\nFactorial of 5 (Eval):"
    print $ evalFactorial 5
    putStrLn "\nFibonacci of 10 (Eval):"
    print $ evalFibonacci 10

    putStrLn "\nPretty print Factorial (5):\n"
    putStrLn $ T.unpack $ prettySumToN 5  -- Assuming prettySumToN demonstrates the factorial function for brevity

    putStrLn "\nCodeGen demonstration of sumToN (5):\n"
    codeAsString <- runQ $ pprint <$> genHaskell (app sumToN (int 5))
    putStrLn codeAsString

    putStrLn "\n-----Trace Eval:-----\n"

    evaluateWithTrace exampleExpr

    -- Running existing demonstrations
    putStrLn "\n--- Existing Demonstrations ---\n"

    -- New demonstrations for pairs
    putStrLn "\n--- Pair Demonstrations ---\n"
    putStrLn "Evaluating examplePairCreation with tracing:\n"
    evaluateWithTrace examplePairCreation
    putStrLn "\nEvaluating examplePairFirst with tracing:\n"
    evaluateWithTrace examplePairFirst
    putStrLn "\nEvaluating examplePairSecond with tracing:\n"
    evaluateWithTrace examplePairSecond

    -- Running the test suite, now including pair tests
    putStrLn "\nRunning the test suite, including pair tests:"
    _ <- runTestTT pairTests
    return ()

    putStrLn "\nEvaluating examplePairExpr:\n"
    evaluateWithTrace examplePairExpr
    putStrLn "\nEvaluating exampleFirstExpr:\n"
    evaluateWithTrace exampleFirstExpr
    putStrLn "\nEvaluating exampleSecondExpr:\n"
    evaluateWithTrace exampleSecondExpr

    codegenTests

    putStrLn "\nPretty print demonstration for complex expressions:"

    putStrLn "\n--- Conditional Example ---"
    putStrLn $ T.unpack $ unPretty $ if_ (bool True) (add (int 1) (int 2)) (mult (int 3) (int 4))

    putStrLn "\n--- Lambda Example ---"
    let lambdaExpr = lam $ \x -> add x (int 1)
    putStrLn $ T.unpack $ unPretty $ app lambdaExpr (int 10)

    putStrLn "\n--- Fix Example ---"
    putStrLn $ T.unpack $ unPretty $ fix $ \f -> lam $ \n -> if_ (leq n (int 1)) n (add (app f (sub n (int 1))) (int 1))


    let runTraceTest test = mapM_ putStrLn . snd . runWriter $ runTrace test
    putStrLn "\nTesting Pairs:"
    runTraceTest testPairs
    putStrLn "\nTesting First Projection:"
    runTraceTest testFirst
    putStrLn "\nTesting Second Projection:"
    runTraceTest testSecond
    putStrLn "\nTesting Equality:"
    runTraceTest testEquality
    putStrLn "\nTesting Conditional:"
    runTraceTest testConditional

    putStrLn "\nTesting Partial Eval, generated using Trace:\n"
    runAndPrintTrace $ toTrace testExpression


    putStrLn "\nTest Abstract Interpreter:\n"
    print $ getAbstract testAbstract


    -- Basic operations
    printTest "\nPositive + Negative" (add (int 5) (int (-3)))
    printTest "\nNegative + Negative" (add (int (-5)) (int (-3)))
    printTest "\nPositive + Positive" (add (int 5) (int 3))
    printTest "\nNegative + Zero" (add (int (-5)) (int 0))
    printTest "\nZero + Zero" (add (int 0) (int 0))

    -- Multiplication tests
    printTest "\nPositive * Negative" (mult (int 5) (int (-3)))
    printTest "\nNegative * Negative" (mult (int (-5)) (int (-3)))
    printTest "\nPositive * Positive" (mult (int 5) (int 3))
    printTest "\nNegative * Zero" (mult (int (-5)) (int 0))
    printTest "\nZero * Zero" (mult (int 0) (int 0))

    -- Negation tests
    printTest "\nNegate Positive" (neg (int 5))
    printTest "\nNegate Negative" (neg (int (-5)))
    printTest "\nNegate Zero" (neg (int 0))

    -- Complex expressions
    printTest "\nComplex Expression 1" (add (mult (int 5) (int (-1))) (int 2))
    printTest "\nComplex Expression 2" (mult (add (int 5) (int 3)) (neg (int 2)))

    -- Testing abstract comparison
    printTest "\nLess or Equal Abstract" (leq (int 5) (int 3))

    putStrLn "\nEdge Tests Abstract Interpreter:\n"

    printTest "\nMultiply Positive by Zero" (mult (int 5) (int 0))
    printTest "\nMultiply Negative by Zero" (mult (int (-5)) (int 0))
    printTest "\nAdd Zero to Zero" (add (int 0) (int 0))
    printTest "\nNegate Zero" (neg (int 0))

    -- Identity and Absorbing Elements
    printTest "\nAdd Zero to Positive" (add (int 0) (int 5))
    printTest "\nAdd Zero to Negative" (add (int 0) (int (-5)))
    printTest "\nMultiply One by Negative" (mult (int 1) (int (-5)))
    printTest "\nMultiply One by Positive" (mult (int 1) (int 5))

    -- Overflow Scenarios (Abstract Interpretation)
    printTest "\nAdd Positive Overflow" (add (int maxBound) (int 1))
    printTest "\nAdd Negative Overflow" (add (int minBound) (int (-1)))
    printTest "\nMultiply Overflow" (mult (int maxBound) (int 2))

    -- Sign Reversal
    printTest "\nNegate Negative" (neg (int (-10)))
    printTest "\nSubtract from Zero" (sub (int 0) (int 10))
    printTest "\nSubtract Leading to Negative" (sub (int 5) (int 10))

    -- Nested Combinations
    printTest "\nNested Operations 1" (add (mult (int 2) (neg (int 5))) (int 10))
    printTest "\nNested Operations 2" (mult (add (int 3) (int 7)) (sub (int 0) (int 2)))

    putStrLn "\nRunning Abstract Tests:\n"
    abstractResults <- runTestTT abstractTests
    print abstractResults

    putStrLn "\nRunning Pair Tests for Partial Type:\n"
    pairPartialResults <- runTestTT pairPartialTest
    print pairPartialResults

    -- Additional demonstration if needed
    putStrLn "\n--- Pair Dynamic Demonstrations ---\n"
    runAndPrintTrace $ toTrace $ pair (toDynamic $ traceStep "int" 5) (toDynamic $ traceStep "bool" False)
    runAndPrintTrace $ toTrace $ first $ pair (toDynamic $ traceStep "int" 5) (toDynamic $ traceStep "bool" False)
    runAndPrintTrace $ toTrace $ second $ pair (toDynamic $ traceStep "int" 5) (toDynamic $ traceStep "bool" False)