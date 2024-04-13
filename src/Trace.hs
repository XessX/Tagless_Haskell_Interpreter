{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Trace where

import Data.Text ()
import Control.Monad.Writer hiding (fix)

import TypeClasses


newtype Trace a = Trace { runTrace :: Writer [String] a }
  deriving (Functor, Applicative, Monad)
-- Utility function to log a step and return the value
traceStep :: Show a => String -> a -> Trace a
traceStep desc val = Trace $ do
  tell [desc ++ " => " ++ show val]
  return val

instance BooleanExp Trace where
  bool b = Trace $ tell ["bool " ++ show b] >> return b
  not_ (Trace a) = Trace $ do
    av <- a
    let result = not av
    tell ["not " ++ show av ++ " => " ++ show result]
    return result
  and_ (Trace a) (Trace b) = Trace $ do
    av <- a
    bv <- b
    let result = av && bv
    tell ["and " ++ show av ++ " " ++ show bv ++ " => " ++ show result]
    return result
  or_ (Trace a) (Trace b) = Trace $ do
    av <- a
    bv <- b
    let result = av || bv
    tell ["or " ++ show av ++ " " ++ show bv ++ " => " ++ show result]
    return result
  
instance IntegerExp Trace where
    int n = Trace $ do
      tell ["int " ++ show n]
      return n
  
    add (Trace a) (Trace b) = Trace $ do
      av <- a
      bv <- b
      let result = av + bv
      tell ["add " ++ show av ++ " " ++ show bv ++ " => " ++ show result]
      return result
  
    mult (Trace a) (Trace b) = Trace $ do
      av <- a
      bv <- b
      let result = av * bv
      tell ["mult " ++ show av ++ " " ++ show bv ++ " => " ++ show result]
      return result
  
    neg (Trace a) = Trace $ do
      av <- a
      tell ["neg " ++ show av]
      return (-av)

    leq (Trace a) (Trace b) = Trace $ do
      av <- a
      bv <- b
      let result = av < bv
      tell ["leq " ++ show av ++ " " ++ show bv ++ " => " ++ show result]
      return result

instance Conditional Trace where
  if_ (Trace cond) (Trace thn) (Trace els) = Trace $ do
    condVal <- cond
    if condVal then do
      thnVal <- thn
      tell ["then => " ++ show thnVal]
      return thnVal
    else do
      elsVal <- els
      tell ["else => " ++ show elsVal]
      return elsVal

instance EqualityExp Trace where
  eqInt (Trace a) (Trace b) = Trace $ do
    av <- a
    bv <- b
    let result = av == bv
    tell ["eqInt " ++ show av ++ " " ++ show bv ++ " => " ++ show result]
    return result

instance Pairs Trace where
  pair (Trace a) (Trace b) = Trace $ do
    av <- a
    bv <- b
    tell ["pair (" ++ show av ++ ", " ++ show bv ++ ")"]
    return (av, bv)

  first (Trace ab) = Trace $ do
    abv <- ab
    tell ["first (" ++ show abv ++ ") => " ++ show (fst abv)]
    return (fst abv)

  second (Trace ab) = Trace $ do
    abv <- ab
    tell ["second (" ++ show abv ++ ") => " ++ show (snd abv)]
    return (snd abv)