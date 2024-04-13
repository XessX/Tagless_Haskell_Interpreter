{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Partial where

import TypeClasses
import Trace

data Partial a = Known a | Dynamic (Trace a)

-- Convert a Partial to a Trace computation, handling both known and dynamic cases
toTrace :: Partial a -> Trace a
toTrace (Known a) = return a
toTrace (Dynamic trace) = trace

-- Lift a value directly into a Partial as a known value
toKnown :: a -> Partial a
toKnown = Known

-- Lift a Trace computation into a Partial as a dynamic computation
toDynamic :: Trace a -> Partial a
toDynamic = Dynamic

instance BooleanExp Partial where
  bool = toKnown

  not_ (Known b) = toKnown (not b)
  not_ (Dynamic traceB) = Dynamic $ do
    b <- traceB
    traceStep "not" (not b)

  and_ (Known a) (Known b) = toKnown (a && b)
  and_ a b = Dynamic $ do
    aValue <- toTrace a
    bValue <- toTrace b
    traceStep "and" (aValue && bValue)

  or_ (Known a) (Known b) = toKnown (a || b)
  or_ a b = Dynamic $ do
    aValue <- toTrace a
    bValue <- toTrace b
    traceStep "or" (aValue || bValue)

instance IntegerExp Partial where
  int = toKnown

  add a b = Dynamic $ do
    aValue <- toTrace a
    bValue <- toTrace b
    traceStep "add" (aValue + bValue)

  mult a b = Dynamic $ do
    aValue <- toTrace a
    bValue <- toTrace b
    traceStep "mult" (aValue * bValue)

  neg a = Dynamic $ do
    aValue <- toTrace a
    traceStep "neg" (-aValue)

  leq a b = Dynamic $ do
    aValue <- toTrace a
    bValue <- toTrace b
    traceStep "leq" (aValue < bValue)

instance Conditional Partial where
  if_ (Known cond) thn els = if cond then thn else els
  if_ cond thn els = Dynamic $ do
    condVal <- toTrace cond
    if condVal then toTrace thn else toTrace els

instance Pairs Partial where
    pair (Known a) (Known b) = Known (a, b)
    pair a b = Dynamic $ do
        aValue <- toTrace a
        bValue <- toTrace b
        traceStep "pair" (aValue, bValue)

    first (Known (a, _)) = Known a
    first ab = Dynamic $ do
        abValue <- toTrace ab
        traceStep "first" (fst abValue)

    second (Known (_, b)) = Known b
    second ab = Dynamic $ do
        abValue <- toTrace ab
        traceStep "second" (snd abValue)
