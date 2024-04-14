{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Abstract where

import TypeClasses

data Sign = Positive | Negative | Zero | Unknown deriving (Show, Eq)

-- Merge two signs into a more general sign
mergeSign :: Sign -> Sign -> Sign
mergeSign Unknown _ = Unknown
mergeSign _ Unknown = Unknown
mergeSign Zero Zero = Zero
mergeSign Positive Positive = Positive
mergeSign Negative Negative = Negative
mergeSign _ _ = Unknown


newtype Abstract a = Abstract { getAbstract :: Sign }

instance IntegerExp Abstract where
    int n
        | n > 0     = Abstract Positive
        | n == 0    = Abstract Zero
        | otherwise = Abstract Negative

    add (Abstract a) (Abstract b) = Abstract $ mergeSign a b
    mult (Abstract a) (Abstract b) = Abstract $ mergeSign a b
    neg (Abstract a) = Abstract $ case a of
        Positive -> Negative
        Negative -> Positive
        Zero     -> Zero
        Unknown  -> Unknown

    leq (Abstract a) (Abstract b) = Abstract Unknown -- Simplification for example purposes

instance BooleanExp Abstract where
  bool b = Abstract $ if b then Positive else Zero -- Using Positive for True, Zero for False as a simplification
  not_ (Abstract Positive) = Abstract Zero
  not_ (Abstract Zero) = Abstract Positive
  not_ _ = Abstract Unknown

  and_ (Abstract Positive) (Abstract Positive) = Abstract Positive
  and_ (Abstract Zero) _ = Abstract Zero
  and_ _ (Abstract Zero) = Abstract Zero
  and_ _ _ = Abstract Unknown

  or_ (Abstract Zero) (Abstract Zero) = Abstract Zero
  or_ (Abstract Positive) _ = Abstract Positive
  or_ _ (Abstract Positive) = Abstract Positive
  or_ _ _ = Abstract Unknown

instance Pairs Abstract where
  pair (Abstract a) (Abstract b) = Abstract Unknown -- Simplifying that we cannot know the details of the pair
  first (Abstract _) = Abstract Unknown -- Result of first operation is unknown
  second (Abstract _) = Abstract Unknown -- Result of second operation is unknown
