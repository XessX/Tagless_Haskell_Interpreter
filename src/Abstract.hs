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
