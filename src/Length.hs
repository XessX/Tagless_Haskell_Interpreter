{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Length where

import TypeClasses


newtype Length a = Length { unLength :: Int }

instance BooleanExp Length where
  bool _ = Length 1
  not_ (Length _) = Length 1
  and_ (Length a) (Length b) = Length (a + b)
  or_ (Length a) (Length b) = Length (a + b)

instance Conditional Length where
    if_ _ (Length t) (Length f) = Length $ 1 + t + f

instance IntegerExp Length where
  int _ = Length 1
  add (Length x) (Length y) = Length $ 1 + x + y
  mult (Length x) (Length y) = Length $ 1 + x + y
  neg (Length x) = Length $ 1 + x
  leq (Length x) (Length y) = Length $ 1 + x + y

instance EqualityExp Length where
  eqInt (Length x) (Length y) = Length $ 1 + x + y

instance LambdaExp Length where
  lam f = Length $ 1 + unLength (f (Length 1))
  app (Length f) (Length x) = Length $ 1 + f + x

instance FixExp Length where
  fix f = Length $ 1 + unLength (f (Length 1))

instance Pairs Length where
  pair (Length a) (Length b) = Length $ 1 + a + b
  first (Length _) = Length 1
  second (Length _) = Length 1
