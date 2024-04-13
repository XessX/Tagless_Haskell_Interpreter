{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TypeClasses where


class BooleanExp repr where
  bool :: Bool -> repr Bool
  not_ :: repr Bool -> repr Bool
  and_ :: repr Bool -> repr Bool -> repr Bool
  or_ :: repr Bool -> repr Bool -> repr Bool  -- Assuming you might need this too


class Conditional repr where
    if_ :: Show a => repr Bool -> repr a -> repr a -> repr a

class IntegerExp repr where
  int :: Int -> repr Int
  add :: repr Int -> repr Int -> repr Int
  mult :: repr Int -> repr Int -> repr Int
  neg :: repr Int -> repr Int
  leq :: repr Int -> repr Int -> repr Bool
  sub :: IntegerExp repr => repr Int -> repr Int -> repr Int
  sub x y = add x (neg y)
  
class EqualityExp repr where
  eqInt :: repr Int -> repr Int -> repr Bool

class LambdaExp repr where
  lam :: (repr a -> repr b) -> repr (a -> b)
  app :: repr (a -> b) -> repr a -> repr b

class FixExp repr where
  fix :: (repr a -> repr a) -> repr a

class Pairs repr where
    pair :: (Show a, Show b) => repr a -> repr b -> repr (a, b)
    first :: (Show a, Show b) => repr (a, b) -> repr a
    second :: (Show a, Show b) => repr (a, b) -> repr b