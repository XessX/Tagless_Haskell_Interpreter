{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Eval where

import TypeClasses

newtype Eval a = Eval { unEval :: a }

instance BooleanExp Eval where
  bool b = Eval b
  not_ (Eval b) = Eval (not b)
  and_ (Eval a) (Eval b) = Eval (a && b)
  or_ (Eval a) (Eval b) = Eval (a || b)

instance Conditional Eval where
  if_ (Eval cond) (Eval thenExpr) (Eval elseExpr) = Eval $ if cond then thenExpr else elseExpr

instance IntegerExp Eval where
  int = Eval
  add (Eval x) (Eval y) = Eval $ x + y
  mult (Eval x) (Eval y) = Eval $ x * y
  neg (Eval x) = Eval $ -x
  leq (Eval x) (Eval y) = Eval $ x < y

instance EqualityExp Eval where
  eqInt (Eval x) (Eval y) = Eval $ x == y

instance LambdaExp Eval where
  lam f = Eval $ \x -> unEval (f (Eval x))
  app (Eval f) (Eval x) = Eval $ f x

instance FixExp Eval where
    fix f = Eval $ fx (unEval . f . Eval) where fx g = g (fx g)

instance Pairs Eval where
  pair (Eval a) (Eval b) = Eval (a, b)
  first (Eval (a, _)) = Eval a
  second (Eval (_, b)) = Eval b
