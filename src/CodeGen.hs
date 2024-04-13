{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module CodeGen where


import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift, Exp, Q)
import qualified Data.Text as T

import TypeClasses

newtype CodeGen a = CodeGen { unCodeGen :: ExpQ }

-- Boolean operations with optimizations for constant expressions
instance BooleanExp CodeGen where
  bool b = CodeGen [| b |]
  not_ (CodeGen b) = CodeGen [| not $(b) |]
  and_ (CodeGen a) (CodeGen b) = CodeGen [| $(a) && $(b) |]
  or_ (CodeGen a) (CodeGen b) = CodeGen [| $(a) || $(b) |]

-- Integer operations with peephole optimizations
instance IntegerExp CodeGen where
  int n = CodeGen [| n |]
  add (CodeGen x) (CodeGen y) = CodeGen $ do
    xExp <- x
    yExp <- y
    -- Optimizing addition with zero
    case (xExp, yExp) of
      (LitE (IntegerL 0), _) -> return yExp
      (_, LitE (IntegerL 0)) -> return xExp
      _ -> [| $(return xExp) + $(return yExp) |]
  mult (CodeGen x) (CodeGen y) = CodeGen $ do
    xExp <- x
    yExp <- y
    -- Multiplication optimizations
    case (xExp, yExp) of
      (LitE (IntegerL 1), _) -> return yExp
      (_, LitE (IntegerL 1)) -> return xExp
      (LitE (IntegerL 0), _) -> [| 0 |]
      (_, LitE (IntegerL 0)) -> [| 0 |]
      _ -> [| $(return xExp) * $(return yExp) |]
  neg (CodeGen x) = CodeGen [| negate $(x) |]
  leq (CodeGen x) (CodeGen y) = CodeGen [| $(x) < $(y) |]

instance EqualityExp CodeGen where
    eqInt (CodeGen x) (CodeGen y) = CodeGen [| $(x) == $(y) |]

-- Conditional with lazy evaluation
instance Conditional CodeGen where
  if_ (CodeGen cond) (CodeGen then_) (CodeGen else_) =
    CodeGen [| if $(cond) then $(then_) else $(else_) |]

-- Lambda and Fixpoint expressions leveraging Haskell's lazy evaluation and higher-order functions
instance LambdaExp CodeGen where
  lam f = CodeGen $ do
    argName <- newName "x"
    bodyExp <- unCodeGen (f (CodeGen (varE argName)))
    return $ LamE [VarP argName] bodyExp
  app (CodeGen f) (CodeGen x) = CodeGen [| $(f) $(x) |]

instance FixExp CodeGen where
  fix f = CodeGen $ do
    funName <- newName "fixFun"
    bodyExp <- unCodeGen (f (CodeGen (varE funName)))
    let fixExp = LamE [VarP funName] (AppE (VarE funName) bodyExp)
    return $ AppE (VarE 'fix) fixExp

-- Pair handling with code generation
instance Pairs CodeGen where
  pair (CodeGen a) (CodeGen b) = CodeGen [| ($(a), $(b)) |]
  first (CodeGen ab) = CodeGen [| fst $(ab) |]
  second (CodeGen ab) = CodeGen [| snd $(ab) |]