{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Pretty where

import qualified Data.Text as T
import qualified Control.Monad.State as CMS
-- Import the ExpSYM class from your types and classes module
import TypeClasses

newtype Pretty a = Pretty { unPretty :: T.Text }

parenthesize :: T.Text -> T.Text
parenthesize text = T.concat [T.pack "(", text, T.pack ")"]

-- Helper function for indentation
indent :: Int -> T.Text -> T.Text
indent level text = T.unlines $ map (T.replicate level " " <>) (T.lines text)

-- Helper function to increase readability by wrapping expressions
wrap :: T.Text -> T.Text
wrap text = "(" <> text <> ")"

instance BooleanExp Pretty where
  bool b = Pretty $ T.pack $ show b
  not_ (Pretty b) = Pretty $ "not (" <> b <> ")"
  and_ (Pretty a) (Pretty b) = Pretty $ "(" <> a <> " && " <> b <> ")"
  or_ (Pretty a) (Pretty b) = Pretty $ "(" <> a <> " || " <> b <> ")"

instance Conditional Pretty where
    if_ (Pretty b) (Pretty t) (Pretty f) = 
        Pretty $ "if " <> b <> "\nthen " <> t <> "\nelse " <> f

instance IntegerExp Pretty where
    int n = Pretty $ T.pack $ show n
    add (Pretty x) (Pretty y) = Pretty $ "(" <> x <> " + " <> y <> ")"
    mult (Pretty x) (Pretty y) = Pretty $ "(" <> x <> " * " <> y <> ")"
    neg (Pretty x) = Pretty $ "(-" <> x <> ")"
    leq (Pretty x) (Pretty y) = Pretty $ "(" <> x <> " < " <> y <> ")"

instance EqualityExp Pretty where
    eqInt (Pretty x) (Pretty y) = Pretty $ wrap $ x <> " == " <> "\n" <> y

-- A simple naming context that counts upwards
type NamingContext = Int

-- Generates a fresh variable name and updates the context
freshName :: NamingContext -> (String, NamingContext)
freshName ctx = ("x" ++ show ctx, ctx + 1)

-- Modified LambdaExp Pretty instance using NamingContext
instance LambdaExp Pretty where
    lam f =
        let (arg, newCtx) = freshName 0  -- Adjust this for NamingState
            body = unPretty $ f (Pretty $ T.pack arg)
        in Pretty $ "(\\" <> T.pack arg <> " -> " <> body <> ""  -- Ensuring the body is wrapped and formatted as in the expected result

    app (Pretty f) (Pretty x) = Pretty $ f <> " (" <> x <> ")"
    
prettyLambda :: NamingContext -> Pretty (Int -> Int)
prettyLambda ctx = lam (\x -> add (int 1) x)


type NamingState = CMS.State Int

-- Generates a fresh variable name based on the provided base name and increments the state
freshNameState :: String -> NamingState String
freshNameState base = do
    n <- CMS.get
    CMS.put (n + 1)
    return (base ++ show n)

-- Example usage of CMS.State in FixExp instance
instance FixExp Pretty where
    fix f = Pretty $ CMS.evalState (prettyFix f) 0

prettyFix :: (Pretty a -> Pretty a) -> NamingState T.Text
prettyFix f = do
    recVar <- freshNameState "f"
    let body = unPretty $ f (Pretty $ T.pack recVar)
    return $ "fix \n" <> T.pack recVar <> " -> \n" <> indent 2 body


instance Pairs Pretty where
  pair (Pretty a) (Pretty b) = Pretty $ T.concat ["(", a, ", ", b, ")"]
  first (Pretty a) = Pretty $ T.concat ["first ", a]
  second (Pretty a) = Pretty $ T.concat ["second ", a]