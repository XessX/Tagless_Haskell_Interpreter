module HxR where

-- Data.Text Library for Hx and PrettyPrint representation
import Data.Text(Text)
import qualified Data.Text as Tx

import TypesClasses

-- Haskell Representation interpreter
newtype HxR a = HxR 
 { unHxR :: Text }

instance ExpSYM HxR where
    int x = HxR $ Tx.pack $ show x
    bool b = HxR $ Tx.pack $ show b
    float f = HxR $ Tx.pack $ show f
    add (HxR x) (HxR y) = HxR 
     $ Tx.concat [Tx.pack "(", x, Tx.pack " + ", y, Tx.pack ")"]
    neg (HxR x) = HxR 
     $ Tx.concat [Tx.pack "(-", x, Tx.pack ")"]
    sub (HxR x) (HxR y) = HxR 
     $ Tx.concat [Tx.pack "(", x, Tx.pack " - ", y, Tx.pack ")"]
    mul (HxR x) (HxR y) = HxR 
     $ Tx.concat [Tx.pack "(", x, Tx.pack " * ", y, Tx.pack ")"]
    leq (HxR x) (HxR y) = HxR 
     $ Tx.concat [Tx.pack "(", x, Tx.pack " <= ", y, Tx.pack ")"]
    lt (HxR x) (HxR y) = HxR 
     $ Tx.concat [Tx.pack "(", x, Tx.pack " < ", y, Tx.pack ")"]
    neq (HxR x) (HxR y) = HxR 
     $ Tx.concat [Tx.pack "(", x, Tx.pack " /= ", y, Tx.pack ")"]
    eq (HxR x) (HxR y) = HxR 
     $ Tx.concat [Tx.pack "(", x, Tx.pack " == ", y, Tx.pack ")"]
    gt (HxR x) (HxR y) = HxR 
     $ Tx.concat [Tx.pack "(", x, Tx.pack " > ", y, Tx.pack ")"]
    gte (HxR x) (HxR y) = HxR 
     $ Tx.concat [Tx.pack "(", x, Tx.pack " >= ", y, Tx.pack ")"]
    if_ (HxR b) (HxR x) (HxR y) = HxR 
     $ Tx.concat [Tx.pack "if ", b, Tx.pack " then ", x, Tx.pack " else ", y]
    lam f = HxR 
     $ Tx.concat [Tx.pack "\\x -> ", unHxR (f (HxR $ Tx.pack "x"))]
    app (HxR f) (HxR x) = HxR 
     $ Tx.concat [f, Tx.pack " ", x]
    fix f = HxR 
     $ Tx.concat [Tx.pack "(fix $ \\x -> ", unHxR (f (HxR $ Tx.pack "x")), Tx.pack ")"]
    fst_ (HxR x) = HxR 
     $ Tx.concat [Tx.pack "fst ", x]
    snd_ (HxR y) = HxR 
     $ Tx.concat [Tx.pack "snd ", y]
    pair (HxR x) (HxR y) = HxR 
     $ Tx.concat [Tx.pack "(", x, Tx.pack ", ", y, Tx.pack ")"]
