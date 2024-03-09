module Hx where

-- Data.Text Library for Hx and PrettyPrint representation
import Data.Text(Text)
import qualified Data.Text as Tx

import TypesClasses

-- factoring repetition of codes
bOp :: Text -> Text -> Text -> Text
bOp op x y = Tx.concat [Tx.pack "(", x, Tx.pack" ", op, Tx.pack " ", y, Tx.pack ")"]

-- Haskell Representation interpreter
newtype Hx a = Hx 
 { unHx :: Text }

instance ExpSYM Hx where
    int x = Hx $ Tx.pack $ show x
    bool b = Hx $ Tx.pack $ show b
    float f = Hx $ Tx.pack $ show f
    add (Hx x) (Hx y) = Hx
     $ bOp (Tx.pack "+") x y
    neg (Hx x) = Hx
     $ Tx.concat [Tx.pack "(-", x, Tx.pack ")"]
    sub (Hx x) (Hx y) = Hx
     $ bOp (Tx.pack "-") x y
    mul (Hx x) (Hx y) = Hx
     $ bOp (Tx.pack "*") x y
    leq (Hx x) (Hx y) = Hx
     $ bOp (Tx.pack "<=") x y
    lt (Hx x) (Hx y) = Hx
     $ bOp (Tx.pack "<") x y
    neq (Hx x) (Hx y) = Hx
     $ bOp (Tx.pack "/=") x y
    eq (Hx x) (Hx y) = Hx
     $ bOp (Tx.pack "==") x y
    gt (Hx x) (Hx y) = Hx
     $ bOp (Tx.pack ">") x y
    gte (Hx x) (Hx y) = Hx
     $ bOp (Tx.pack ">=") x y
    if_ (Hx b) (Hx x) (Hx y) = Hx
     $ Tx.concat [Tx.pack "if ", b, Tx.pack " then ", x, Tx.pack " else ", y]
    lam f = Hx
     $ Tx.concat [Tx.pack "\\x -> ", unHx (f (Hx $ Tx.pack "x"))]
    app (Hx f) (Hx x) = Hx
     $ Tx.concat [f, Tx.pack " ", x]
    fix f = Hx
     $ Tx.concat [Tx.pack "(fix $ \\x -> ", unHx (f (Hx $ Tx.pack "x")), Tx.pack ")"]
    fst_ (Hx x) = Hx
     $ Tx.concat [Tx.pack "fst ", x]
    snd_ (Hx y) = Hx
     $ Tx.concat [Tx.pack "snd ", y]
    pair (Hx x) (Hx y) = Hx
     $ Tx.concat [Tx.pack "(", x, Tx.pack ", ", y, Tx.pack ")"]