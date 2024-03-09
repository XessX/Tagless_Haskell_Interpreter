module PPrint where

-- Data.Text Library for Hx and PrettyPrint representation
import Data.Text (Text)
import qualified Data.Text as Tx


import TypesClasses

--Pretty Print Representation using Data.Text
newtype PPrint a = PPrint 
 { unPPrint :: Text }

instance ExpSYM PPrint where
    int x = PPrint $ Tx.pack $ show x
    bool b = PPrint $ Tx.pack $ show b
    float f = PPrint $ Tx.pack $ show f
    add (PPrint x) (PPrint y) = PPrint 
     $ Tx.concat [Tx.pack "(", x, Tx.pack " + ", y, Tx.pack ")"]
    neg (PPrint x) = PPrint 
     $ Tx.concat [Tx.pack "(-", x, Tx.pack ")"]
    sub (PPrint x) (PPrint y) = PPrint 
     $ Tx.concat [Tx.pack "(", x, Tx.pack " - ", y, Tx.pack ")"]
    mul (PPrint x) (PPrint y) = PPrint 
     $ Tx.concat [Tx.pack "(", x, Tx.pack " * ", y, Tx.pack ")"]
    leq (PPrint x) (PPrint y) = PPrint 
     $ Tx.concat [Tx.pack "(", x, Tx.pack " <= ", y, Tx.pack ")"]
    lt (PPrint x) (PPrint y) = PPrint 
     $ Tx.concat [Tx.pack "(", x, Tx.pack " < ", y, Tx.pack ")"]
    neq (PPrint x) (PPrint y) = PPrint 
     $ Tx.concat [Tx.pack "(", x, Tx.pack " /= ", y, Tx.pack ")"]
    eq (PPrint x) (PPrint y) = PPrint 
     $ Tx.concat [Tx.pack "(", x, Tx.pack " == ", y, Tx.pack ")"]
    gt (PPrint x) (PPrint y) = PPrint 
     $ Tx.concat [Tx.pack "(", x, Tx.pack " > ", y, Tx.pack ")"]
    gte (PPrint x) (PPrint y) = PPrint 
     $ Tx.concat [Tx.pack "(", x, Tx.pack " >= ", y, Tx.pack ")"]
    if_ (PPrint b) (PPrint x) (PPrint y) = PPrint 
     $ Tx.concat [Tx.pack "if ", b, Tx.pack " then ", x, Tx.pack " else ", y]
    lam f = PPrint 
     $ Tx.concat [Tx.pack "\\x -> ", unPPrint (f (PPrint $ Tx.pack "x"))]
    app (PPrint f) (PPrint x) = PPrint 
     $ Tx.concat [f, Tx.pack " ", x]
    fix f = PPrint 
     $ Tx.concat [Tx.pack "(fix $ \\x -> ", unPPrint (f (PPrint $ Tx.pack "x")), Tx.pack ")"]
    fst_ (PPrint x) = PPrint 
     $ Tx.concat [Tx.pack "fst ", x]
    snd_ (PPrint y) = PPrint 
     $ Tx.concat [Tx.pack "snd ", y]
    pair (PPrint x) (PPrint y) = PPrint 
     $ Tx.concat [Tx.pack "(", x, Tx.pack ", ", y, Tx.pack ")"]
