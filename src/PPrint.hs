module PPrint where

-- Data.Text Library for Hx and PrettyPrint representation
import Data.Text (Text)
import qualified Data.Text as Tx


import TypesClasses

bOp :: Text -> Text -> Text -> Text
bOp op x y = Tx.concat [Tx.pack "(", x, Tx.pack" ", op, Tx.pack " ", y, Tx.pack ")"]

--PrettyPrint Representation Interpreter
newtype PPrint a = PPrint 
 { unPPrint :: Text }

instance ExpSYM PPrint where
    int x = PPrint $ Tx.pack $ show x
    bool b = PPrint $ Tx.pack $ show b
    float f = PPrint $ Tx.pack $ show f
    add (PPrint x) (PPrint y) = PPrint
     $ bOp (Tx.pack "+") x y
    neg (PPrint x) = PPrint
     $ Tx.concat [Tx.pack "(-", x, Tx.pack ")"]
    sub (PPrint x) (PPrint y) = PPrint
     $ bOp (Tx.pack "-") x y
    mul (PPrint x) (PPrint y) = PPrint
     $ bOp (Tx.pack "*") x y
    leq (PPrint x) (PPrint y) = PPrint
     $ bOp (Tx.pack "<=") x y
    lt (PPrint x) (PPrint y) = PPrint
     $ bOp (Tx.pack "<") x y
    neq (PPrint x) (PPrint y) = PPrint
     $ bOp (Tx.pack "/=") x y
    eq (PPrint x) (PPrint y) = PPrint
     $ bOp (Tx.pack "==") x y
    gt (PPrint x) (PPrint y) = PPrint
     $ bOp (Tx.pack ">") x y
    gte (PPrint x) (PPrint y) = PPrint
     $ bOp (Tx.pack ">=") x y
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