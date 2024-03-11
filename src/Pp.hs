module Pp where

import Data.Text(Text)
import Prettyprinter (pretty, parens, (<+>), Doc, layoutPretty, defaultLayoutOptions)
import Prettyprinter.Render.Text (renderStrict)

-- Import the ExpSYM class from your types and classes module
import TypesClasses

newtype Pp a = Pp
 { unPp :: Doc () }

instance ExpSYM Pp where
    int x = Pp $ pretty x
    bool b = Pp $ pretty b
    float f = Pp $ pretty f
    add (Pp x) (Pp y) = Pp
     $ parens (x <+> pretty "+" <+> y)
    neg (Pp x) = Pp
     $ parens (pretty "-" <+> x)
    sub (Pp x) (Pp y) = Pp
     $ parens (x <+> pretty "-" <+> y)
    mul (Pp x) (Pp y) = Pp
     $ parens (x <+> pretty "*" <+> y)
    leql (Pp x) (Pp y) = Pp
     $ parens (x <+> pretty "<=" <+> y)
    lth (Pp x) (Pp y) = Pp
     $ parens (x <+> pretty "<" <+> y)
    neql (Pp x) (Pp y) = Pp
     $ parens (x <+> pretty "/=" <+> y)
    eql (Pp x) (Pp y) = Pp
     $ parens (x <+> pretty "==" <+> y)
    gtr (Pp x) (Pp y) = Pp
     $ parens (x <+> pretty ">" <+> y)
    gtre (Pp x) (Pp y) = Pp
     $ parens (x <+> pretty ">=" <+> y)
    if_el (Pp b) (Pp x) (Pp y) = Pp
     $ pretty "if" <+> b <+> pretty "then" <+> x <+> pretty "else" <+> y
    lam f = let x = Pp $ pretty "x"
                body = unPp $ f x
             in Pp $ parens (pretty "\\" <> pretty "x" <+> pretty "->" <+> body)
    app (Pp f) (Pp x) = Pp $ f <+> x
    fix f = let x = Pp $ pretty "x"
                body = unPp $ f x
            in Pp $ parens (pretty "fix" <+> (pretty "\\" <> pretty "x" <+> pretty "->" <+> body))
    fst_ (Pp x) = Pp
     $ pretty "fst" <+> x
    snd_ (Pp y) = Pp
     $ pretty "snd" <+> y
    pair (Pp x) (Pp y) = Pp
     $ parens (x <> pretty "," <+> y)

-- A sample expression for demonstration, replace with your actual expression
pExpr :: Pp Int
pExpr = mul (int 5) (sub (int 4) (int 5))

-- Function to convert Pp to Text
ppToText :: Pp a -> Text
ppToText = renderStrict . layoutPretty defaultLayoutOptions . unPp
